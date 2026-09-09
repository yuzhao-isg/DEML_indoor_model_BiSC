#-----------------------------------------------------------------------------#
#                             Table 2: CV results                             #
#-----------------------------------------------------------------------------#
# Description:
# This script summarizes cross-validation performance metrics for the DEML
# indoor model across spatial, short-term, and long-term validation settings.
# It reads model output tables, labels observations by season, computes
# regression-quality metrics (R², RMSE, MAD, intercept, slope), and organizes the
# results into a tidy table for reporting purposes.
#
# Outputs:
# - summary tables in the R session (used for downstream reporting)
#
# Workflow:
# 1. Define project paths relative to the script location.
# 2. Add season labels based on month-day.
# 3. Compute evaluation metrics for all-season, warm-season, and cool-season data.
# 4. Repeat this for temperature and humidity under each validation scenario.
#-----------------------------------------------------------------------------#

rm(list = ls())

library(tidyverse)
library(lubridate)
library(xtable)

#-----------------------------------------------------------------------------#
#                               Setup paths                                    #
#-----------------------------------------------------------------------------#
script_dir <- if (!is.null(sys.frames()[[1]]$ofile)) {
  dirname(normalizePath(sys.frames()[[1]]$ofile))
} else {
  getwd()
}

project_root <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = FALSE)
input_dir <- file.path(project_root, "results", "Model")

#-----------------------------------------------------------------------------#
#                             Helper functions                                #
#-----------------------------------------------------------------------------#
add_season <- function(df) {
  df %>%
    mutate(
      month_day = format(date, "%m-%d"),
      season_2 = case_when(
        month_day >= "05-15" & month_day <= "10-15" ~ "Warm season",
        TRUE ~ "Cool season"
      )
    )
}

summarize_model_performance <- function(data, predicted_col, outcome_col) {
  data <- data %>%
    mutate(
      predicted = .data[[predicted_col]],
      outcome = .data[[outcome_col]]
    )

  groups <- list(
    "all season" = data,
    "Warm season" = data %>% filter(season_2 == "Warm season"),
    "Cool season" = data %>% filter(season_2 == "Cool season")
  )

  purrr::map_dfr(names(groups), function(group_name) {
    df <- groups[[group_name]]
    model <- lm(outcome ~ predicted, data = df)

    tibble(
      group = group_name,
      RMSE = sqrt(mean((df$outcome - df$predicted)^2, na.rm = TRUE)),
      MAD = median(abs(df$outcome - df$predicted), na.rm = TRUE),
      R2 = summary(model)$r.squared,
      R2_adjust = summary(model)$adj.r.squared,
      intercept = coef(model)[1],
      slope = coef(model)[2]
    )
  }) %>%
    column_to_rownames("group")
}

read_validation_data <- function(file_path) {
  read_csv(file_path) %>%
    add_season()
}

#-----------------------------------------------------------------------------#
#                       Spatial validation performance                        #
#-----------------------------------------------------------------------------#
spatial_temp_data <- read_validation_data(
  file.path(input_dir, "Spacial_Validation_seed1225", "Temperature", "temp_perf_DEML_spacial.csv")
)
spatial_hr_data <- read_validation_data(
  file.path(input_dir, "Spacial_Validation_seed1225", "Humidity", "hr_perf_DEML_spacial.csv")
)

deml_Performance_temp <- summarize_model_performance(spatial_temp_data, "temp_deml_test", "indoor_temp")
rf_Performance_temp <- summarize_model_performance(spatial_temp_data, "temp_rf_pred", "indoor_temp")
xgb_Performance_temp <- summarize_model_performance(spatial_temp_data, "temp_xgb_pred", "indoor_temp")
gbm_Performance_temp <- summarize_model_performance(spatial_temp_data, "temp_gbm_pred", "indoor_temp")

deml_Performance_hr <- summarize_model_performance(spatial_hr_data, "hr_deml_test", "indoor_hr")
rf_Performance_hr <- summarize_model_performance(spatial_hr_data, "hr_rf_pred", "indoor_hr")
xgb_Performance_hr <- summarize_model_performance(spatial_hr_data, "hr_xgb_pred", "indoor_hr")
gbm_Performance_hr <- summarize_model_performance(spatial_hr_data, "hr_gbm_pred", "indoor_hr")

#-----------------------------------------------------------------------------#
#                  Short-term temporal validation performance                 #
#-----------------------------------------------------------------------------#
short_term_temp_data <- read_validation_data(
  file.path(input_dir, "Temporal_Validation_daily", "Temperature", "temp_perf_DEML_random_day.csv")
)
short_term_hr_data <- read_validation_data(
  file.path(input_dir, "Temporal_Validation_daily", "Humidity", "hr_perf_DEML_random_day.csv")
)

deml_Performance_temp_short <- summarize_model_performance(short_term_temp_data, "temp_deml_test", "indoor_temp")
rf_Performance_temp_short <- summarize_model_performance(short_term_temp_data, "temp_rf_pred", "indoor_temp")
xgb_Performance_temp_short <- summarize_model_performance(short_term_temp_data, "temp_xgb_pred", "indoor_temp")
gbm_Performance_temp_short <- summarize_model_performance(short_term_temp_data, "temp_gbm_pred", "indoor_temp")

deml_Performance_hr_short <- summarize_model_performance(short_term_hr_data, "hr_deml_test", "indoor_hr")
rf_Performance_hr_short <- summarize_model_performance(short_term_hr_data, "hr_rf_pred", "indoor_hr")
xgb_Performance_hr_short <- summarize_model_performance(short_term_hr_data, "hr_xgb_pred", "indoor_hr")
gbm_Performance_hr_short <- summarize_model_performance(short_term_hr_data, "hr_gbm_pred", "indoor_hr")

#-----------------------------------------------------------------------------#
#                   Long-term temporal validation performance                 #
#-----------------------------------------------------------------------------#
long_term_temp_data <- read_validation_data(
  file.path(input_dir, "Temporal_Validation_weekly_40", "Temperature", "temp_perf_DEML_weekly.csv")
)
long_term_hr_data <- read_validation_data(
  file.path(input_dir, "Temporal_Validation_weekly_40", "Humidity", "hr_perf_DEML_weekly.csv")
)

deml_Performance_temp_long <- summarize_model_performance(long_term_temp_data, "temp_deml_test", "indoor_temp")
rf_Performance_temp_long <- summarize_model_performance(long_term_temp_data, "temp_rf_pred", "indoor_temp")
xgb_Performance_temp_long <- summarize_model_performance(long_term_temp_data, "temp_xgb_pred", "indoor_temp")
gbm_Performance_temp_long <- summarize_model_performance(long_term_temp_data, "temp_gbm_pred", "indoor_temp")

deml_Performance_hr_long <- summarize_model_performance(long_term_hr_data, "hr_deml_test", "indoor_hr")
rf_Performance_hr_long <- summarize_model_performance(long_term_hr_data, "hr_rf_pred", "indoor_hr")
xgb_Performance_hr_long <- summarize_model_performance(long_term_hr_data, "hr_xgb_pred", "indoor_hr")
gbm_Performance_hr_long <- summarize_model_performance(long_term_hr_data, "hr_gbm_pred", "indoor_hr")

###################################################################################
# END 