#-----------------------------------------------------------------------------#
#                          CV scatterplots figure - DEML                      #
#-----------------------------------------------------------------------------#
# Description:
# This script generates seasonal scatterplot figures for cross-validation results
# of the DEML indoor environment model. It reads model performance tables for
# temperature and humidity under both long-term and short-term validation
# scenarios, separates observations into warm and cold seasons, and saves a 2x2
# grid of hexbin comparison plots as PNG files.
#
# Outputs:
# - results/Figure/CV/season_long_term_1.png
# - results/Figure/CV/season_short_term_1.png
#
# Workflow:
# 1. Define project paths relative to the script location.
# 2. Add a season label based on month-day.
# 3. Build hexbin plots comparing measured vs predicted values.
# 4. Add model quality metrics (R², RMSE, MAD) to each panel.
# 5. Arrange plots into a combined figure and save as PNG.
#-----------------------------------------------------------------------------#

rm(list = ls())

library(tidyverse)
library(colorspace)
library(gridExtra)

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
output_dir <- file.path(project_root, "results", "Figure", "CV")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

#-----------------------------------------------------------------------------#
#                              Helper functions                                #
#-----------------------------------------------------------------------------#
add_season <- function(df) {
  df %>%
    mutate(
      month_day = format(date, "%m-%d"),
      season = case_when(
        month_day >= "05-15" & month_day <= "10-15" ~ "Warm season",
        TRUE ~ "Cold season"
      )
    )
}

make_season_plot <- function(df, x_var, y_var, season_name, palette, title, x_lab, y_lab, metric_text) {
  df %>%
    filter(season == season_name) %>%
    ggplot(aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_hex() +
    geom_abline(intercept = 0, slope = 1) +
    scale_fill_continuous_sequential(palette = palette) +
    coord_equal() +
    theme_bw() +
    labs(fill = "Count") +
    ggtitle(title) +
    xlab(x_lab) +
    ylab(y_lab) +
    theme(
      aspect.ratio = 1,
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    ) +
    annotate(
      "text",
      x = Inf,
      y = Inf,
      label = metric_text,
      hjust = 1.05,
      vjust = 1.05,
      size = 4,
      color = "black",
      fontface = "bold"
    )
}

#-----------------------------------------------------------------------------#
#                            Long-term validation                              #
#-----------------------------------------------------------------------------#
long_term_temp <- add_season(read_csv(file.path(input_dir, "Temporal_Validation_weekly_40", "Temperature", "temp_perf_DEML_weekly.csv")))
long_term_hr <- add_season(read_csv(file.path(input_dir, "Temporal_Validation_weekly_40", "Humidity", "hr_perf_DEML_weekly.csv")))

warm_long_temp <- make_season_plot(
  long_term_temp,
  x_var = "temp_deml_test",
  y_var = "indoor_temp",
  season_name = "Warm season",
  palette = "Purple-Orange",
  title = "Warm season - Temperature",
  x_lab = "Predicted indoor temperature (ºC)",
  y_lab = "Measured indoor temperature (ºC)",
  metric_text = "R² = 0.761\nRMSE = 1.191 ºC\nMAD = 0.675 ºC"
)

cold_long_temp <- make_season_plot(
  long_term_temp,
  x_var = "temp_deml_test",
  y_var = "indoor_temp",
  season_name = "Cold season",
  palette = "Purple-Blue",
  title = "Cold season - Temperature",
  x_lab = "Predicted indoor temperature (ºC)",
  y_lab = "Measured indoor temperature (ºC)",
  metric_text = "R² = 0.553\nRMSE = 1.307 ºC\nMAD = 0.742 ºC"
)

warm_long_hr <- make_season_plot(
  long_term_hr,
  x_var = "hr_deml_test",
  y_var = "indoor_hr",
  season_name = "Warm season",
  palette = "Oranges",
  title = "Warm season - Humidity",
  x_lab = "Predicted indoor humidity (%)",
  y_lab = "Measured indoor humidity (%)",
  metric_text = "R² = 0.509\nRMSE = 5.223 %\nMAD = 2.812 %"
)

cold_long_hr <- make_season_plot(
  long_term_hr,
  x_var = "hr_deml_test",
  y_var = "indoor_hr",
  season_name = "Cold season",
  palette = "Mint",
  title = "Cold season - Humidity",
  x_lab = "Predicted indoor humidity (%)",
  y_lab = "Measured indoor humidity (%)",
  metric_text = "R² = 0.482\nRMSE = 5.962 %\nMAD = 3.814 %"
)

long_term_grid <- grid.arrange(warm_long_temp, cold_long_temp, warm_long_hr, cold_long_hr, ncol = 2)

ggsave(
  file.path(output_dir, "season_long_term_1.png"),
  plot = long_term_grid,
  width = 12,
  height = 8,
  units = "in",
  dpi = 600
)

#-----------------------------------------------------------------------------#
#                           Short-term validation                              #
#-----------------------------------------------------------------------------#
short_term_temp <- add_season(read_csv(file.path(input_dir, "Temporal_Validation_daily", "Temperature", "temp_perf_DEML_random_day.csv")))
short_term_hr <- add_season(read_csv(file.path(input_dir, "Temporal_Validation_daily", "Humidity", "hr_perf_DEML_random_day.csv")))

warm_short_temp <- make_season_plot(
  short_term_temp,
  x_var = "temp_deml_test",
  y_var = "indoor_temp",
  season_name = "Warm season",
  palette = "Purple-Orange",
  title = "Warm season - Temperature",
  x_lab = "Predicted indoor temperature (ºC)",
  y_lab = "Measured indoor temperature (ºC)",
  metric_text = "R² = 0.954\nRMSE = 0.521 ºC\nMAD = 0.285 ºC"
)

cold_short_temp <- make_season_plot(
  short_term_temp,
  x_var = "temp_deml_test",
  y_var = "indoor_temp",
  season_name = "Cold season",
  palette = "Purple-Blue",
  title = "Cold season - Temperature",
  x_lab = "Predicted indoor temperature (ºC)",
  y_lab = "Measured indoor temperature (ºC)",
  metric_text = "R² = 0.915\nRMSE = 0.580 ºC\nMAD = 0.334 ºC"
)

warm_short_hr <- make_season_plot(
  short_term_hr,
  x_var = "hr_deml_test",
  y_var = "indoor_hr",
  season_name = "Warm season",
  palette = "Oranges",
  title = "Warm season - Humidity",
  x_lab = "Predicted indoor humidity (%)",
  y_lab = "Measured indoor humidity (%)",
  metric_text = "R² = 0.887\nRMSE = 2.293 %\nMAD = 1.391 %"
)

cold_short_hr <- make_season_plot(
  short_term_hr,
  x_var = "hr_deml_test",
  y_var = "indoor_hr",
  season_name = "Cold season",
  palette = "Mint",
  title = "Cold season - Humidity",
  x_lab = "Predicted indoor humidity (%)",
  y_lab = "Measured indoor humidity (%)",
  metric_text = "R² = 0.894\nRMSE = 2.877 %\nMAD = 1.855 %"
)

short_term_grid <- grid.arrange(warm_short_temp, cold_short_temp, warm_short_hr, cold_short_hr, ncol = 2)

ggsave(
  file.path(output_dir, "season_short_term_1.png"),
  plot = short_term_grid,
  width = 12,
  height = 8,
  units = "in",
  dpi = 600
)

########################################################################################
# END                                                                         