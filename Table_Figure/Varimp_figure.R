#-----------------------------------------------------------------------------#
#                           importance of predictor                           #
#-----------------------------------------------------------------------------#
# Description:
# This script summarizes variable importance across temperature and humidity
# models and generates figure panels for long-term and short-term validation.
#
# Outputs:
# - results/Figure/importance/*.png
# - results/Figure/*.png

rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidytext)

script_dir <- if (!is.null(sys.frames()[[1]]$ofile)) {
  dirname(normalizePath(sys.frames()[[1]]$ofile))
} else {
  getwd()
}
project_root <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = FALSE)
model_dir <- file.path(project_root, "results", "Model")
figure_dir <- file.path(project_root, "results", "Figure")
if (!dir.exists(figure_dir)) dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

#-----------------------------------------------------------------------------#
#                             Long-term validation                            #
#-----------------------------------------------------------------------------#

#==================          temperature         ========================# 

importance_rf_temp <- read.csv(file.path(model_dir, "Temporal_Validation_weekly_40", "table_figure", "Temperature", "varimp_DEML_train_rf_weekly.csv")) %>%
  mutate(Model = "RF") %>%
  rename(Importance = Importance_rf)

importance_xgb_temp <- read.csv(file.path(model_dir, "Temporal_Validation_weekly_40", "table_figure", "Temperature", "varimp_DEML_train_xgb_weekly.csv")) %>%
  mutate(Model = "XGBoost") %>%
  rename(Importance = Importance_xgb)

importance_gbm_temp <- read.csv(file.path(model_dir, "Temporal_Validation_weekly_40", "table_figure", "Temperature", "varimp_DEML_train_gbm_weekly.csv")) %>%
  mutate(Model = "GBM") %>%
  rename(Importance = Importance_gbm)


#==================          humidity         ========================# 

importance_rf_hr <- read.csv(file.path(model_dir, "Temporal_Validation_weekly_40", "table_figure", "Humidity", "varimp_DEML_train_rf_weekly.csv")) %>%
  mutate(Model = "RF") %>%
  rename(Importance = Importance_rf)

importance_xgb_hr <- read.csv(file.path(model_dir, "Temporal_Validation_weekly_40", "table_figure", "Humidity", "varimp_DEML_train_xgb_weekly.csv")) %>%
  mutate(Model = "XGBoost") %>%
  rename(Importance = Importance_xgb)

importance_gbm_hr <- read.csv(file.path(model_dir, "Temporal_Validation_weekly_40", "table_figure", "Humidity", "varimp_DEML_train_gbm_weekly.csv")) %>%
  mutate(Model = "GBM") %>%
  rename(Importance = Importance_gbm)


#################################################################################
#==================          temperature         ========================# 
#################################################################################

# Calculate the weighted importance score for factor variables
trainset_temp <- readRDS(file.path(model_dir, "Temporal_Validation_weekly_40", "train_dummy_dataset.rds"))

# First step: calculate the frequency of each dummy varaiables 

cat_vars <- c("difficult_pay_heating", "ct1", "ct6_m01", "ct7_m01",
              "ct8ec", "ct8k", "ct8g", "ct8h","ct8i","ct8j","heating_system_bedroom", 
              "heating_system_livingroom", "EC_2_32w","GS3_m01","N6_m01","N6_m03",
              "cooling_system_day","cooling_system_night","month")
dummy_freq_temp <- data.frame(dummy_var = character(), frequency = numeric())
for (var in cat_vars) {
  # find the var start variable
  dummy_names <- grep(paste0("^", var, "\\."), colnames(trainset_temp), value = TRUE)
  
  # calculate the frequency
  for (dummy in dummy_names) {
    freq <- mean(trainset_temp[[dummy]], na.rm = TRUE)
    dummy_freq_temp <- rbind(dummy_freq_temp, data.frame(Variable = dummy, frequency = freq))
  }
}

# Second step: calculate weighted importance
importance_list_temp <- list(
  RF = importance_rf_temp, 
  XGBoost = importance_xgb_temp, 
  GBM = importance_gbm_temp
)

calc_weighted_importance <- function(importance_df, freq_df, category_prefix) {
  # Find the dummy variables related to this categorical variable in importance_df
  dummy_names <- grep(paste0("^", category_prefix, "\\."), importance_df$Variable, value = TRUE)
  
  # Extract "importance" and its corresponding frequency
  imp_sub <- importance_df %>%
    filter(Variable %in% dummy_names) %>%
    select(Variable, Importance)
  
  # combine with frequency database
  imp_freq <- imp_sub %>%
    left_join(freq_df, by = "Variable") %>%
    filter(!is.na(frequency)) 
  # calculate weighted importance
  weighted_imp <- sum(imp_freq$Importance * imp_freq$frequency) / sum(imp_freq$frequency)
  
  return(weighted_imp)
}

weighted_importance_temp <- sapply(cat_vars, function(cat_var) {
  sapply(importance_list_temp, function(df) {
    calc_weighted_importance(df, dummy_freq_temp, cat_var)
  })
})

weighted_importance_temp <- as.data.frame(weighted_importance_temp)

# convert the database form keep consistent with previous one
weighted_importance_temp <- weighted_importance_temp %>%
  tibble::rownames_to_column(var = "Model")

importance_long_temp <- weighted_importance_temp %>%
  pivot_longer(
    cols = -Model,
    names_to = "Variable",
    values_to = "Importance"
  )

# Third step: Combine with previous variable
Varimp_temp <- bind_rows(importance_rf_temp, importance_xgb_temp, importance_gbm_temp)

# Remove dummy variables
importance_no_dummies_temp <- Varimp_temp %>%
  filter(!str_detect(Variable, paste0("^(", paste(cat_vars, collapse = "|"), ")\\.")))

# combine these two data frame
importance_new_temp <- bind_rows(importance_no_dummies_temp, importance_long_temp)
importance_new_temp$Model <- factor(importance_new_temp$Model, levels = c("RF", "XGBoost", "GBM"))

#################################################################################

# Select top 20 most important variables for each model
Varimp_top20_temp <- importance_new_temp %>%
  group_by(Model) %>%
  slice_max(order_by = Importance, n = 20) %>%
  ungroup()

Varimp_top20_temp <- Varimp_top20_temp %>%
  mutate(Variable = case_when(
    Variable == "temperature_mean_home" ~ "Outdoor_Temperature_Mean",
    Variable == "temp_mean_lag1_avg" ~ "Outdoor_Temperature_lag1",
    Variable == "month" ~ "Month", 
    Variable == "year" ~ "Year", 
    Variable == "yday" ~ "Day of Year", 
    Variable == "temperature_min_home" ~ "Outdoor_Temperature_Min",
    Variable == "temperature_max_home" ~ "Outdoor_Temperature_Max",
    Variable == "hum_mean_lag1_avg" ~ "Outdoor_Humidity_lag1",
    Variable == "humidity_mean_home" ~ "Outdoor_Humidity_Mean",
    Variable == "h5" ~ "Number_People",
    Variable == "Energia_calefacció_demanda_300m" ~ "Energy_Demand_Heating",
    Variable == "ct3_8a" ~ "Floor_Bedroom", 
    Variable == "cooling_system_day" ~ "Usage_Cooling_System_Day",
    Variable == "cooling_system_night" ~ "Usage_Cooling_System_Night",
    Variable == "heating_system_livingroom" ~ "Heating_System_Livingroom",
    Variable == "heating_system_bedroom" ~ "Heating_System_Bedroom",
    Variable == "ndvi_300" ~ "NDVI_300m", Variable == "ct8m" ~ "Volume_Bedroom",
    Variable == "maternal_age" ~ "Maternal_Age",
    Variable == "ct8l_f" ~ "Area_Window",
    Variable == "Energia_refrigeració_demanda_300m" ~ "Energy_Demand_Cooling",
    Variable == "sr" ~ "Solar_Radiation",
    Variable == "VALOR_AILLAMENTS_300m" ~ "Transmittance_Facade",
    Variable == "VALOR_FINESTRES_300m" ~ "Transmittance_Window",
    Variable == "Average_building" ~ "Construction_Year", 
    Variable == "r6_5" ~ "Window_Open_Kitchen_Day",
    Variable == "apavg" ~ "Atmospheric_Pressure",
    Variable == "rhavg" ~ "Relative_Humidity",
    Variable == "r6_2" ~ "Window_Open_Baby_Bedroom_Day",
    Variable == "r6_1" ~ "Window_Open_Parent_Bedroom_Day",
    Variable == "r6_3" ~ "Window_Open_Other_Bedroom_Day",
    Variable == "wind_direction_10m" ~ "Wind_Direction",
    Variable == "r6_4" ~ "Window_Open_Livingroom_Day",
    Variable == "n7_1a" ~ "Window_Open_Parent_Bedroom_Night",
    Variable == "n7_1b" ~ "Window_Open_Parent_Bedroom_Night_Half",
    Variable == "n7_3b" ~ "Window_Open_Parent_Livingroom_Night_Half",
    Variable == "n7_3a" ~ "Window_Open_Parent_Livingroom_Night",
    Variable == "N6_m03" ~ "Frequency_Close_Window",
    TRUE ~ Variable  # Keep other names unchanged
  ))


ggplot(Varimp_top20_temp, aes(x = Importance, 
                              y = fct_reorder(Variable, Importance), 
                              fill = Model)) +
  geom_col(show.legend = FALSE, width = 0.8, alpha = 0.85) +
  facet_wrap(~ Model, scales = "free_x", nrow = 1) +  
  scale_fill_manual(values = c("#1f77b4", "#6A0DAD", "#2E8B57")) +
  labs(
    x = "Variable Importance",
    y = NULL,  
    title = "Top 20 Variable Importance for Temperature Prediction"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    strip.text = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 12, lineheight = 0.8), 
    axis.text.x = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.spacing = unit(1.5, "lines"),  
    plot.margin = margin(1, 1, 1, 2, "cm")  
  )
# ggplot(Varimp_top20_temp, aes(x = reorder_within(Variable, Importance, Model), 
#                               y = Importance, 
#                               fill = Importance)) + 
#   geom_col(show.legend = TRUE, width = 0.7) +
#   coord_flip() +
#   facet_wrap(~ Model, scales = "free_y", ncol = 1, strip.position = "right") +
#   scale_fill_viridis_c(option = "plasma", direction = -1, name = "Importance") + 
#   scale_x_reordered() +
#   labs(
#     x = NULL,
#     y = "Variable Importance",
#     title = "Top 20 Variable Importance - Temperature"
#   ) +
#   theme_minimal(base_size = 11) +  
#   theme(
#     strip.text = element_text(face = "bold", size = 10),
#     axis.text.y = element_text(size = 8),  
#     axis.text.x = element_text(size = 8),
#     legend.position = "bottom",  
#     legend.key.size = unit(0.4, "cm"),
#     panel.grid.major.y = element_blank(),
#     panel.spacing = unit(0.8, "lines")
#   )


ggsave(file.path(figure_dir, "importance", "Var_importance_Across_models_temp.png"), 
       width = 12, height = 8, 
       dpi = 600)



#################################################################################
#==================          humidity         ========================# 
#################################################################################

# Calculate the weighted importance score for factor variables
trainset_hr <- readRDS(file.path(model_dir, "Temporal_Validation_weekly_40", "train_dummy_dataset.rds"))

# First step: calculate the frequency of each dummy varaiables 
cat_vars <- c("difficult_pay_heating", "ct1", "ct6_m01", "ct7_m01",
              "ct8ec", "ct8k", "ct8g", "ct8h","ct8i","ct8j","heating_system_bedroom", 
              "heating_system_livingroom", "EC_2_32w","GS3_m01","N6_m01","N6_m03",
              "cooling_system_day","cooling_system_night","month")

dummy_freq_hr <- data.frame(dummy_var = character(), frequency = numeric())

for (var in cat_vars) {
  # find the var start variable
  dummy_names <- grep(paste0("^", var, "\\."), colnames(trainset_hr), value = TRUE)
  
  # calculate the frequency
  for (dummy in dummy_names) {
    freq <- mean(trainset_hr[[dummy]], na.rm = TRUE)
    dummy_freq_hr <- rbind(dummy_freq_hr, data.frame(Variable = dummy, frequency = freq))
  }
}

# Second step: calculate weighted improtance
importance_list_hr <- list(
  RF = importance_rf_hr, 
  XGBoost = importance_xgb_hr, 
  GBM = importance_gbm_hr
)

calc_weighted_importance <- function(importance_df, freq_df, category_prefix) {
  # Find the dummy variables related to this categorical variable in importance_df
  dummy_names <- grep(paste0("^", category_prefix, "\\."), importance_df$Variable, value = TRUE)
  
  # Extract "importance" and its corresponding frequency
  imp_sub <- importance_df %>%
    filter(Variable %in% dummy_names) %>%
    select(Variable, Importance)
  
  # combine with frequency database
  imp_freq <- imp_sub %>%
    left_join(freq_df, by = "Variable") %>%
    filter(!is.na(frequency)) 
  # calculate weighted importance
  weighted_imp <- sum(imp_freq$Importance * imp_freq$frequency) / sum(imp_freq$frequency)
  
  return(weighted_imp)
}

weighted_importance_hr <- sapply(cat_vars, function(cat_var) {
  sapply(importance_list_hr, function(df) {
    calc_weighted_importance(df, dummy_freq_hr, cat_var)
  })
})

weighted_importance_hr <- as.data.frame(weighted_importance_hr)

# convert the database form keep consistent with previous one
weighted_importance_hr <- weighted_importance_hr %>%
  tibble::rownames_to_column(var = "Model")

importance_long_hr <- weighted_importance_hr %>%
  pivot_longer(
    cols = -Model,
    names_to = "Variable",
    values_to = "Importance"
  )

# Third step: Combine with previous variable
Varimp_hr <- bind_rows(importance_rf_hr, importance_xgb_hr, importance_gbm_hr)

# Remove dummy variables
importance_no_dummies_hr <- Varimp_hr %>%
  filter(!str_detect(Variable, paste0("^(", paste(cat_vars, collapse = "|"), ")\\.")))

# combine these two data frame
importance_new_hr <- bind_rows(importance_no_dummies_hr, importance_long_hr)
importance_new_hr$Model <- factor(importance_new_hr$Model, levels = c("RF", "XGBoost", "GBM"))

#################################################################################

# Select top 20 most important variables for each model
Varimp_top20_hr <- importance_new_hr %>%
  group_by(Model) %>%
  slice_max(order_by = Importance, n = 20) %>%
  ungroup()

Varimp_top20_hr <- Varimp_top20_hr %>%
  mutate(Variable = case_when(
    Variable == "temperature_mean_home" ~ "Outdoor_Temperature_Mean",
    Variable == "temp_mean_lag1_avg" ~ "Outdoor_Temperature_lag1",
    Variable == "month" ~ "Month", 
    Variable == "year" ~ "Year", 
    Variable == "yday" ~ "Day of Year", 
    Variable == "temperature_min_home" ~ "Outdoor_Temperature_Min",
    Variable == "temperature_max_home" ~ "Outdoor_Temperature_Max",
    Variable == "hum_mean_lag1_avg" ~ "Outdoor_Humidity_lag1",
    Variable == "humidity_mean_home" ~ "Outdoor_Humidity_Mean",
    Variable == "h5" ~ "Number_People",
    Variable == "Energia_calefacció_demanda_300m" ~ "Energy_Demand_Heating",
    Variable == "ct3_8a" ~ "Floor_Bedroom", 
    Variable == "cooling_system_day" ~ "Usage_Cooling_System_Day",
    Variable == "cooling_system_night" ~ "Usage_Cooling_System_Night",
    Variable == "heating_system_livingroom" ~ "Heating_System_Livingroom",
    Variable == "heating_system_bedroom" ~ "Heating_System_Bedroom",
    Variable == "ndvi_300" ~ "NDVI_300m", Variable == "ct8m" ~ "Volume_Bedroom",
    Variable == "maternal_age" ~ "Maternal_Age",
    Variable == "ct8l_f" ~ "Area_Window",
    Variable == "Energia_refrigeració_demanda_300m" ~ "Energy_Demand_Cooling",
    Variable == "sr" ~ "Solar_Radiation",
    Variable == "VALOR_AILLAMENTS_300m" ~ "Transmittance_Facade",
    Variable == "VALOR_FINESTRES_300m" ~ "Transmittance_Window",
    Variable == "Average_building" ~ "Construction_Year", 
    Variable == "r6_5" ~ "Window_Open_Kitchen_Day",
    Variable == "apavg" ~ "Atmospheric_Pressure",
    Variable == "rhavg" ~ "Relative_Humidity",
    Variable == "r6_2" ~ "Window_Open_Baby_Bedroom_Day",
    Variable == "r6_1" ~ "Window_Open_Parent_Bedroom_Day",
    Variable == "r6_3" ~ "Window_Open_Other_Bedroom_Day",
    Variable == "wind_direction_10m" ~ "Wind_Direction",
    Variable == "r6_4" ~ "Window_Open_Livingroom_Day",
    Variable == "n7_1a" ~ "Window_Open_Parent_Bedroom_Night",
    Variable == "n7_1b" ~ "Window_Open_Parent_Bedroom_Night_Half",
    Variable == "n7_3b" ~ "Window_Open_Parent_Livingroom_Night_Half",
    Variable == "n7_3a" ~ "Window_Open_Parent_Livingroom_Night",
    Variable == "N6_m03" ~ "Frequency_Close_Window",
    Variable == "indoor_temp" ~ "Indoor_Temperature",
    TRUE ~ Variable  # Keep other names unchanged
  ))

# Plot variable importance

ggplot(Varimp_top20_hr, aes(x = Importance, 
                              y = fct_reorder(Variable, Importance), 
                              fill = Model)) +
  geom_col(show.legend = FALSE, width = 0.8, alpha = 0.85) +
  facet_wrap(~ Model, scales = "free_x", nrow = 1) +  
  scale_fill_manual(values = c("#1f77b4", "#6A0DAD", "#2E8B57")) +
  labs(
    x = "Variable Importance",
    y = NULL,  
    title = "Top 20 Variable Importance for Humidity Prediction"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    strip.text = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 12, lineheight = 0.8), 
    axis.text.x = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.spacing = unit(1.5, "lines"),  
    plot.margin = margin(1, 1, 1, 2, "cm")  
  )

ggsave(file.path(figure_dir, "importance", "Var_importance_Across_models_hr.png"), 
       width = 12, height = 8, 
       dpi = 600)

##################################################################################






#-----------------------------------------------------------------------------#
#                            Short-term validation                            #
#-----------------------------------------------------------------------------#

#==================          temperature         ========================# 

importance_rf_temp <- read.csv(file.path(model_dir, "Temporal_Validation_daily", "table_figure", "Temperature", "varimp_DEML_train_rf_random_day.csv")) %>%
  mutate(Model = "RF") %>%
  rename(Importance = Importance_rf)

importance_xgb_temp <- read.csv(file.path(model_dir, "Temporal_Validation_daily", "table_figure", "Temperature", "varimp_DEML_train_xgb_random_day.csv")) %>%
  mutate(Model = "XGBoost") %>%
  rename(Importance = Importance_xgb)

importance_gbm_temp <- read.csv(file.path(model_dir, "Temporal_Validation_daily", "table_figure", "Temperature", "varimp_DEML_train_gbm_random_day.csv")) %>%
  mutate(Model = "GBM") %>%
  rename(Importance = Importance_gbm)


#==================          humidity         ========================# 

importance_rf_hr <- read.csv(file.path(model_dir, "Temporal_Validation_daily", "table_figure", "Humidity", "varimp_DEML_train_rf_random_day.csv")) %>%
  mutate(Model = "RF") %>%
  rename(Importance = Importance_rf)

importance_xgb_hr <- read.csv(file.path(model_dir, "Temporal_Validation_daily", "table_figure", "Humidity", "varimp_DEML_train_xgb_random_day.csv")) %>%
  mutate(Model = "XGBoost") %>%
  rename(Importance = Importance_xgb)

importance_gbm_hr <- read.csv(file.path(model_dir, "Temporal_Validation_daily", "table_figure", "Humidity", "varimp_DEML_train_gbm_random_day.csv")) %>%
  mutate(Model = "GBM") %>%
  rename(Importance = Importance_gbm)


#################################################################################
#==================          temperature         ========================# 
#################################################################################

# Calculate the weighted importance score for factor variables
trainset_temp <- readRDS(file.path(model_dir, "Temporal_Validation_daily", "train_dummy_dataset.rds"))

# First step: calculate the frequency of each dummy varaiables 

cat_vars <- c("difficult_pay_heating", "ct1", "ct6_m01", "ct7_m01",
              "ct8ec", "ct8k", "ct8g", "ct8h","ct8i","ct8j","heating_system_bedroom", 
              "heating_system_livingroom", "EC_2_32w","GS3_m01","N6_m01","N6_m03",
              "cooling_system_day","cooling_system_night","month")
dummy_freq_temp <- data.frame(dummy_var = character(), frequency = numeric())
for (var in cat_vars) {
  # find the var start variable
  dummy_names <- grep(paste0("^", var, "\\."), colnames(trainset_temp), value = TRUE)
  
  # calculate the frequency
  for (dummy in dummy_names) {
    freq <- mean(trainset_temp[[dummy]], na.rm = TRUE)
    dummy_freq_temp <- rbind(dummy_freq_temp, data.frame(Variable = dummy, frequency = freq))
  }
}

# Second step: calculate weighted importance
importance_list_temp <- list(
  RF = importance_rf_temp, 
  XGBoost = importance_xgb_temp, 
  GBM = importance_gbm_temp
)

calc_weighted_importance <- function(importance_df, freq_df, category_prefix) {
  # Find the dummy variables related to this categorical variable in importance_df
  dummy_names <- grep(paste0("^", category_prefix, "\\."), importance_df$Variable, value = TRUE)
  
  # Extract "importance" and its corresponding frequency
  imp_sub <- importance_df %>%
    filter(Variable %in% dummy_names) %>%
    select(Variable, Importance)
  
  # combine with frequency database
  imp_freq <- imp_sub %>%
    left_join(freq_df, by = "Variable") %>%
    filter(!is.na(frequency)) 
  # calculate weighted importance
  weighted_imp <- sum(imp_freq$Importance * imp_freq$frequency) / sum(imp_freq$frequency)
  
  return(weighted_imp)
}

weighted_importance_temp <- sapply(cat_vars, function(cat_var) {
  sapply(importance_list_temp, function(df) {
    calc_weighted_importance(df, dummy_freq_temp, cat_var)
  })
})

weighted_importance_temp <- as.data.frame(weighted_importance_temp)

# convert the database form keep consistent with previous one
weighted_importance_temp <- weighted_importance_temp %>%
  tibble::rownames_to_column(var = "Model")

importance_long_temp <- weighted_importance_temp %>%
  pivot_longer(
    cols = -Model,
    names_to = "Variable",
    values_to = "Importance"
  )

# Third step: Combine with previous variable
Varimp_temp <- bind_rows(importance_rf_temp, importance_xgb_temp, importance_gbm_temp)

# Remove dummy variables
importance_no_dummies_temp <- Varimp_temp %>%
  filter(!str_detect(Variable, paste0("^(", paste(cat_vars, collapse = "|"), ")\\.")))

# combine these two data frame
importance_new_temp <- bind_rows(importance_no_dummies_temp, importance_long_temp)
importance_new_temp$Model <- factor(importance_new_temp$Model, levels = c("RF", "XGBoost", "GBM"))

#################################################################################

# Select top 20 most important variables for each model
Varimp_top20_temp <- importance_new_temp %>%
  group_by(Model) %>%
  slice_max(order_by = Importance, n = 20) %>%
  ungroup()

Varimp_top20_temp <- Varimp_top20_temp %>%
  mutate(Variable = case_when(
    Variable == "temperature_mean_home" ~ "Outdoor_Temperature_Mean",
    Variable == "temp_mean_lag1_avg" ~ "Outdoor_Temperature_lag1",
    Variable == "month" ~ "Month", 
    Variable == "year" ~ "Year", 
    Variable == "yday" ~ "Day of Year", 
    Variable == "temperature_min_home" ~ "Outdoor_Temperature_Min",
    Variable == "temperature_max_home" ~ "Outdoor_Temperature_Max",
    Variable == "hum_mean_lag1_avg" ~ "Outdoor_Humidity_lag1",
    Variable == "humidity_mean_home" ~ "Outdoor_Humidity_Mean",
    Variable == "h5" ~ "Number_People",
    Variable == "Energia_calefacció_demanda_300m" ~ "Energy_Demand_Heating",
    Variable == "ct3_8a" ~ "Floor_Bedroom", 
    Variable == "cooling_system_day" ~ "Usage_Cooling_System_Day",
    Variable == "cooling_system_night" ~ "Usage_Cooling_System_Night",
    Variable == "heating_system_livingroom" ~ "Heating_System_Livingroom",
    Variable == "heating_system_bedroom" ~ "Heating_System_Bedroom",
    Variable == "ndvi_300" ~ "NDVI_300m", Variable == "ct8m" ~ "Volume_Bedroom",
    Variable == "maternal_age" ~ "Maternal_Age",
    Variable == "ct8l_f" ~ "Area_Window",
    Variable == "Energia_refrigeració_demanda_300m" ~ "Energy_Demand_Cooling",
    Variable == "sr" ~ "Solar_Radiation",
    Variable == "VALOR_AILLAMENTS_300m" ~ "Transmittance_Facade",
    Variable == "VALOR_FINESTRES_300m" ~ "Transmittance_Window",
    Variable == "Average_building" ~ "Construction_Year", 
    Variable == "r6_5" ~ "Window_Open_Kitchen_Day",
    Variable == "apavg" ~ "Atmospheric_Pressure",
    Variable == "rhavg" ~ "Relative_Humidity",
    Variable == "r6_2" ~ "Window_Open_Baby_Bedroom_Day",
    Variable == "r6_1" ~ "Window_Open_Parent_Bedroom_Day",
    Variable == "r6_3" ~ "Window_Open_Other_Bedroom_Day",
    Variable == "wind_direction_10m" ~ "Wind_Direction",
    Variable == "r6_4" ~ "Window_Open_Livingroom_Day",
    Variable == "n7_1a" ~ "Window_Open_Parent_Bedroom_Night",
    Variable == "n7_1b" ~ "Window_Open_Parent_Bedroom_Night_Half",
    Variable == "n7_3b" ~ "Window_Open_Parent_Livingroom_Night_Half",
    Variable == "n7_3a" ~ "Window_Open_Parent_Livingroom_Night",
    Variable == "N6_m03" ~ "Frequency_Close_Window",
    TRUE ~ Variable  # Keep other names unchanged
  ))

ggplot(Varimp_top20_temp, aes(x = Importance, 
                              y = fct_reorder(Variable, Importance), 
                              fill = Model)) +
  geom_col(show.legend = FALSE, width = 0.8, alpha = 0.85) +
  facet_wrap(~ Model, scales = "free_x", nrow = 1) +  
  scale_fill_manual(values = c("#1f77b4", "#6A0DAD", "#2E8B57")) +
  labs(
    x = "Variable Importance",
    y = NULL,  
    title = "Top 20 Variable Importance for Temperature Prediction"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40", size = 11),
    strip.text = element_text(face = "bold", size = 11),
    axis.text.y = element_text(size = 9, lineheight = 0.8), 
    axis.text.x = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.spacing = unit(1.5, "lines"),  
    plot.margin = margin(1, 1, 1, 2, "cm")  
  )


ggsave(file.path(figure_dir, "Var_importance_Across_models_temp_short_term.png"), 
       width = 12, height = 8, 
       dpi = 600)



#################################################################################
#==================          humidity         ========================# 
#################################################################################

# Calculate the weighted importance score for factor variables
trainset_hr <- readRDS(file.path(model_dir, "Temporal_Validation_daily", "train_dummy_dataset.rds"))

# First step: calculate the frequency of each dummy varaiables 
cat_vars <- c("difficult_pay_heating", "ct1", "ct6_m01", "ct7_m01",
              "ct8ec", "ct8k", "ct8g", "ct8h","ct8i","ct8j","heating_system_bedroom", 
              "heating_system_livingroom", "EC_2_32w","GS3_m01","N6_m01","N6_m03",
              "cooling_system_day","cooling_system_night","month")

dummy_freq_hr <- data.frame(dummy_var = character(), frequency = numeric())

for (var in cat_vars) {
  # find the var start variable
  dummy_names <- grep(paste0("^", var, "\\."), colnames(trainset_hr), value = TRUE)
  
  # calculate the frequency
  for (dummy in dummy_names) {
    freq <- mean(trainset_hr[[dummy]], na.rm = TRUE)
    dummy_freq_hr <- rbind(dummy_freq_hr, data.frame(Variable = dummy, frequency = freq))
  }
}

# Second step: calculate weighted improtance
importance_list_hr <- list(
  RF = importance_rf_hr, 
  XGBoost = importance_xgb_hr, 
  GBM = importance_gbm_hr
)

calc_weighted_importance <- function(importance_df, freq_df, category_prefix) {
  # Find the dummy variables related to this categorical variable in importance_df
  dummy_names <- grep(paste0("^", category_prefix, "\\."), importance_df$Variable, value = TRUE)
  
  # Extract "importance" and its corresponding frequency
  imp_sub <- importance_df %>%
    filter(Variable %in% dummy_names) %>%
    select(Variable, Importance)
  
  # combine with frequency database
  imp_freq <- imp_sub %>%
    left_join(freq_df, by = "Variable") %>%
    filter(!is.na(frequency)) 
  # calculate weighted importance
  weighted_imp <- sum(imp_freq$Importance * imp_freq$frequency) / sum(imp_freq$frequency)
  
  return(weighted_imp)
}

weighted_importance_hr <- sapply(cat_vars, function(cat_var) {
  sapply(importance_list_hr, function(df) {
    calc_weighted_importance(df, dummy_freq_hr, cat_var)
  })
})

weighted_importance_hr <- as.data.frame(weighted_importance_hr)

# convert the database form keep consistent with previous one
weighted_importance_hr <- weighted_importance_hr %>%
  tibble::rownames_to_column(var = "Model")

importance_long_hr <- weighted_importance_hr %>%
  pivot_longer(
    cols = -Model,
    names_to = "Variable",
    values_to = "Importance"
  )

# Third step: Combine with previous variable
Varimp_hr <- bind_rows(importance_rf_hr, importance_xgb_hr, importance_gbm_hr)

# Remove dummy variables
importance_no_dummies_hr <- Varimp_hr %>%
  filter(!str_detect(Variable, paste0("^(", paste(cat_vars, collapse = "|"), ")\\.")))

# combine these two data frame
importance_new_hr <- bind_rows(importance_no_dummies_hr, importance_long_hr)
importance_new_hr$Model <- factor(importance_new_hr$Model, levels = c("RF", "XGBoost", "GBM"))

#################################################################################

# Select top 20 most important variables for each model
Varimp_top20_hr <- importance_new_hr %>%
  group_by(Model) %>%
  slice_max(order_by = Importance, n = 20) %>%
  ungroup()

Varimp_top20_hr <- Varimp_top20_hr %>%
  mutate(Variable = case_when(
    Variable == "temperature_mean_home" ~ "Outdoor_Temperature_Mean",
    Variable == "temp_mean_lag1_avg" ~ "Outdoor_Temperature_lag1",
    Variable == "month" ~ "Month", 
    Variable == "year" ~ "Year", 
    Variable == "yday" ~ "Day of Year", 
    Variable == "temperature_min_home" ~ "Outdoor_Temperature_Min",
    Variable == "temperature_max_home" ~ "Outdoor_Temperature_Max",
    Variable == "hum_mean_lag1_avg" ~ "Outdoor_Humidity_lag1",
    Variable == "humidity_mean_home" ~ "Outdoor_Humidity_Mean",
    Variable == "h5" ~ "Number_People",
    Variable == "Energia_calefacció_demanda_300m" ~ "Energy_Demand_Heating",
    Variable == "ct3_8a" ~ "Floor_Bedroom", 
    Variable == "cooling_system_day" ~ "Usage_Cooling_System_Day",
    Variable == "cooling_system_night" ~ "Usage_Cooling_System_Night",
    Variable == "heating_system_livingroom" ~ "Heating_System_Livingroom",
    Variable == "heating_system_bedroom" ~ "Heating_System_Bedroom",
    Variable == "ndvi_300" ~ "NDVI_300m", Variable == "ct8m" ~ "Volume_Bedroom",
    Variable == "maternal_age" ~ "Maternal_Age",
    Variable == "ct8l_f" ~ "Area_Window",
    Variable == "Energia_refrigeració_demanda_300m" ~ "Energy_Demand_Cooling",
    Variable == "sr" ~ "Solar_Radiation",
    Variable == "VALOR_AILLAMENTS_300m" ~ "Transmittance_Facade",
    Variable == "VALOR_FINESTRES_300m" ~ "Transmittance_Window",
    Variable == "Average_building" ~ "Construction_Year", 
    Variable == "r6_5" ~ "Window_Open_Kitchen_Day",
    Variable == "apavg" ~ "Atmospheric_Pressure",
    Variable == "rhavg" ~ "Relative_Humidity",
    Variable == "r6_2" ~ "Window_Open_Baby_Bedroom_Day",
    Variable == "r6_1" ~ "Window_Open_Parent_Bedroom_Day",
    Variable == "wind_direction_10m" ~ "Wind_Direction",
    Variable == "r6_4" ~ "Window_Open_Livingroom_Day",
    Variable == "n7_1a" ~ "Window_Open_Parent_Bedroom_Night",
    Variable == "n7_1b" ~ "Window_Open_Parent_Bedroom_Night_Half",
    Variable == "n7_3b" ~ "Window_Open_Parent_Livingroom_Night_Half",
    Variable == "n7_3a" ~ "Window_Open_Parent_Livingroom_Night",
    Variable == "N6_m03" ~ "Frequency_Close_Window",
    TRUE ~ Variable  # Keep other names unchanged
  ))
# Plot variable importance

ggplot(Varimp_top20_hr, aes(x = Importance, 
                            y = fct_reorder(Variable, Importance), 
                            fill = Model)) +
  geom_col(show.legend = FALSE, width = 0.8, alpha = 0.85) +
  facet_wrap(~ Model, scales = "free_x", nrow = 1) +  
  scale_fill_manual(values = c("#1f77b4", "#6A0DAD", "#2E8B57")) +
  labs(
    x = "Variable Importance",
    y = NULL,  
    title = "Top 20 Variable Importance for Humidity Prediction"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40", size = 11),
    strip.text = element_text(face = "bold", size = 11),
    axis.text.y = element_text(size = 9, lineheight = 0.8), 
    axis.text.x = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.spacing = unit(1.5, "lines"),  
    plot.margin = margin(1, 1, 1, 2, "cm")  
  )

ggsave(file.path(figure_dir, "Var_importance_Across_models_hr_short_term.png"), 
       width = 12, height = 8, 
       dpi = 600)

##################################################################################
# END

# Calculate the percentage contribution of each feature to the model
Varimp <- Varimp %>%
  group_by(Model) %>%
  mutate(Total_Importance = sum(Importance),
         Percentage_Contribution = Importance / Total_Importance * 100) 


