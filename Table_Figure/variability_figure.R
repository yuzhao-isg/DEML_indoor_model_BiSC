#-----------------------------------------------------------------------------#
#                    variability figure for indoor model                      #
#-----------------------------------------------------------------------------#
# Description:
# This script produces variability and distribution figures comparing indoor and
# outdoor temperature/humidity across seasons and months.
#
# Outputs:
# - results/Figure/variability/*.png
# - results/Figure/Distribution/*.png

rm(list = ls())
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

script_dir <- if (!is.null(sys.frames()[[1]]$ofile)) {
  dirname(normalizePath(sys.frames()[[1]]$ofile))
} else {
  getwd()
}
project_root <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = FALSE)
model_dir <- file.path(project_root, "db", "output", "Model")
exposure_dir <- file.path(project_root, "temperature_exposure", "results", "New_Data", "Whole_Pregnancy")
figure_dir <- file.path(project_root, "results", "Figure")
if (!dir.exists(figure_dir)) dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# combine the indoor temperature with outdoor
load(file.path(exposure_dir, "temp_hr_bisc.rds"))
temp_hr_bisc$date <- as.Date(temp_hr_bisc$date)
temp_hr_bisc <- temp_hr_bisc %>%
  mutate(month = factor(months(date, abbreviate = TRUE), levels = month.abb))

monthly_data <- temp_hr_bisc %>%
  group_by(month) %>%
  summarize(mean_indoor_temp = mean(indoor_temp, na.rm = TRUE),
            mean_outdoor_temp = mean(temperature_mean_home, na.rm = TRUE),
            sd_indoor_temp = sd(indoor_temp, na.rm = TRUE),
            sd_outdoor_temp = sd(temperature_mean_home, na.rm = TRUE),
            mean_indoor_humidity = mean(indoor_hr, na.rm = TRUE),
            mean_outdoor_humidity = mean(humidity_mean_home, na.rm = TRUE),
            sd_indoor_humidity = sd(indoor_hr, na.rm = TRUE),
            sd_outdoor_humidity = sd(humidity_mean_home, na.rm = TRUE))

# Calculate lower and upper bounds for indoor and outdoor temperatures
monthly_data <- monthly_data %>%
  mutate(outdoor_temp_lower = mean_outdoor_temp - sd_outdoor_temp,
         outdoor_temp_upper = mean_outdoor_temp + sd_outdoor_temp,
         indoor_temp_lower = mean_indoor_temp - sd_indoor_temp,
         indoor_temp_upper = mean_indoor_temp + sd_indoor_temp,
         outdoor_hr_lower = mean_outdoor_humidity - sd_outdoor_humidity,
         outdoor_hr_upper = mean_outdoor_humidity + sd_outdoor_humidity,
         indoor_hr_lower = mean_indoor_humidity - sd_indoor_humidity,
         indoor_hr_upper = mean_indoor_humidity + sd_indoor_humidity
  )

monthly_data$month <- factor(monthly_data$month, 
                             levels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                        "Jun", "Jul", "Aug", "Sep", "Oct", 
                                        "Nov", "Dec"), 
                             ordered = TRUE)
monthly_data$month_numeric <- as.numeric(monthly_data$month)

#-----------------------------------------------------------------------------#
#                             Merge boxplot and Mean(SD)                      #
#-----------------------------------------------------------------------------#

#==================          temperature         ========================# 

# set the month to full name
month_abbr_to_full <- setNames(month.name, month.abb) 
long_temp <- temp_hr_bisc %>%
  pivot_longer(cols = c(temperature_mean_home, indoor_temp), 
               names_to = "Temperature_Type", values_to = "Temperature") %>%
  mutate(month_full = month_abbr_to_full[month]) 
# Modify Temperature_Type for better legend display
long_temp$Temperature_Type <- factor(long_temp$Temperature_Type, 
                                     levels = c("temperature_mean_home", "indoor_temp"),
                                     labels = c("Outdoor", "Indoor"))


ggplot() +

  # Outdoor temperature variability (min-max shaded area)
  geom_ribbon(data = monthly_data, aes(x = month_numeric, ymin = outdoor_temp_lower, ymax = outdoor_temp_upper), 
              fill = "#FDB462", alpha = 0.4) +
  geom_ribbon(data = monthly_data, aes(x = month_numeric, ymin = indoor_temp_lower, ymax = indoor_temp_upper), 
              fill = "#80B1D3", alpha = 0.4) +
  
  # Outdoor temperature mean line with legend
  geom_line(data = monthly_data, aes(x = month_numeric, y = mean_outdoor_temp, color = "Outdoor"), 
            size = 1.2, linetype = "solid") +  
  geom_line(data = monthly_data, aes(x = month_numeric, y = mean_indoor_temp, color = "Indoor"), 
            size = 1.2, linetype = "solid") + 
  
  # Boxplots for real & predicted indoor temperature
  geom_boxplot(data = long_temp, aes(x = as.factor(month_full), y = Temperature, fill = Temperature_Type),
               position = position_dodge(width = 0.6), width = 0.5, alpha = 0.7) +
  
  labs(x = "Month", y = "Temperature (°C)") +
  
  scale_fill_manual(name = "Temperature Type", 
                    values = c("Outdoor" = "#FF8C00", 
                               "Indoor" = "#377EB8")) +
  scale_color_manual(name = "Mean±SD", 
                     values = c("Outdoor" = "#FF8C00",
                                "Indoor" = "#377EB8")) +
  
  scale_x_discrete(name = "Month", 
                   limits = month.name) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "bold"),
        axis.text.y = element_text(size = 18), 
        axis.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 18, face = "bold"),
        legend.position = "top") +
  
  guides(fill = guide_legend(title = "Temperature Type"),
         color = guide_legend(title = "Mean±SD"))


ggsave(file.path(figure_dir, "variability", "Variability_temp_Mean.png"), 
       width = 12, height = 8, 
       dpi = 600) 



#==================          humidity         ========================# 

long_hr <- temp_hr_bisc %>%
  pivot_longer(cols = c(humidity_mean_home, indoor_hr), 
               names_to = "Humidity_Type", values_to = "Humidity") %>%
  mutate(month_full = month_abbr_to_full[month]) 
# Modify Temperature_Type for better legend display
long_hr$Humidity_Type <- factor(long_hr$Humidity_Type, 
                                     levels = c("humidity_mean_home", "indoor_hr"),
                                     labels = c("Outdoor", "Indoor"))


ggplot() +
  
  # Outdoor temperature variability (min-max shaded area)
  geom_ribbon(data = monthly_data, aes(x = month_numeric, ymin = outdoor_hr_lower, ymax = outdoor_hr_upper), 
              fill = "#E6CCB3", alpha = 0.4) +
  geom_ribbon(data = monthly_data, aes(x = month_numeric, ymin = indoor_hr_lower, ymax = indoor_hr_upper), 
              fill = "#B2FFFF", alpha = 0.4) +
  
  # Outdoor temperature mean line with legend
  geom_line(data = monthly_data, aes(x = month_numeric, y = mean_outdoor_humidity, color = "Outdoor"), 
            size = 1.2, linetype = "solid") +  
  geom_line(data = monthly_data, aes(x = month_numeric, y = mean_indoor_humidity, color = "Indoor"), 
            size = 1.2, linetype = "solid") + 
  
  # Boxplots for real & predicted indoor temperature
  geom_boxplot(data = long_hr, aes(x = as.factor(month_full), y = Humidity, fill = Humidity_Type),
               position = position_dodge(width = 0.6), width = 0.5, alpha = 0.7) +
  
  labs(x = "Month", y = "Humidity (%)") +
  
  scale_fill_manual(name = "Humidity Type", 
                    values = c("Outdoor" = "#CC9966",  
                               "Indoor" = "#20B2AA")) +
  scale_color_manual(name = "Mean±SD", 
                     values = c("Outdoor" = "#CC9966",  
                                "Indoor" = "#20B2AA")) +
  
  scale_x_discrete(name = "Month", 
                   limits = month.name) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "bold"),
        axis.text.y = element_text(size = 18), 
        axis.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 18, face = "bold"),
        legend.position = "top") +
  
  guides(fill = guide_legend(title = "Humidity Type"),
         color = guide_legend(title = "Mean±SD"))


ggsave(file.path(figure_dir, "variability", "Variability_hr_Mean.png"), 
       width = 12, height = 8, 
       dpi = 600) 

###############################################################################


#-----------------------------------------------------------------------------#
#                           distribution of measurements                      #
#-----------------------------------------------------------------------------#

path_mod <- model_dir

load(file.path(path_mod, "temp_preidctors_data_model.RData"))

seasonal_counts <- temp_preidctors_data_model %>%
  group_by(season_2) %>%
  summarise(
    temp_obs = sum(!is.na(indoor_temp)),
    humidity_obs = sum(!is.na(indoor_hr)),
    .groups = 'drop'
  )

#-----------------------------------------------------------------------------#
#                                   Temperature                               #
#-----------------------------------------------------------------------------#
library(patchwork)
temp_measured <- ggplot(temp_preidctors_data_model, 
       aes(x = date, y = indoor_temp, color = season_2)) +
  geom_point(alpha = 0.4) +
  scale_color_manual(
    values = c("warm_season" = "#D62728",  
               "cool_season" = "#1F77B4"),
    labels = c("Cold season", "Warm season")
  ) +
  scale_x_date(
    date_labels = "%Y-%m",
    date_breaks = "4 month", 
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    x = "Calendar date", 
    y = "Indoor temperature (°C)",
    color = "Season",
    title = "Measured"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5,  
                              margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )


temp_hr_bisc <- temp_hr_bisc %>%
  mutate(month_day = format(date, "%m-%d"),
         season_2 = case_when(
           month_day >= "05-15" & month_day <= "10-15" ~ "warm_season",
           TRUE ~ "cool_season"
         ))

temp_hr_bisc_clean <- temp_hr_bisc %>%
  filter(!is.na(season_2), !is.na(date), !is.na(indoor_temp))
temp_prediction <- ggplot(temp_hr_bisc_clean, 
                          aes(x = date, y = indoor_temp, color = season_2)) +
  geom_point(alpha = 0.4) +
  scale_color_manual(
    values = c("warm_season" = "#D62728",  
               "cool_season" = "#1F77B4"),
    labels = c("Cold season", "Warm season")
  ) +
  scale_x_date(
    date_labels = "%Y-%m",
    date_breaks = "4 month", 
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    x = "Calendar date", 
    y = "Indoor temperature (°C)",
    color = "Season",
    title = "Predicted"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5,  
                              margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

all_dates <- c(temp_preidctors_data_model$date, temp_hr_bisc_clean$date)
date_range <- range(all_dates, na.rm = TRUE)

temp_measured <- temp_measured +
  scale_x_date(
    limits = date_range,
    date_labels = "%Y-%m",
    date_breaks = "4 month", 
    expand = expansion(mult = c(0.01, 0.01))
  )

temp_prediction <- temp_prediction +
  scale_x_date(
    limits = date_range,
    date_labels = "%Y-%m",
    date_breaks = "4 month", 
    expand = expansion(mult = c(0.01, 0.01))
  )

temp_measured / temp_prediction +
  plot_annotation(
    title = "Temporal Distribution of Measurements and Predictions",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

ggsave(file.path(project_root, "results", "Figure", "Distribution", "measured_predictions_temp.png"), 
       width = 12, height = 8, dpi = 600) 



#-----------------------------------------------------------------------------#
#                                    Humidity                                 #
#-----------------------------------------------------------------------------#

hr_measured <- ggplot(temp_preidctors_data_model, 
                        aes(x = date, y = indoor_hr, color = season_2)) +
  geom_point(alpha = 0.4) +
  scale_color_manual(
    values = c("warm_season" = "#FD8D3C",  
               "cool_season" = "#74C476"),
    labels = c("Cold season", "Warm season")
  ) +
  scale_x_date(
    date_labels = "%Y-%m",
    date_breaks = "4 month", 
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    x = "Calendar date", 
    y = "Indoor humidty (°C)",
    color = "Season",
    title = "Measured"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5,  
                              margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )


temp_hr_bisc <- temp_hr_bisc %>%
  mutate(month_day = format(date, "%m-%d"),
         season_2 = case_when(
           month_day >= "05-15" & month_day <= "10-15" ~ "warm_season",
           TRUE ~ "cool_season"
         ))

hr_prediction <- ggplot(temp_hr_bisc_clean, 
                          aes(x = date, y = indoor_hr, color = season_2)) +
  geom_point(alpha = 0.4) +
  scale_color_manual(
    values = c("warm_season" = "#FD8D3C",  
               "cool_season" = "#74C476"),
    labels = c("Cold season", "Warm season")
  ) +
  scale_x_date(
    date_labels = "%Y-%m",
    date_breaks = "4 month", 
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    x = "Calendar date", 
    y = "Indoor humidity (°C)",
    color = "Season",
    title = "Predictions"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5,  
                              margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

all_dates <- c(temp_preidctors_data_model$date, temp_hr_bisc_clean$date)
date_range <- range(all_dates, na.rm = TRUE)

hr_measured <- hr_measured +
  scale_x_date(
    limits = date_range,
    date_labels = "%Y-%m",
    date_breaks = "4 month", 
    expand = expansion(mult = c(0.01, 0.01))
  )

hr_prediction <- hr_prediction +
  scale_x_date(
    limits = date_range,
    date_labels = "%Y-%m",
    date_breaks = "4 month", 
    expand = expansion(mult = c(0.01, 0.01))
  )

hr_measured / hr_prediction +
  plot_annotation(
    title = "Temporal Distribution of Measurements and Predictions",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

ggsave(file.path(project_root, "results", "Figure", "Distribution", "measured_predictions_hr.png"), 
       width = 12, height = 8, dpi = 600) 




#-----------------------------------------------------------------------------#
#                                abstract figure                              #
#-----------------------------------------------------------------------------#
participant_id <- "10005511"


sample_measured <- temp_preidctors_data_model %>%
  filter(id_mother == participant_id) %>%
  mutate(data_type = "Measured (2 weeks)")


sample_predicted <- temp_hr_bisc %>%
  mutate(
    month_day = format(date, "%m-%d"),
    season_2 = case_when(
      month_day >= "05-15" & month_day <= "10-15" ~ "warm_season",
      TRUE ~ "cool_season"
    ),
    data_type = "Predicted (full pregnancy)",
    month = month(date)
  ) %>%
  filter(id_mother == participant_id)

combined_data <- bind_rows(sample_measured, sample_predicted)

date_range <- range(combined_data$date, na.rm = TRUE)

temp_abstract <- ggplot(combined_data, 
                        aes(x = date, y = indoor_temp, color = season_2)) + 
  geom_point(alpha = 0.6) + 
  scale_color_manual(
    values = c("warm_season" = "#D62728", "cool_season" = "#1F77B4"),
    labels = c("Cold season", "Warm season"),
    name = "Season"
  ) +
  scale_x_date(
    limits = date_range,
    date_labels = "%Y-%m",
    date_breaks = "1 month", 
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  facet_wrap(~ data_type, ncol = 1, scales = "fixed") +
  labs(
    x = "Calendar date", 
    y = "Indoor temperature (°C)",
    title = "Temperature"
  ) +
  theme_minimal(base_size = 24) +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 20)
  )

print(temp_abstract)

ggsave(file.path(project_root, "results", "Figure", "abstract_temp_BE.png"), 
       plot = temp_abstract, width = 17, height = 6, dpi = 600) 

################################################################################
# HUMIDITY

hr_abstract <- ggplot(combined_data, 
                        aes(x = date, y = indoor_hr, color = season_2)) + 
  geom_point(alpha = 0.6) + 
  scale_color_manual(
    values = c("warm_season" = "#FD8D3C", "cool_season" = "#74C476"),
    labels = c("Cold season", "Warm season"),
    name = "Season"
  ) +
  scale_x_date(
    limits = date_range,
    date_labels = "%Y-%m",
    date_breaks = "1 month", 
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  facet_wrap(~ data_type, ncol = 1, scales = "fixed") +
  labs(
    x = "Calendar date", 
    y = "Indoor humidity (%)",
    title = "Humidity"
  ) +
  theme_minimal(base_size = 24) +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 20)
  )
print(hr_abstract)

ggsave(file.path(project_root, "results", "Figure", "abstract_hr_BE.png"), 
       plot = hr_abstract, width = 17, height = 6, dpi = 600) 

###############################################################################


#-----------------------------------------------------------------------------#
#                                   value map                                 #
#-----------------------------------------------------------------------------#
library(ggmap)
library(ggplot2)
library(dplyr)
# prepare the map
register_stadiamaps(key = "79951dab-b1c3-48e4-9102-8baec6709d11")

barcelona_bbox <- c(left = 2.05, bottom = 41.30, right = 2.25, top = 41.45)
barcelona_map <- get_stadiamap(bbox = barcelona_bbox, zoom = 12, maptype = "stamen_toner_lite")

# prepare the participants value
load(file.path(project_root, "db", "temperature", "participant_coords.RData"))
names(participant)[2] <- "id_mother"
load(file.path(exposure_dir, "temp_hr_bisc.rds"))

participant_means <- temp_hr_bisc %>% 
  left_join(participant, by = c("id_mother", "gid")) %>%
  mutate(month_day = format(date, "%m-%d"),
         season_2 = case_when(
           month_day >= "05-15" & month_day <= "10-15" ~ "warm_season",
           TRUE ~ "cold_season")) %>%
  select(id_mother, gid, date, indoor_temp, indoor_hr, longitude, latitude, season_2) %>%
  group_by(id_mother, season_2, gid,longitude, latitude) %>%
  summarise(mean_temp = mean(indoor_temp, na.rm = TRUE),
            mean_hr = mean(indoor_hr, na.rm = TRUE)) %>%
  ungroup()
participant_means <- participant_means %>%
  mutate(season_2 = factor(season_2, levels = c("warm_season", "cold_season")))

ggmap(barcelona_map) +
  geom_point(
    data = participant_means, 
    aes(x = longitude, y = latitude, color = mean_temp), 
    size = 3, alpha = 0.8, shape = 16
  ) +
  scale_color_gradientn(
    colors = c(
      "#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8",
      "#FFFFBF", "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026"
    ),
    name = "Mean Temperature (°C)",
    limits = c(min(participant_means$mean_temp, na.rm = TRUE),
               max(participant_means$mean_temp, na.rm = TRUE)),
    guide = guide_colorbar(barwidth = 15, barheight = 1)
  ) +
  facet_wrap(~ season_2, ncol = 2, 
             labeller = as_labeller(c(
               "warm_season" = "Warm season",
               "cold_season" = "Cold season"
             ))) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 18)
  )

ggsave(file.path(project_root, "results", "Figure", "Distribution", "temp_map.png"), 
       width = 10, height = 8, dpi = 600) 


# Humidity
ggmap(barcelona_map) +
  geom_point(
    data = participant_means, 
    aes(x = longitude, y = latitude, color = mean_hr), 
    size = 3, alpha = 0.85, shape = 16
  ) +
  scale_color_gradientn(
    colors = c("#7B3F00", "#A0522D", "#C68642", "#E6CCB2",
               "#A8D5BA", "#66B28E", "#2E8B57", "#1C5D37"),
    name = "Mean Relative Humidity (%)",
    guide = guide_colorbar(barwidth = 15, barheight = 1)
  ) +
  facet_wrap(~ season_2, ncol = 2,
             labeller = as_labeller(c(
               "warm_season" = "Warm season",
               "cold_season" = "Cold season"
             ))) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 18)
  )


ggsave(file.path(project_root, "results", "Figure", "Distribution", "hr_map.png"), 
       width = 10, height = 8, dpi = 600) 



#-----------------------------------------------------------------------------#
#                    box figure for measured/predicted value                  #
#-----------------------------------------------------------------------------#
measured_long <- temp_preidctors_data_model %>%
  mutate(type = "Measured") %>%
  select(id_mother, season_2, indoor_temp, indoor_hr, type)

predicted_long <- temp_hr_bisc %>%
  mutate(type = "Predicted") %>%
  mutate(month_day = format(date, "%m-%d"),
         season_2 = case_when(
           month_day >= "05-15" & month_day <= "10-15" ~ "warm_season",
           TRUE ~ "cool_season")) %>%
  select(id_mother, season_2, indoor_temp, indoor_hr, type)

measured_overall <- temp_preidctors_data_model %>%
  mutate(season_2 = "Overall", type = "Measured") %>%
  select(id_mother, season_2, indoor_temp, indoor_hr, type)

predicted_overall <- temp_hr_bisc %>%
  mutate(season_2 = "Overall", type = "Predicted") %>%
  select(id_mother, season_2, indoor_temp, indoor_hr, type)

combined_long <- bind_rows(measured_long, predicted_long,
                           measured_overall, predicted_overall) %>%
  mutate(season_2 = factor(season_2, levels = c("Overall", "warm_season", "cool_season")))

library(ggplot2)

ggplot(combined_long, aes(x = season_2, y = indoor_temp, fill = type)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1, position = position_dodge(width = 0.8)) +
  scale_x_discrete(labels = c("warm_season" = "Warm season", 
                              "cool_season" = "Cold season", 
                              "Overall" = "Whole pregnancy")) +
  scale_fill_manual(values = c("Measured" = "#1F77B4", "Predicted" = "#FF7F0E")) +
  labs(
    x = "Season",
    y = "Indoor Temperature (°C)",
    fill = "Data type",
    title = "Comparison of Measured and Predicted Indoor Temperature"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

ggsave(file.path(project_root, "results", "Figure", "Distribution", "temp_box.png"), 
       width = 10, height = 8, dpi = 600) 

ggplot(combined_long, aes(x = season_2, y = indoor_hr, fill = type)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1, position = position_dodge(width = 0.8)) +
  scale_x_discrete(labels = c("warm_season" = "Warm season", 
                              "cool_season" = "Cold season", 
                              "Overall" = "Whole pregnancy")) +
  scale_fill_manual(values = c("Measured" = "#20B2AA", "Predicted" = "#CC9966")) +
  labs(
    x = "Season",
    y = "Indoor Humidity (%)",
    fill = "Data type",
    title = "Comparison of Measured and Predicted Indoor Humidity"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

ggsave(file.path(project_root, "results", "Figure", "Distribution", "hr_box.png"), 
       width = 10, height = 8, dpi = 600) 
#-----------------------------------------------------------------------------#
#                    figure for comparing in/outdoor temp                     #
#-----------------------------------------------------------------------------#

temp_hr_bisc$date <- as.Date(temp_hr_bisc$date)
temp_hr_bisc <- temp_hr_bisc %>%
  mutate(month = factor(months(date, abbreviate = TRUE), levels = month.abb))

monthly_data <- temp_hr_bisc %>%
  group_by(month) %>%
  summarize(mean_indoor_temp = mean(indoor_temp, na.rm = TRUE),
            mean_outdoor_temp = mean(temperature_mean_home, na.rm = TRUE),
            sd_indoor_temp = sd(indoor_temp, na.rm = TRUE),
            sd_outdoor_temp = sd(temperature_mean_home, na.rm = TRUE),
            mean_indoor_humidity = mean(indoor_hr, na.rm = TRUE),
            mean_outdoor_humidity = mean(humidity_mean_home, na.rm = TRUE),
            sd_indoor_humidity = sd(indoor_hr, na.rm = TRUE),
            sd_outdoor_humidity = sd(humidity_mean_home, na.rm = TRUE))

# Calculate lower and upper bounds for indoor and outdoor temperatures
monthly_data <- monthly_data %>%
  mutate(outdoor_temp_lower = mean_outdoor_temp - sd_outdoor_temp,
         outdoor_temp_upper = mean_outdoor_temp + sd_outdoor_temp,
         indoor_temp_lower = mean_indoor_temp - sd_indoor_temp,
         indoor_temp_upper = mean_indoor_temp + sd_indoor_temp,
         outdoor_hr_lower = mean_outdoor_humidity - sd_outdoor_humidity,
         outdoor_hr_upper = mean_outdoor_humidity + sd_outdoor_humidity,
         indoor_hr_lower = mean_indoor_humidity - sd_indoor_humidity,
         indoor_hr_upper = mean_indoor_humidity + sd_indoor_humidity
  )

monthly_data$month <- factor(monthly_data$month, 
                             levels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                        "Jun", "Jul", "Aug", "Sep", "Oct", 
                                        "Nov", "Dec"), 
                             ordered = TRUE)
monthly_data$month_numeric <- as.numeric(monthly_data$month)

#==================          temperature         ========================# 
# Create the plot
ggplot(monthly_data, aes(x = month_numeric)) +
  
  geom_ribbon(aes(ymin = outdoor_temp_lower, ymax = outdoor_temp_upper), 
              fill = "#B2FFFF", alpha = 0.4) +
  geom_ribbon(aes(ymin = indoor_temp_lower, ymax = indoor_temp_upper), 
              fill = "#DDA0DD", alpha = 0.4) +
  
  geom_line(aes(y = mean_outdoor_temp, color = "Outdoor", group = 1), 
            size = 1.2) +
  geom_line(aes(y = mean_indoor_temp, color = "Indoor", group = 1), 
            size = 1.2) +
  labs(x = "Month", y = "Temperature (°C)") +
  scale_x_continuous(breaks = 1:12, labels = levels(monthly_data$month)) + 
  scale_color_manual(name = "Mean Temperature", 
                     values = c("Outdoor" = "#20B2AA", 
                                "Indoor" = "#BA55D3")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 20, face = "bold"),
        axis.text.y = element_text(size = 20), 
        axis.title = element_text(size = 28, face = "bold"),
        legend.title = element_text(size = 25, face = "bold"),
        legend.text = element_text(size = 23, face = "bold"),
        legend.position = "top")

ggsave(file.path(project_root, "results", "Figure", "Variability_Temp_abstract.png"), 
       width = 12, height = 8, 
       dpi = 600) 

#==================          humidity         ========================# 

ggplot(monthly_data, aes(x = month_numeric)) +
  
  geom_ribbon(aes(ymin = outdoor_hr_lower, ymax = outdoor_hr_upper), 
              fill = "#FDB462", alpha = 0.4) +
  geom_ribbon(aes(ymin = indoor_hr_lower, ymax = indoor_hr_upper), 
              fill = "#80B1D3", alpha = 0.4) +
  
  geom_line(aes(y = mean_outdoor_humidity, color = "Outdoor", group = 1), 
            size = 1.2) +
  geom_line(aes(y = mean_indoor_humidity, color = "Indoor", group = 1), 
            size = 1.2) +
  labs(x = "Month", y = "Humidity (%)") +
  scale_x_continuous(breaks = 1:12, labels = levels(monthly_data$month)) + 
  scale_color_manual(
    name = "Mean Humidity",
    values = c("Outdoor" = "#FF7F00",  
               "Indoor" = "#377EB8")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 20, face = "bold"),
        axis.text.y = element_text(size = 20), 
        axis.title = element_text(size = 28, face = "bold"),
        legend.title = element_text(size = 25, face = "bold"),
        legend.text = element_text(size = 23, face = "bold"),
        legend.position = "top")

ggsave(file.path(project_root, "results", "Figure", "Variability_Humidity_abstract.png"), 
       width = 12, height = 8, 
       dpi = 600) 


#-----------------------------------------------------------------------------#
#                    calculate the differences between O and I                #
#-----------------------------------------------------------------------------#

temp_hr_difference <- temp_hr_bisc %>%
  mutate(temperature_diff = indoor_temp - temperature_mean_home,
         humidity_diff = indoor_hr - humidity_mean_home) %>%
  mutate(month_day = format(date, "%m-%d"),
         month = month(date),
          season_2 = case_when(
            month_day >= "05-15" & month_day <= "10-15" ~ "Warm season",
            TRUE ~ "Cool season"
          ))

seasonal_diff <- temp_hr_difference %>%
  group_by(month) %>%
  summarise(mean_temp_diff = mean(temperature_diff, na.rm = TRUE), 
            median_temp_diff = median(temperature_diff, na.rm = TRUE),
            iqr_temp_diff = IQR(temperature_diff, na.rm = TRUE),
            sd_temp_diff = sd(temperature_diff, na.rm = TRUE),
            mean_hr_diff = mean(humidity_diff, na.rm = TRUE), 
            median_hr_diff = median(humidity_diff, na.rm = TRUE),
            iqr_hr_diff = IQR(humidity_diff, na.rm = TRUE),
            sd_hr_diff = sd(humidity_diff, na.rm = TRUE))


#  season_2    mean_temp_diff median_temp_diff sd_temp_diff mean_hr_diff median_hr_diff sd_hr_diff
#   1 Cool season           6.78             6.57         2.04        -9.69          -10.2       8.42
# 2 Warm season           3.05             2.86         1.65        -9.96          -10.4       6.26


# season mean_temp_diff median_temp_diff sd_temp_diff mean_hr_diff median_hr_diff sd_hr_diff
# <chr>           <dbl>            <dbl>        <dbl>        <dbl>          <dbl>      <dbl>
# 1 Autumn           5.04             4.79         2.01       -10.2          -10.9        7.43
# 2 Spring           5.74             5.73         1.62        -8.72          -9.08       7.81
# 3 Summer           2.45             2.31         1.50        -9.33          -9.56       5.96
# 4 Winter           8.06             7.91         2.18        -9.52         -10.2        9.53


seasonal_sum <- temp_hr_bisc %>%
  group_by(season) %>%
  summarise(mean_temp = mean(indoor_temp, na.rm = TRUE), 
            median_temp = median(indoor_temp, na.rm = TRUE),
            sd_temp = sd(indoor_temp, na.rm = TRUE),
            mean_hr = mean(indoor_hr, na.rm = TRUE), 
            median_hr = median(indoor_hr, na.rm = TRUE),
            sd_hr = sd(indoor_hr, na.rm = TRUE))
# season mean_temp median_temp sd_temp mean_hr median_hr sd_hr
# <chr>      <dbl>       <dbl>   <dbl>   <dbl>     <dbl> <dbl>
# 1 Autumn      22.9        22.7    2.61    61.1      61.2  6.32
# 2 Spring      20.8        20.5    1.78    60.7      60.8  5.79
# 3 Summer      26.9        27.3    1.63    58.9      59.1  5.15
# 4 Winter      19.0        19.0    1.16    61.1      61.3  6.70