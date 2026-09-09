# Process indoor temperature and humidity logger data for model development.
#
# Workflow:
# 1. Detect temperature and humidity outliers using daily and weekly IQR limits.
# 2. Replace flagged sensor values with a local moving-average estimate.
# 3. Keep days with at least 20 hours of indoor measurements.
# 4. Calculate daily indoor temperature, humidity, and dew-point summaries.
# 5. Merge indoor summaries with outdoor temperature predictors.
# 6. Save the training dataset and produce overall and seasonal summaries.
#
# Run this script from the repository root. Raw data belongs in db/input and
# generated databases belong in db/output.
library(dplyr)
library(tidyverse)
library(ggplot2)
library(zoo)
rm(list = ls())

setwd("db")

load("input/indoor_temperature/TT_H_indoor_process.RData")
##################################################################################
# I used many methods to draw the time series plot for indoor temperature and humidity to check the fluctuation of the value
# Plotting checks are kept in this processing script.


#############-----------------------------------------------------###############
#############                       Remove outlier                ############### 
#############-----------------------------------------------------###############
# Outliers detection using the interquartile range (IQR) method, [Q1 -1.5IQR, Q3+1.5IQR], both daily and weekly distribution, the outliers replaced by the value of Q1 -1.5IQR or Q3+1.5IQR

# First Step: check the outlier

data_process <- data_process %>%
  group_by(time_d, id) %>%
  mutate(
    Q1_daily_temp = quantile(temp, na.rm = TRUE, 0.25),
    Q3_daily_temp = quantile(temp, na.rm = TRUE, 0.75),
    IQR_daily_temp = IQR(temp), 
    lower_bound_daily_temp = Q1_daily_temp - IQR_daily_temp*1.5,
    upper_bound_daily_temp = Q3_daily_temp + IQR_daily_temp*1.5,
    daily_median_temp = median(temp),
    Q1_daily_hr = quantile(hr, na.rm = TRUE, 0.25),
    Q3_daily_hr = quantile(hr, na.rm = TRUE, 0.75),
    IQR_daily_hr = IQR(hr), 
    lower_bound_daily_hr = Q1_daily_hr - IQR_daily_hr*1.5,
    upper_bound_daily_hr= Q3_daily_hr + IQR_daily_hr*1.5,
    daily_median_hr = median(hr)
  )

data_process <- data_process %>%
  group_by(week, id) %>%
  mutate(
    Q1_weekly_temp = quantile(temp, na.rm = TRUE, 0.25),
    Q3_weekly_temp = quantile(temp, na.rm = TRUE, 0.75),
    IQR_weekly_temp = IQR(temp), 
    lower_bound_weekly_temp = Q1_weekly_temp - IQR_weekly_temp*1.5,
    upper_bound_weekly_temp = Q3_weekly_temp + IQR_weekly_temp*1.5,
    Q1_weekly_hr = quantile(hr, na.rm = TRUE, 0.25),
    Q3_weekly_hr = quantile(hr, na.rm = TRUE, 0.75),
    IQR_weekly_hr = IQR(hr), 
    lower_bound_weekly_hr = Q1_weekly_hr - IQR_weekly_hr*1.5,
    upper_bound_weekly_hr = Q3_weekly_hr + IQR_weekly_hr*1.5
  )

detect_outlier_indoor_temp <- data_process %>%
  mutate(check_outlier_temp = ifelse((temp > upper_bound_daily_temp & temp > upper_bound_weekly_temp)
                                     | (temp < lower_bound_daily_temp & temp < lower_bound_weekly_temp), "TRUE", "FALSE"),
         check_outlier_hr = ifelse((hr > upper_bound_daily_hr & temp > upper_bound_weekly_hr)
                                   | (hr < lower_bound_daily_hr & hr < lower_bound_weekly_hr), "TRUE", "FALSE"))
table(detect_outlier_indoor_temp$check_outlier_temp)
# FALSE    TRUE 
# 1588046   13241  0.827%
table(detect_outlier_indoor_temp$check_outlier_hr)
#  FALSE    TRUE 
# 1597771    3516  0.220%
save(detect_outlier_indoor_temp, file = "output/Clean_indoor_temp/detect_outlier_indoor_temp.RData")


# Second Step: impute the outliers using moving average 10-points
# Function to impute outliers using a 10-point moving average (5 before and 5 after)
impute_outliers_ma10 <- function(x, is_outlier) {
  # Ensure is_outlier is a logical vector
  if (!is.logical(is_outlier)) {
    # Try to convert to logical vector
    is_outlier <- as.logical(is_outlier)
  }
  
  # If it's still not logical after conversion, throw a clear error
  if (!is.logical(is_outlier)) {
    stop("is_outlier must be a logical vector (TRUE/FALSE)")
  }
  
  # Ensure x and is_outlier have the same length
  if (length(x) != length(is_outlier)) {
    stop("x and is_outlier must have the same length")
  }
  
  # Store original values
  result <- x
  
  # If there are no outliers, return the original data
  if (!any(is_outlier, na.rm = TRUE)) {
    return(result)
  }
  
  # Set outliers to NA for calculation purposes
  x_clean <- x
  x_clean[is_outlier] <- NA
  
  # Calculate replacement values for each outlier
  n <- length(x)
  window_size <- 11
  half_window <- floor(window_size/2)
  
  # For each outlier, calculate the mean of surrounding points
  outlier_indices <- which(is_outlier)
  for (i in outlier_indices) {
    # Define window boundaries
    start_idx <- max(1, i - half_window)
    end_idx <- min(n, i + half_window)
    
    # Get values in window (excluding the central point)
    window_values <- x_clean[start_idx:end_idx]
    if (i >= start_idx && i <= end_idx) {
      # Find and exclude the center point position within the window
      center_pos <- i - start_idx + 1
      if (center_pos >= 1 && center_pos <= length(window_values)) {
        window_values <- window_values[-center_pos]
      }
    }
    
    # Calculate mean of non-NA values
    mean_val <- mean(window_values, na.rm = TRUE)
    
    # Replace outlier with mean if calculable
    if (!is.na(mean_val) && !is.nan(mean_val)) {
      result[i] <- mean_val
    }
  }
  
  return(result)
}

clean_outlier_indoor_temp <- detect_outlier_indoor_temp %>%
  group_by(id) %>%  
  mutate(
    temp_clean = impute_outliers_ma10(temp, check_outlier_temp),
    hr_clean = impute_outliers_ma10(hr, check_outlier_hr)
  ) %>%
  ungroup()

clean_outlier_indoor_both <- clean_outlier_indoor_temp %>%
  select(id_bisc, week, hr, temp, dew, time, time_d, time_h, hr_clean, temp_clean)
save(clean_outlier_indoor_both, file = "output/Clean_indoor_temp/clean_outlier_indoor_both.RData")

#############-----------------------------------------------------###############
#############        keep the data at least 20 hour/day           ###############
#############-----------------------------------------------------###############
# Identify complete days

complete_outliers_indoor_value <- clean_outlier_indoor_both %>%
  group_by(id_bisc, week, time_d) %>%
  filter(n() >= 120) %>%
  ungroup()

save(complete_outliers_indoor_value, file = "output/Clean_indoor_temp/complete_outliers_indoor_value.RData")


#############-----------------------------------------------------###############
#############             calculate daily indoor temp/hr          ###############
#############-----------------------------------------------------###############
rm(list = ls())
load("output/Clean_indoor_temp/complete_outliers_indoor_value.RData")
load("input/temperature/Joan_new_data/BiSC_Temp_Hum_Data.RData")
# calculate daily indoor temperature and humidity for participants
data_day <- complete_outliers_indoor_value %>% 
  group_by(id_bisc, week, time_d) %>%
  summarise(hr_mean = mean(hr_clean), temp_mean = mean(temp_clean), dew_mean = mean(dew))
# modify the public variable's name, let two dataset can be merged
data_day <- data_day %>%
  rename(subject_id = id_bisc,
         date = time_d)
# merged the dataset based on the indoor logger dataset (will be used to train the model)
train_indoortemp <- merge(BiSC_Temp_Hum_Data, data_day, by = c("subject_id","date"))

# the last date in outdoor temp means the delivery date

train_indoortemp <- train_indoortemp %>% 
  drop_na(temperature_mean_home)

# based on the date to creat a new variable season
train_indoortemp$season <- as.factor(train_indoortemp$season)
# assess the correlation coefficient between indoor and outdoor

cor.test(train_indoortemp$model_temp_home,train_indoortemp$temp_mean, use = "complete.obs", method = "pearson")
# Model mean temerpature and indoor temp: 0.9159912 (0.9127339,0.9191320)
cor.test(train_indoortemp$temperature_mean_home,train_indoortemp$temp_mean, use = "complete.obs", method = "pearson")
# Joan's mean temeprtaure and indoor temp : 0.9147963 (0.9114949,0.9179799)

train_indoortemp %>%
  group_by(season) %>%
  summarize(cor_temp=cor(model_temp_home,temp_mean, use = "complete.obs", method = "pearson"))
# Model mean temerpature and indoor temp
#    season       cor
#    Spring      0.679
#    Summer      0.745
#    Autumn      0.879
#    Winter      0.315

train_indoortemp %>%
  group_by(season) %>%
  summarize(cor_temp=cor(temperature_mean_home,temp_mean, use = "complete.obs", method = "pearson"))
# Joan's mean temeprtaure and indoor temp
#    season       cor
#    Spring      0.684
#    Summer      0.742
#    Autumn      0.878
#    Winter      0.315


train_indoortemp <- train_indoortemp %>%
  rename(indoor_hr = hr_mean,
         indoor_temp = temp_mean,
         indoor_dew_temp = dew_mean,
         id_mother = subject_id)

# correlation fro humidity
cor.test(train_indoortemp$humidity_mean_home,train_indoortemp$indoor_hr, use = "complete.obs", method = "pearson")
# Joan's mean humidty and indoor hum : 0.50732 (0.4924152,0.5219274)


train_indoortemp %>%
  group_by(season) %>%
  summarize(cor_hum=cor(humidity_mean_home,indoor_hr, use = "complete.obs", method = "pearson"))
# Joan's mean temeprtaure and indoor temp
#    season       cor
#    Spring      0.478
#    Summer      0.581
#    Autumn      0.607
#    Winter      0.383

save(train_indoortemp, file = "output/temperature/indoordata_train.RData")
length(unique(train_indoortemp$id_mother)) #978


#############-----------------------------------------------------###############
#############                description indoor temp/hr           ###############
#############-----------------------------------------------------###############
load("output/temperature/indoordata_train.RData")

describe_stats <- function(x) {
  if (!is.numeric(x)) {
    stop("Input must be numeric.")
  }
  result <- c(
    Min = min(x, na.rm = TRUE),
    Q1 = quantile(x, 0.25, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Mean = mean(x, na.rm = TRUE),
    Q3 = quantile(x, 0.75, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    IQR = IQR(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE)
  )
  return(round(result, 2))
}

describe_by_season <- function(data, value_var, season_var) {
  data %>%
    group_by(.data[[season_var]]) %>%
    summarise(
      Min = round(min(.data[[value_var]], na.rm = TRUE), 2),
      Q1 = round(quantile(.data[[value_var]], 0.25, na.rm = TRUE), 2),
      Median = round(median(.data[[value_var]], na.rm = TRUE), 2),
      Mean = round(mean(.data[[value_var]], na.rm = TRUE), 2),
      Q3 = round(quantile(.data[[value_var]], 0.75, na.rm = TRUE), 2),
      Max = round(max(.data[[value_var]], na.rm = TRUE), 2),
      IQR = round(IQR(.data[[value_var]], na.rm = TRUE), 2),
      SD = round(sd(.data[[value_var]], na.rm = TRUE), 2)
    ) %>%
    arrange(.data[[season_var]])
}

describe_stats(train_indoortemp$indoor_temp)
describe_stats(train_indoortemp$indoor_hr)

describe_by_season(train_indoortemp, value_var = "indoor_temp", season_var = "season")
describe_by_season(train_indoortemp, value_var = "indoor_hr", season_var = "season")
