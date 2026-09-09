# Prepare predictors for the random-forest indoor-temperature model.
#
# Workflow:
# 1. Add recent outdoor-temperature summaries before and during pregnancy.
# 2. Merge meteorological, questionnaire, energy, and indoor-temperature data.
# 3. Create model-ready datasets and check predictor correlations and VIF.
# 4. Complete the questionnaire predictors and save the final model datasets.
#
# Run this script from the repository root. Raw data belongs in db/input and
# generated databases belong in db/output.
rm(list = ls())
setwd("db")

############-----------------------------------------------------##################
#         Merge the databases of predictors with indoor temperature data          #
############-----------------------------------------------------##################
library(dplyr)
library(tidyverse)
library(lubridate)
library(slider)

#############-----------------------------------------------------###############
#############       mean previous 1/3 days outdoor temp           ###############
#############-----------------------------------------------------###############
load("output/Predict/questionnaire_meteo_predictors_adjust.RData")
load("input/GIS_team/Joan_group_data/Previous_3ds/previous_3d_data.RData")
load("input/temperature/Joan_new_data/BiSC_Temp_Hum_daily_20250523.RData")

# combine the rows that the value before conception date
Joan_Temp_Hum_home <- Joan_Temp_Hum_all %>%
  select(subject_id, place_id_home, date, temperature_min_home, temperature_mean_home,
         temperature_max_home, humidity_mean_home) %>%
  mutate(Mark = "pregnancy") %>%
  rename(id_mother = subject_id)

# keep the variable name consistent
previous_3d_data <- previous_3d_data %>%
  rename(place_id_home = place_id,
         temperature_min_home = temperature_min,
         temperature_max_home = temperature_max,
         temperature_mean_home = temperature_mean,
         humidity_mean_home = humidity_mean,
         id_mother = subject_id) %>%
  mutate(Mark = "before_preg")

Previous_data <- rbind(Joan_Temp_Hum_home,previous_3d_data)

# calculate the new variables: mean outdoor temperature of precede one and three days
Previous_data <- Previous_data %>%
  arrange(id_mother, date) %>%
  group_by(id_mother) %>%
  mutate(
    temp_mean_lag1_avg = slide_dbl(temperature_mean_home, mean, .before = 1, .after = -1, .complete = TRUE),
    temp_mean_lag3_avg = slide_dbl(temperature_mean_home, mean, .before = 3, .after = -1, .complete = TRUE),
    
    hum_mean_lag1_avg  = slide_dbl(humidity_mean_home, mean, .before = 1, .after = -1, .complete = TRUE),
    hum_mean_lag3_avg  = slide_dbl(humidity_mean_home, mean, .before = 3, .after = -1, .complete = TRUE)
  ) %>%
  ungroup()

# remove the date (before conception date) and combine predictors
Previous_data <- Previous_data %>%
  subset(Mark == "pregnancy") %>%
  select(-Mark) %>%
  select(id_mother, date, temp_mean_lag1_avg, temp_mean_lag3_avg, hum_mean_lag1_avg, hum_mean_lag3_avg)

questionnaire_meteo_predictors <- left_join(questionnaire_meteo_predictors_adjust, Previous_data, by = c("id_mother","date"))
save(questionnaire_meteo_predictors, file = "output/Predict/new/questionnaire_meteo_predictors.RData")


#############-----------------------------------------------------###############
#############         combine with indoor_temperature_data        ###############
#############-----------------------------------------------------###############
rm(list = ls())
load("output/temperature/indoordata_train.RData")
load("output/Predict/new/questionnaire_meteo_predictors.RData")
load("output/Predict/Barcelona_open_data.RData")
names(Barcelona_open_data)[2] <- "id_mother"

#################################################################################
##### prepare the model database (indoor temperature combine with predictors)
train_indoortemp <- train_indoortemp %>%
  select(c("id_mother", "date", "day_of_week", "week", "indoor_hr", "indoor_temp", "indoor_dew_temp"))

temp_preidctors_data <- full_join(questionnaire_meteo_predictors,train_indoortemp,
                                  by=c("id_mother","date"))

# temp_preidctors_data  <- temp_preidctors_data [c("id_mother", "date", "day_of_week", "gid")]
# participant_date <- temp_preidctors_data
# save(participant_date, file = "output/temperature/participant_date.RData")


############-----------------------------------------------------###############
#############         combine with Barcelona_open_data            ###############
#############-----------------------------------------------------###############
temp_preidctors_data <- left_join(temp_preidctors_data, Barcelona_open_data,
                                  by=c("gid","id_mother"))

## because the average building data is from 2021 we should minus the years between the year of date to 2021
temp_preidctors_data <- temp_preidctors_data %>%
  mutate(Average_building = Average_building - (2021-year))


#############-----------------------------------------------------###############
#############             Catalonia Energy data buffer            ############### 
#############-----------------------------------------------------###############
load("input/Barcelona_open_data/Energy_participants_50m.RData")
load("input/Barcelona_open_data/Energy_participants_100m.RData")
load("input/Barcelona_open_data/Energy_participants_300m.RData")
load("input/Barcelona_open_data/Energy_participants_500m.RData")


# Transfer the sf data to data frame
library(sf)

energy_data_50m <- energy_data_50m %>%
  st_drop_geometry()
energy_data_100m <- energy_data_100m %>%
  st_drop_geometry()
energy_data_300m <- energy_data_300m %>%
  st_drop_geometry()
energy_data_500m <- energy_data_500m %>%
  st_drop_geometry()

## combine with previous databases
temp_preidctors_data <- temp_preidctors_data %>%
  left_join(energy_data_50m, by="gid") %>%
  left_join(energy_data_100m, by="gid") %>%
  left_join(energy_data_300m, by="gid") %>%
  left_join(energy_data_500m, by="gid")

# create the new variable: day of week based on date (1 = Monday, 7 = Sunday) 
temp_preidctors_data$day_of_week <- wday(temp_preidctors_data$date, week_start = 1)  # Monday = 1

temp_preidctors_data <- temp_preidctors_data[
  , !names(temp_preidctors_data) %in% c("subject_id")
]

date_seq <- seq.Date(from = as.Date("2018-01-01"), to = as.Date("2021-12-31"), by = "day")
num_seq <- seq(1,length(date_seq))
day <- data.frame(date=date_seq, day=num_seq)
temp_preidctors_data <- left_join(temp_preidctors_data,day,by="date") %>%
  mutate(season = case_when(
    month(date) %in% c(12,1,2) ~ "Winter",
    month(date) %in% c(3,4,5) ~ "Spring",
    month(date) %in% c(6,7,8) ~ "Summer",
    month(date) %in% c(9,10,11) ~ "Autumn",
  )) %>%
  mutate(month_day = format(date, "%m-%d"),
         season_2 = case_when(
           month_day >= "05-15" & month_day <= "10-15" ~ "warm_season",
           TRUE ~ "cool_season"
         ))

temp_preidctors_data$yday <- as.POSIXlt(temp_preidctors_data$date)$yday + 1 

#############-----------------------------------------------------###############
#############        divide the train and predict database        ###############
#############-----------------------------------------------------###############
temp_preidctors_data_model <- temp_preidctors_data %>%
  subset(!is.na(indoor_temp))
temp_preidctors_data_remain <- temp_preidctors_data%>%
  subset(is.na(indoor_temp))

save(temp_preidctors_data_model, file = "output/Model/temp_preidctors_data_model.RData")
save(temp_preidctors_data_remain, file = "output/Model/temp_preidctors_data_remain.RData")
#################################################################################
# End
