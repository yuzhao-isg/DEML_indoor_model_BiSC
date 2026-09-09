# Prepare daily meteorological predictors for the indoor temperature models.
#
# Workflow:
# 1. Combine outdoor temperature with GIS home characteristics.
# 2. Build a complete daily record for the Barcelona weather stations.
# 3. Impute missing humidity, radiation, pressure, wind, and precipitation data.
# 4. Assign each participant to the nearest monitoring station.
# 5. Merge the meteorological variables with participant-level outdoor data.
#
# Run this script from the repository root. Place source data in db/input and
# generated databases in db/output.
rm(list = ls())
library(tidyverse)
library(dplyr)
library(sf)
library(lubridate)
setwd("db")
##################################################################################
#####                merge NDVI with outdoor temperature                  ########
##################################################################################

bisc_airTemperature_daily_20240227 <- read_csv("input/outdoor_temperature/gis_results/bisc_airTemperature_daily_20240227.csv")
load("input/temperature/Joan_new_data/BiSC_Temp_Hum_daily_20250523.RData")
# input NDVI without weighted value
bisc_home_ndvi <- read_csv("input/GIS_team/20231009_092551_biscHome_ndvi.csv")
bisc_home_canopy <- read_csv("input/GIS_team/20231004_165612_biscHome_canopy.csv")
bisc_home_grey <- read_csv("input/GIS_team/20240312_103910_biscHome_grey.csv")

names(bisc_home_ndvi)[7:10] <- c("ndvi_50","ndvi_100","ndvi_300","ndvi_500")
names(bisc_home_canopy)[7:10] <- c("canopy_50","canopy_100","canopy_300","canopy_500")
names(bisc_home_grey)[7:10] <- c("grey_50","grey_100","grey_300","grey_500")

class(bisc_home_canopy$period_start)

# change the period_start and period_end to Date 
bisc_home_canopy <- bisc_home_canopy %>%
  mutate(period_start = as.Date(period_start, format = "%d/%m/%Y")) %>%
  mutate(period_end = as.Date(period_end, format = "%d/%m/%Y")) %>%
  select(-c("period_start","period_end"))

bisc_home_grey <- bisc_home_grey %>%
  mutate(period_start = as.Date(period_start, format = "%d/%m/%Y")) %>%
  mutate(period_end = as.Date(period_end, format = "%d/%m/%Y"))%>%
  select(-c("period_start","period_end"))

bisc_home_ndvi_canopy <- full_join(bisc_home_ndvi,bisc_home_canopy,
                               by=c("gid","subject_id","period_ids","place_id")) # important
## I noticed that here are more than 200 participants the conception date and delivery date are different between three gis documents
## after checking, docs of canopy and grey are same, ndvi and yu_bisc_24_010_2024_09_12 are same
## the conception date and delivery date based on the ndvi
bisc_home_ndvi_canopy_grey <- full_join(bisc_home_ndvi_canopy,bisc_home_grey,
                                   by=c("gid","subject_id","period_ids","place_id"))

# split one row to several rows bansed on one variables
names(bisc_home_ndvi_canopy_grey)[3] <- "period_id"
bisc_home_ndvi_canopy_grey_split <- bisc_home_ndvi_canopy_grey %>%
  mutate(period_ids = period_id) %>%
  as_tibble() %>%
  separate_rows(period_id, sep = ", ")

# keep the class of period_id is consistent with bisc_airTemperature_daily_20240227
bisc_home_ndvi_canopy_grey_split$period_id <- as.numeric(bisc_home_ndvi_canopy_grey_split$period_id)

# merge the database based on subject_id and period_id
# for 10019711 and 10024211 period_id = 1
period_id <- bisc_airTemperature_daily_20240227 %>% select(subject_id,period_id,date) 

Joan_Temp_Hum_all_new <- left_join(Joan_Temp_Hum_all, period_id, by = c("subject_id","date")) %>%
  mutate(period_id = ifelse(subject_id == "10019711"|subject_id == "10024211", 1, period_id))
  
bisc_outdoorTemp_ndvi_canopy_grey_daily <- full_join(Joan_Temp_Hum_all_new, bisc_home_ndvi_canopy_grey_split,
                                         by = c('subject_id', 'period_id'))
save(bisc_outdoorTemp_ndvi_canopy_grey_daily, file = "output/Predict/OutdoorTemp_ndvi_canopy_grey.RData")
####################################################################################
##########################
### --- Read data --- ###
########################

load("input/temperature/Meteo.RData")
load("input/temperature/Meteo_AEMET.RData")
########################
#### process the database for multiple imputation (impute the missing value)
### create the full cases for 8 stations, daily date from 2018-0101 to 2021-12-31
# Create a sequence of dates from 2018-01-01 to 2021-12-31
date_seq <- seq.Date(from = as.Date("2018-01-01"), to = as.Date("2021-12-31"), by = "day")
# prapre the station just for Barcelona
xema_stations <- read_csv("input/outdoor_temperature/outdtem/data/meteo/xema_stations.csv") %>%
  rename(ID = CODI_ESTACIO) %>%
  dplyr::select(ID, LATITUD, LONGITUD) %>%
  dplyr::filter(ID %in% c("D5","X2","WU","X4","X8","DO","XC", "WZ","UQ","XJ","XV",
                          "U3","XE","DK","X7","UE","D3","UG","U6","D9","XL")) # Station in BNC "Y9","DH","VT","CF","CO","KX"and "AN" without data)
pre_station_date <- expand.grid(ID = xema_stations$ID,date = date_seq)
pre_station_date <- merge(pre_station_date, xema_stations, by="ID")
Meteo_xema <- full_join(pre_station_date,meteo_all, by=c("ID","date","LATITUD","LONGITUD"))
data_daily$rhmin <- NA
data_daily$rhmax <- NA
data_daily$apavg <- NA
data_daily$apmin <- NA
data_daily$apmax <- NA
data_daily$sr <- NA
data_daily$wind_direction_10m <- NA
data_daily$wind_speed_6m <- NA
data_daily$wind_direction_6m <- NA
data_daily$wind_speed_2m <- NA
data_daily$wind_direction_2m <- NA

data_daily[,c("rhmin","rhmax","apavg", "apmin", "apmax","sr","wind_direction_10m",
              "wind_speed_6m","wind_direction_6m","wind_speed_2m","wind_direction_2m")] <-
  lapply(data_daily[,c("rhmin","rhmax","apavg", "apmin", "apmax","sr",
                        "wind_direction_10m","wind_speed_6m","wind_direction_6m",
                        "wind_speed_2m","wind_direction_2m")],as.numeric) 
Meteo_xema_aemet <- rbind(Meteo_xema,data_daily)
save(Meteo_xema_aemet, file = "output/Clean/Meteo_xema_aemet.RData")
########################### impute for the missing value ######################
## 6 stations around BNC with case 8430 from 2018-01-01 to 2021-12-31, it should be 8766 case (365*3+366)*6=8766
load("output/Clean/Meteo_xema_aemet.RData")
# install.packages("imputeTS")
library(imputeTS)
# Function to impute missing values for a single time series (station)
Meteo_xema_aemet$date <- as.Date(Meteo_xema_aemet$date,"%Y%m%d")
Meteo_xema_aemet <- arrange(Meteo_xema_aemet,date)

#### check the missing value for each varables
df_summary <- Meteo_xema_aemet %>%
  group_by(ID) %>%
  summarise(
    count_missing_ap = sum(is.na(apavg)),   # Counts missing values
    count_missing_rh = sum(is.na(rhavg)),
    count_missing_sr = sum(is.na(sr)),
    count_missing_wind_speed_10m = sum(is.na(wind_speed_10m)),
    count_missing_wind_direction_10m = sum(is.na(wind_direction_10m)),
    count_missing_precipitation = sum(is.na(precipitation))
  )
write.csv(df_summary, "../results/imputation/meteo_missing.csv", row.names = FALSE)


##########################   Impute for humidity    #######################
set.seed(123)
Meteo_imputed_RH <- Meteo_xema_aemet %>%
  group_by(ID) %>%
  mutate(rhavg = na_kalman(rhavg))

########################## impute for solar radiation #######################
######### station "0201D" AND "X2" without this data
set.seed(123)
Meteo_imputed_SR <- Meteo_xema_aemet %>%
  filter(ID!="0201D"&ID!="0200E"&ID!="X2"&ID!="UE") %>%
  group_by(ID) %>% 
  mutate(sr = na_kalman(sr))

#######################  Impute for atmosphere pressure  #######################
set.seed(123)
Meteo_imputed_ap <- Meteo_xema_aemet %>%
  filter(ID!="0201D"&ID!="0200E"&ID!="KX"&ID!="U3"&ID!="U6"&ID!="UE"&ID!="UG"&ID!="UQ"&
           ID!="X2"&ID!="X7"&ID!="XL"&ID!="XV") %>% # filter the station without any data
  group_by(ID) %>% 
  mutate(apavg = na_kalman(apavg))

#######################  Impute for wind_speed_10m  #######################
set.seed(123)
Meteo_imputed_wind_speed_10m <- Meteo_xema_aemet %>%
  filter(ID!="KX"&ID!="U3"&ID!="U6"&ID!="UE"&ID!="UG"&ID!="UQ"&
           ID!="WU"&ID!="X2"&ID!="X7") %>% # filter the station without any data
  group_by(ID) %>% 
  mutate(wind_speed_10m = na_kalman(wind_speed_10m))


#######################  Impute for wind_direction_10m  #######################
set.seed(123)
Meteo_imputed_wind_direction_10m <- Meteo_xema_aemet %>%
  filter(ID!="0201D"&ID!="0200E"&ID!="KX"&ID!="U3"&ID!="U6"&ID!="UE"&ID!="UG"&ID!="UQ"&
           ID!="WU"&ID!="X2"&ID!="X7") %>% # filter the station without anyone data
  group_by(ID) %>% 
  mutate(wind_direction_10m = na_kalman(wind_direction_10m))

##########################  Impute for precipitation  ##########################
set.seed(123)
Meteo_imputed_precipitation <- Meteo_xema_aemet %>%
  filter(ID!="X2") %>% # filter the station without anyone data
  group_by(ID) %>% 
  mutate(precipitation = na_kalman(precipitation))

################### Function to compute summary statistic #####################
summary_impute <- function(df,var){
  df %>%
    group_by(ID) %>%
    summarise(
      mean = mean({{var}}, na.rm = TRUE),
      sd = sd({{var}}, na.rm = TRUE),
      median = median({{var}}, na.rm = TRUE)
    )
}

summary_before_rh <- summary_impute(Meteo_xema_aemet,rhavg)
summary_after_rh <- summary_impute(Meteo_imputed_RH,rhavg)
summary_before_sr <- summary_impute(Meteo_xema_aemet,sr)
summary_after_sr <- summary_impute(Meteo_imputed_SR,sr)
summary_before_ap <- summary_impute(Meteo_xema_aemet,apavg)
summary_after_ap <- summary_impute(Meteo_imputed_ap,apavg)
summary_before_precipitation <- summary_impute(Meteo_xema_aemet,precipitation)
summary_after_precipitation <- summary_impute(Meteo_imputed_precipitation,precipitation)

########### ggplot_na_imputations is used to visualize the original and imputed values together
###### create the plot for each station
# Define a function to plot data for a specific station
plot_station_comparison <- function(station_name) {
  # Filter data for the specified station
  data_station <- Meteo_xema_aemet %>% filter(ID == station_name)
  data_imputed_station <- Meteo_imputed_RH %>% filter(ID == station_name)
  
  # Plot original and imputed data
  ggplot_na_imputations(
    data_station$rhavg,
    data_imputed_station$rhavg
  ) +
    ggtitle(paste("Imputation Comparison for", station_name))+
    xlab("Time") +
    ylab("Humidity")
}
plot_station_comparison("X2")

######### THE PLOT FOR ALL STATION
##############################    relative humidity    #########################
RH_imputation <- ggplot_na_imputations(
     Meteo_xema_aemet$rhavg,
     Meteo_imputed_RH$rhavg
 ) +
     ggtitle("Imputation Comparison") +
     xlab("Time") +
     ylab("Humidity")
pdf(file = "../results/imputation/RH_imputation.pdf")
print(RH_imputation)
dev.off()

################################################################################
# new plot function

plot_imputation <- function(original_data, imputed_data, var_name, y_label) {
  total_points <- nrow(imputed_data)
  imputed_points <- sum(is.na(original_data[[var_name]]))
  imputed_percent <- round(imputed_points / total_points * 100, 1)
  
  plot_data <- imputed_data %>%
    left_join(
      original_data %>% select(date, !!paste0(var_name, "_original") := !!sym(var_name)),
      by = "date"
    ) %>%
    mutate(
      type = ifelse(is.na(!!sym(paste0(var_name, "_original"))), "Imputed", "Observed")
    )
  
  daily_plot <- plot_data %>%
    group_by(date, type) %>%
    summarise(value = mean(!!sym(var_name), na.rm = TRUE)) %>%
    ungroup()
  
  p <- ggplot() +

    geom_line(data = daily_plot %>% filter(type=="Observed"),
              aes(x = date, y = value), color = "grey70", size = 0.8) +

    geom_point(data = daily_plot %>% filter(type=="Observed"),
               aes(x = date, y = value, color = "Observed"), size = 1.5) +

    geom_point(data = daily_plot %>% filter(type=="Imputed"),
               aes(x = date, y = value, color = "Imputed"), size = 2.5, shape = 17) +
    scale_color_manual(values = c("Observed" = "#1F77B4", "Imputed" = "#D62728")) +
    labs(
      title = paste0("Time Series of Observed and Imputed ", y_label, "\n(Imputed values: ", imputed_percent, "%)"),
      x = "Calendar Date",
      y = y_label,
      color = "Data Type"
    ) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )
  
  return(p)
}

##############################     solar radiation     #########################
Meteo_xema_aemet_SR <- Meteo_xema_aemet %>%
  filter(ID!="0201D"&ID!="0200E"&ID!="X2"&ID!="UE"&ID!="KX") 

SR_imputation <- plot_imputation(
  Meteo_xema_aemet_SR,
  Meteo_imputed_SR,
  var_name = "sr",
  y_label = "Solar Radiation (W/m2)"
)

ggsave("../results/imputation/SR_imputation.png",
       plot = SR_imputation, width = 12, height = 8, dpi = 600)



#############################     wind_speed_10m     ############################
Meteo_xema_aemet_wind_speed_10m <- Meteo_xema_aemet %>%
  filter(ID!="KX"&ID!="U3"&ID!="U6"&ID!="UE"&ID!="UG"&ID!="UQ"&
           ID!="WU"&ID!="X2"&ID!="X7")
wind_speed_10m_imputation <- plot_imputation(
  Meteo_xema_aemet_wind_speed_10m,
  Meteo_imputed_wind_speed_10m,
  var_name = "wind_speed_10m",
  y_label = "Wind Speed (m/s)"
)
ggsave("../results/imputation/wind_speed_10m_imputation.png",
       plot = wind_speed_10m_imputation, width = 12, height = 8, dpi = 600)

###########################     wind_direction_10m     ##########################
Meteo_xema_aemet_wind_direction_10m <- Meteo_xema_aemet %>%
  filter(ID!="0201D"&ID!="0200E"&ID!="KX"&ID!="U3"&ID!="U6"&ID!="UE"&ID!="UG"&ID!="UQ"&
           ID!="WU"&ID!="X2"&ID!="X7")

wind_direction_10m_imputation <- plot_imputation(
  Meteo_xema_aemet_wind_direction_10m,
  Meteo_imputed_wind_direction_10m,
  var_name = "wind_direction_10m",
  y_label = "Wind Direction (clockwise degrees from the north direction)"
)
ggsave("../results/imputation/wind_direction_10m_imputation.png",
       plot = wind_direction_10m_imputation, width = 12, height = 8, dpi = 600)

#############################     precipitation     ############################
Meteo_xema_aemet_precipitation <- Meteo_xema_aemet %>%
  filter(ID!="X2") 

precipitation_imputation <- plot_imputation(
  Meteo_xema_aemet_precipitation,
  Meteo_imputed_precipitation,
  var_name = "precipitation",
  y_label = "Precipitation (mm/h)"
)
ggsave("../results/imputation/precipitation_imputation.png",
       plot = precipitation_imputation, width = 12, height = 8, dpi = 600)


####################### save the imputed database separatly for humidity and solar radiation
save(Meteo_imputed_RH, file = "output/impute/Meteo_imputed_RH.RData")
save(Meteo_imputed_SR, file = "output/impute/Meteo_imputed_SR.RData")
save(Meteo_imputed_ap, file = "output/impute/Meteo_imputed_ap.RData")
save(Meteo_imputed_wind_speed_10m, file = "output/impute/Meteo_imputed_wind_speed_10m.RData")
save(Meteo_imputed_wind_direction_10m, file = "output/impute/Meteo_imputed_wind_direction_10m.RData")
save(Meteo_imputed_precipitation, file = "output/impute/Meteo_imputed_precipitation.RData")


# for simplicity we rename the datasets (you could use the same name)
rm(list = ls())
load("output/impute/Meteo_imputed_RH.RData")
load("output/impute/Meteo_imputed_SR.RData")
load("output/impute/Meteo_imputed_ap.RData")
load("output/impute/Meteo_imputed_wind_speed_10m.RData")
load("output/impute/Meteo_imputed_wind_direction_10m.RData")
load("output/impute/Meteo_imputed_precipitation.RData")
load("input/temperature/participant_coords.RData")
load("output/Predict/OutdoorTemp_ndvi_canopy_grey.RData")

humidity <- Meteo_imputed_RH
solar_radiation <- Meteo_imputed_SR
atmosphere_pressure <- Meteo_imputed_ap
wind_speed_10m <- Meteo_imputed_wind_speed_10m
wind_direction_10m <- Meteo_imputed_wind_direction_10m
precipitation <- Meteo_imputed_precipitation
participants <- participant
outdoor_temp_ndvi_daily <- bisc_outdoorTemp_ndvi_canopy_grey_daily

remove(participant, Meteo_imputed_RH, Meteo_imputed_SR, Meteo_imputed_ap,
       Meteo_imputed_wind_speed_10m, Meteo_imputed_wind_direction_10m, Meteo_imputed_precipitation)

# check the data 
dplyr::glimpse(humidity)
dplyr::glimpse(solar_radiation)
dplyr::glimpse(participants)
dplyr::glimpse(outdoor_temp_ndvi_daily)

#####################################################
### --- calculate nearest monitoring station --- ###
###################################################

# Convert participants and monitoring stations to spatial points
participants_sf <- sf::st_as_sf(participants, coords = c("longitude", "latitude"), crs = 4326)
stations_sf_rh <- sf::st_as_sf(humidity, coords = c("LONGITUD", "LATITUD"), crs = 4326)
stations_sf_sr <- sf::st_as_sf(solar_radiation, coords = c("LONGITUD", "LATITUD"), crs = 4326)
stations_sf_ap <- sf::st_as_sf(atmosphere_pressure, coords = c("LONGITUD", "LATITUD"), crs = 4326)
stations_sf_wind_speed_10m <- sf::st_as_sf(wind_speed_10m, coords = c("LONGITUD", "LATITUD"), crs = 4326)
stations_sf_wind_direction_10m <- sf::st_as_sf(wind_direction_10m, coords = c("LONGITUD", "LATITUD"), crs = 4326)
stations_sf_precipitation <- sf::st_as_sf(precipitation, coords = c("LONGITUD", "LATITUD"), crs = 4326)


# Find the nearest station for each participant
nearest_stations_rh <- sf::st_nearest_feature(participants_sf, stations_sf_rh)
nearest_stations_sr <- sf::st_nearest_feature(participants_sf, stations_sf_sr)
nearest_stations_ap <- sf::st_nearest_feature(participants_sf, stations_sf_ap)
nearest_stations_wind_speed_10m <- sf::st_nearest_feature(participants_sf, stations_sf_wind_speed_10m)
nearest_stations_wind_direction_10m <- sf::st_nearest_feature(participants_sf, stations_sf_wind_direction_10m)
nearest_stations_precipitation <- sf::st_nearest_feature(participants_sf, stations_sf_precipitation)


participants$nearest_station_id_rh <- stations_sf_rh$ID[nearest_stations_rh]
participants$nearest_station_id_sr <- stations_sf_sr$ID[nearest_stations_sr]
participants$nearest_station_id_ap <- stations_sf_ap$ID[nearest_stations_ap]
participants$nearest_station_id_wind_speed_10m <- stations_sf_wind_speed_10m$ID[nearest_stations_wind_speed_10m]
participants$nearest_station_id_wind_direction_10m <- stations_sf_wind_direction_10m$ID[nearest_stations_wind_direction_10m]
participants$nearest_station_id_precipitation <- stations_sf_precipitation$ID[nearest_stations_precipitation]
################################################################################
### --- extract the average relative humidity per each day of pregnancy --- ###
##############################################################################

# convert date columns to Date class
humidity$date <- as.Date(humidity$date)
solar_radiation$date <- as.Date(solar_radiation$date)
atmosphere_pressure$date <- as.Date(atmosphere_pressure$date)
wind_speed_10m$date <- as.Date(wind_speed_10m$date)
wind_direction_10m$date <- as.Date(wind_direction_10m$date)
precipitation$date <- as.Date(precipitation$date)
outdoor_temp_ndvi_daily$date <- as.Date(outdoor_temp_ndvi_daily$date)

# join the datasets  to include the nearest monitoring station
outdoor_temp_ndvi_daily <- outdoor_temp_ndvi_daily %>%
                      dplyr::left_join(participants %>% 
                      dplyr::select(subject_id, gid,nearest_station_id_rh,nearest_station_id_sr,
                                    nearest_station_id_ap,nearest_station_id_wind_speed_10m,
                                    nearest_station_id_wind_direction_10m,nearest_station_id_precipitation),
                      by = c("subject_id","gid"))

# Join the humidity data from the nearest station to each participant's daily date
result_rh <- outdoor_temp_ndvi_daily %>%
          dplyr::left_join(humidity %>% select(ID, date, rhavg), 
                           by = c("nearest_station_id_rh" = "ID", "date" = "date"))

result_sr <- outdoor_temp_ndvi_daily %>%
  dplyr::left_join(solar_radiation %>% select(ID, date, sr), 
                   by = c("nearest_station_id_sr" = "ID", "date" = "date"))

result_ap <- outdoor_temp_ndvi_daily %>%
  dplyr::left_join(atmosphere_pressure %>% select(ID, date, apavg), 
                   by = c("nearest_station_id_ap" = "ID", "date" = "date"))

result_wind_speed_10m <- outdoor_temp_ndvi_daily %>%
  dplyr::left_join(wind_speed_10m %>% select(ID, date, wind_speed_10m), 
                   by = c("nearest_station_id_wind_speed_10m" = "ID", "date" = "date"))

result_wind_direction_10m <- outdoor_temp_ndvi_daily %>%
  dplyr::left_join(wind_direction_10m %>% select(ID, date, wind_direction_10m), 
                   by = c("nearest_station_id_wind_direction_10m" = "ID", "date" = "date"))

result_precipitation <- outdoor_temp_ndvi_daily %>%
  dplyr::left_join(precipitation %>% select(ID, date, precipitation), 
                   by = c("nearest_station_id_precipitation" = "ID", "date" = "date"))

# select relevant columns for your analysis 

result_sr <- result_sr %>%
  dplyr::select(subject_id, gid, date, nearest_station_id_sr, 
                sr, period_id, temperature_min_home, temperature_mean_home,
                temperature_max_home, humidity_mean_home, ndvi_300)

result_ap <- result_ap %>%
  dplyr::select(subject_id, date, nearest_station_id_ap, 
                apavg)

result_wind_speed_10m <- result_wind_speed_10m %>%
  dplyr::select(subject_id, date, nearest_station_id_wind_speed_10m, 
                wind_speed_10m)

result_wind_direction_10m <- result_wind_direction_10m %>%
  dplyr::select(subject_id, date, nearest_station_id_wind_direction_10m, 
                wind_direction_10m)

result_precipitation <- result_precipitation %>%
  dplyr::select(subject_id, date, nearest_station_id_precipitation, 
                precipitation)


dplyr::glimpse(result_sr) # 297,137 same number with the outdoor temperature data (double check)

###### identify the different subject_id between "participant" and "bisc_home_ndvi"(from GIS group)
ids_only_in_bisc_home_ndvi <- bisc_home_ndvi %>%
  filter(!subject_id %in% participant$subject_id)## 12012911,12024211,12041011

# install.packages("skimr")
library(skimr)
skim(result_rh)

bisc_outdoorTemp_ndvi_meteo_daily <- left_join(result_sr,result_ap,by = c("subject_id","date")) %>%
  left_join(result_wind_speed_10m, by = c("subject_id","date")) %>%
  left_join(result_wind_direction_10m, by = c("subject_id","date")) %>%
  left_join(result_precipitation, by = c("subject_id","date")) %>%
  select(-c("nearest_station_id_sr","nearest_station_id_ap",
            "nearest_station_id_wind_speed_10m","nearest_station_id_wind_direction_10m",
            "nearest_station_id_precipitation"))
bisc_meteo_predictor <- bisc_outdoorTemp_ndvi_meteo_daily
save(bisc_meteo_predictor, file = "output/Predict/bisc_meteo_predictor.RData")
################################################################################

study_rh <- meteo_all %>%
  filter(ID %in% c("X2", "X4", "X8", "D5", "XL", "WU")) %>%
  filter(between(date, as.Date("2018-07-06"), as.Date("2021-09-28")))
summary(study_rh$rhavg)




