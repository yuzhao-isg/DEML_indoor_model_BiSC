# Process Barcelona environmental and building-energy data for the indoor
# temperature models.
#
# Workflow:
# 1. Match participant coordinates with average building age.
# 2. Join building energy-behavior and heat-vulnerability data spatially.
# 3. Summarize building-energy indicators within 50, 100, 300, and 500 m buffers.
# 4. Save participant-level environmental predictors for later model scripts.
#
# Public source data: https://opendata-ajuntament.barcelona.cat/
# Run this script from the repository root. Raw data belongs in db/input and
# generated databases belong in db/output.
rm(list = ls())
library(dplyr)
library(tidyverse)
library(sf)
setwd("db")

# Load participant coordinates.
load("input/temperature/participant_coords.RData")
participant_sf <- sf::st_as_sf(participant, coords = c("longitude", "latitude"), crs = 4326)
#############-----------------------------------------------------###############
#############              average age of building                ###############
#############-----------------------------------------------------###############
Average_age_building <- read_csv("input/Barcelona_open_data/2021_edificacions_edat_mitjana.csv")
Lat_Lon_information <- read_csv("input/Barcelona_open_data/220930_censcomercialbcn_opendata_2022_v10_mod.csv")
## because the average age of building csv document without the latitude and longitude information, I match it from other document
Lat_Lon_information <- Lat_Lon_information[c(29,30,43,44,46)]
names(Lat_Lon_information)[1:5] <- c("latitude","longitude","Seccio_censal","Codi_barri","Codi_districte")
Average_age_building_GIS <- left_join(Average_age_building, Lat_Lon_information,
                                  by=c("Seccio_censal","Codi_barri","Codi_districte"))
# delete one missing case
Average_age_building_GIS <- na.omit(Average_age_building_GIS)

Average_age_building_sf <- sf::st_as_sf(Average_age_building_GIS, coords = c("longitude", "latitude"), crs = 4326)
## match it using the nearest point
participant_aab <- st_join(participant_sf, Average_age_building_sf, join = st_nearest_feature) %>%
  select(gid, subject_id, Edat_mitjana)
participant_aab <- as.data.frame(participant_aab)

#############-----------------------------------------------------###############
#############             Energy behavior of buildings            ###############
#############-----------------------------------------------------###############
#############-----------------------------------------------------###############
#############             extract the energy value within buffer         ###############
#############-----------------------------------------------------###############
# download from: Energy behavior of buildings in the city of Barcelona. Theoretical cooling demand in the event of a heat wave episode
# https://opendata-ajuntament.barcelona.cat/data/en/dataset/comportament-energetic-edificis

# because there are limited door-door information, we finally decide to use the buffer average data to represent the energy use
rm(list = ls())
load("input/Barcelona_open_data/Energy_participants_100m_all.RData")

energy_data_100m <- joined_data_100m %>%
  group_by(gid) %>%
  summarize(across(c("METRES_CADASTRE", "Energia primària no renovable", "VALOR AILLAMENTS",
                     "VALOR FINESTRES", "Energia calefacció", "Energia refrigeració",
                     "Energia calefacció demanda", "Energia refrigeració demanda"), mean, na.rm = TRUE))

names(energy_data_100m)[2:9] <- c("METRES_CADASTRE_100m", "Energia_primària_no_renovable_100m", 
                                  "VALOR_AILLAMENTS_100m","VALOR_FINESTRES_100m", 
                                  "Energia_calefacció_100m", "Energia_refrigeració_100m",
                                  "Energia_calefacció_demanda_100m", "Energia_refrigeració_demanda_100m")

save(energy_data_100m, file = "input/Barcelona_open_data/Energy_participants_100m.RData")

rm(list = ls())
load("input/Barcelona_open_data/Energy_participants_300m_all.RData")

energy_data_300m <- joined_data_300m %>%
  group_by(gid) %>%
  summarize(across(c("METRES_CADASTRE", "Energia primària no renovable", "VALOR AILLAMENTS",
                     "VALOR FINESTRES", "Energia calefacció", "Energia refrigeració",
                     "Energia calefacció demanda", "Energia refrigeració demanda"), mean, na.rm = TRUE))

names(energy_data_300m)[2:9] <- c("METRES_CADASTRE_300m", "Energia_primària_no_renovable_300m", 
                                  "VALOR_AILLAMENTS_300m","VALOR_FINESTRES_300m", 
                                  "Energia_calefacció_300m", "Energia_refrigeració_300m",
                                  "Energia_calefacció_demanda_300m", "Energia_refrigeració_demanda_300m")

save(energy_data_300m, file = "input/Barcelona_open_data/Energy_participants_300m.RData")

rm(list = ls())
load("input/Barcelona_open_data/Energy_participants_500m_all.RData")
energy_data_500m <- joined_data_500m %>%
  group_by(gid) %>%
  summarize(across(c("METRES_CADASTRE", "Energia primària no renovable", "VALOR AILLAMENTS",
                     "VALOR FINESTRES", "Energia calefacció", "Energia refrigeració",
                     "Energia calefacció demanda", "Energia refrigeració demanda"), mean, na.rm = TRUE))

names(energy_data_500m)[2:9] <- c("METRES_CADASTRE_500m", "Energia_primària_no_renovable_500m", 
                                  "VALOR_AILLAMENTS_500m","VALOR_FINESTRES_500m", 
                                  "Energia_calefacció_500m", "Energia_refrigeració_500m",
                                  "Energia_calefacció_demanda_500m", "Energia_refrigeració_demanda_500m")

save(energy_data_500m, file = "input/Barcelona_open_data/Energy_participants_500m.RData")

## read the gpkg file: Energy behavior of buildings in the city of Barcelona. Theoretical cooling demand in the event of a heat wave episode
Energy_behavior <- st_read("input/Barcelona_open_data/2017_comp_energ_edificis.gpkg")
Energy_behavior <- st_transform(Energy_behavior, crs = 4326)
participant_eb <- st_join(participant_sf, Energy_behavior, join = st_intersects)
participant_eb <- as.data.frame(participant_eb)

#############-----------------------------------------------------###############
#############          Vulnerability factors in a heat wave       ###############
#############-----------------------------------------------------###############
Vulnerability_factors_heat <- st_read("input/Barcelona_open_data/2017_factors_vulnera.gpkg")
Vulnerability_factors_heat <- st_transform(Vulnerability_factors_heat, crs = 4326)
participant_vfh <- st_join(participant_sf, Vulnerability_factors_heat, join = st_intersects)
participant_vfh <- as.data.frame(participant_vfh)

## combine the three database
Barcelona_open_data <- participant_aab %>%
  full_join(participant_eb, by=c("gid","subject_id","geometry")) %>%
  full_join(participant_vfh, by=c("gid","subject_id","geometry"))

Barcelona_open_data <- select(Barcelona_open_data, -geometry)
names(Barcelona_open_data)[3:5] <- c("Average_building", "Energy_behavior", "Vulnerability_heat")
save(Barcelona_open_data, file = "output/Predict/Barcelona_open_data.RData")
### important the average building data is 2021, so in the later step, it should minus the year to 2021



#############-----------------------------------------------------###############
#############         Catalonia: energy efficiency of buildings   ###############
#############-----------------------------------------------------###############
Energy_efficiency <- read_csv("input/Barcelona_open_data/Certificats_d_efici_ncia_energ_tica_d_edificis.csv")

Energy_efficiency_Barcelona <- subset(Energy_efficiency,NOM_PROVINCIA == "Barcelona")
names(Energy_efficiency_Barcelona)[2:8] <- c(" Street_address","number_street_door","ESCALA",
                                              "flat_floor","flat_number","postal_code","city")

table(Energy_efficiency_Barcelona$`Qualificació de consum d'energia primaria no renovable`, useNA = 'ifany')
# A      B      C      D      E      F      G 
# 11198  15864  43234  89785 485832 119735 175832 


continuous <- c("Energia primària no renovable","METRES_CADASTRE","Consum d'energia final",
                "VALOR AILLAMENTS", "VALOR FINESTRES","Energia calefacció","Energia refrigeració",
                "Energia calefacció demanda","Energia refrigeració demanda","LATITUD",
                "LONGITUD")
summary <- data.frame(
  Variable = names(Energy_efficiency_Barcelona),
  Nonmissing = sapply(Energy_efficiency_Barcelona, function(x) sum(!is.na(x))),
  Missing = sapply(Energy_efficiency_Barcelona, function(x) sum(is.na(x)))
)
write.csv(summary, "input/Barcelona_open_data/summary_frequency.csv", row.names = FALSE)

selected_rows <- summary[c("Qualificació de consum d'energia primaria no renovable",
                           "Energia primària no renovable","METRES_CADASTRE","Consum d'energia final",
                           "VALOR AILLAMENTS", "VALOR FINESTRES","Energia calefacció","Energia refrigeració",
                           "Energia calefacció demanda","Energia refrigeració demanda","LATITUD",
                           "LONGITUD"),]

#############-----------------------------------------------------###############
#############             extract the value within buffer         ###############
#############-----------------------------------------------------###############
load("input/temperature/participant_coords.RData")

# Convert data to sf objects and set the appropriate CRS
participant_sf <- st_as_sf(participant, coords = c("longitude", "latitude"), crs = 4326)

# delete the cases with missing value of LONGITUD and LATITUD
Energy_efficiency_new <- subset(Energy_efficiency, LONGITUD != "null" & LATITUD != "null")
energy_sf <- st_as_sf(Energy_efficiency_new, coords = c("LONGITUD","LATITUD"), crs = 4326)

# Create 50m Buffers around each participant’s location
participant_buffer_50m <- st_buffer(participant_sf, dist = 50)
participant_buffer_100m <- st_buffer(participant_sf, dist = 100)
participant_buffer_300m <- st_buffer(participant_sf, dist = 300)
participant_buffer_500m <- st_buffer(participant_sf, dist = 500)

# Perform Spatial Join to find all points within each buffer
joined_data_50m <- st_join(participant_buffer_50m, energy_sf)
joined_data_100m <- st_join(participant_buffer_100m, energy_sf)
joined_data_300m <- st_join(participant_buffer_300m, energy_sf)
joined_data_500m <- st_join(participant_buffer_500m, energy_sf)
save(joined_data_50m, file = "input/Barcelona_open_data/Energy_participants_50m_all.RData")
save(joined_data_100m, file = "input/Barcelona_open_data/Energy_participants_100m_all.RData")
save(joined_data_300m, file = "input/Barcelona_open_data/Energy_participants_300m_all.RData")
save(joined_data_500m, file = "input/Barcelona_open_data/Energy_participants_500m_all.RData")

# Calculate the Average for each participant within the buffer for the interested variables
energy_data_50m <- joined_data_50m %>%
  group_by(gid) %>%
  summarize(across(c("METRES_CADASTRE", "Energia primària no renovable", "VALOR AILLAMENTS",
                     "VALOR FINESTRES", "Energia calefacció", "Energia refrigeració",
                     "Energia calefacció demanda", "Energia refrigeració demanda"), mean, na.rm = TRUE))
names(energy_data_50m)[2:9] <- c("METRES_CADASTRE_50m", "Energia_primària_no_renovable_50m", 
                                 "VALOR_AILLAMENTS_50m","VALOR_FINESTRES_50m", 
                                 "Energia_calefacció_50m", "Energia_refrigeració_50m",
                                 "Energia_calefacció_demanda_50m", "Energia_refrigeració_demanda_50m")

energy_data_100m <- joined_data_100m %>%
  group_by(gid) %>%
  summarize(across(c("METRES_CADASTRE", "Energia primària no renovable", "VALOR AILLAMENTS",
                     "VALOR FINESTRES", "Energia calefacció", "Energia refrigeració",
                     "Energia calefacció demanda", "Energia refrigeració demanda"), mean, na.rm = TRUE))

names(energy_data_100m)[2:9] <- c("METRES_CADASTRE_100m", "Energia_primària_no_renovable_100m", 
                                 "VALOR_AILLAMENTS_100m","VALOR_FINESTRES_100m", 
                                 "Energia_calefacció_100m", "Energia_refrigeració_100m",
                                 "Energia_calefacció_demanda_100m", "Energia_refrigeració_demanda_100m")

energy_data_300m <- joined_data_300m %>%
  group_by(gid) %>%
  summarize(across(c("METRES_CADASTRE", "Energia primària no renovable", "VALOR AILLAMENTS",
                     "VALOR FINESTRES", "Energia calefacció", "Energia refrigeració",
                     "Energia calefacció demanda", "Energia refrigeració demanda"), mean, na.rm = TRUE))

names(energy_data_300m)[2:9] <- c("METRES_CADASTRE_300m", "Energia_primària_no_renovable_300m", 
                                 "VALOR_AILLAMENTS_300m","VALOR_FINESTRES_300m", 
                                 "Energia_calefacció_300m", "Energia_refrigeració_300m",
                                 "Energia_calefacció_demanda_300m", "Energia_refrigeració_demanda_300m")

energy_data_500m <- joined_data_500m %>%
  group_by(gid) %>%
  summarize(across(c("METRES_CADASTRE", "Energia primària no renovable", "VALOR AILLAMENTS",
                     "VALOR FINESTRES", "Energia calefacció", "Energia refrigeració",
                     "Energia calefacció demanda", "Energia refrigeració demanda"), mean, na.rm = TRUE))

names(energy_data_500m)[2:9] <- c("METRES_CADASTRE_500m", "Energia_primària_no_renovable_500m", 
                                 "VALOR_AILLAMENTS_500m","VALOR_FINESTRES_500m", 
                                 "Energia_calefacció_500m", "Energia_refrigeració_500m",
                                 "Energia_calefacció_demanda_500m", "Energia_refrigeració_demanda_500m")

save(energy_data_50m, file = "input/Barcelona_open_data/Energy_participants_50m.RData")
save(energy_data_100m, file = "input/Barcelona_open_data/Energy_participants_100m.RData")
save(energy_data_300m, file = "input/Barcelona_open_data/Energy_participants_300m.RData")
save(energy_data_500m, file = "input/Barcelona_open_data/Energy_participants_500m.RData")





