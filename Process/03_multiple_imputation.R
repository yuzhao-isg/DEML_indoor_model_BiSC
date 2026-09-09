# Multiple imputation of the home characteristics, cooling system, and indoor
# habit databases using mice.
#
# Workflow:
# 1. Add participant-level demographic and socioeconomic predictors.
# 2. Combine the home characteristics with the cooling system questionnaire.
# 3. Impute home characteristics and cooling system variables.
# 4. Add indoor habits and impute the remaining habit variables.
# 5. Combine the imputed questionnaire data with meteorological predictors.
# 6. Set heating and cooling use according to month and outdoor temperature.
#
# Run this script from the repository root. Input data belongs in db/input and
# generated data belongs in db/output.
library(dplyr)
library(tidyverse)
rm(list = ls())

setwd("db")

#############-----------------------------------------------------###############
#############                 multiple imputation                 ###############
#############-----------------------------------------------------###############

# Add demographic and socioeconomic variables as basic predictors.
load("input/2024_10_17_Yu_Zhao_BiSC_24_010.RData")
basic_predictors <- yu_bisc_24_010_2024_10_17[,c(1, 2, 13, 18:20, 26:29)]
names(basic_predictors)[3:10] <- c("maternal_age","educational_level","employment_situation",
                                   "spouse_finance","difficult_pay_food","difficult_pay_clothes",
                                   "difficult_pay_heating","difficult_pay_rent")
basic_predictors <- basic_predictors %>%
  mutate(educational_level = recode(educational_level,"Primary or less" = 1, "Secondary" = 2,
                            "University" = 3)) %>%
  mutate(employment_situation = as.numeric(employment_situation),
         spouse_finance = as.numeric(spouse_finance),
         difficult_pay_food = as.numeric(difficult_pay_food),
         difficult_pay_clothes = as.numeric(difficult_pay_clothes),
         difficult_pay_heating = as.numeric(difficult_pay_heating),
         difficult_pay_rent = as.numeric(difficult_pay_rent))
  
load("../output/Clean/HC_clean_new.RData")
HC_impute_raw <- full_join(HC_clean_new, basic_predictors, by=c("id_child","id_mother"))
save(HC_impute_raw, file = "../output/impute/HC_impute_raw.RData")
#################################################################################
library(mice)


#############-----------------------------------------------------###############
#############                HC (include EC) database             ###############
#############-----------------------------------------------------###############


load("../output/impute/HC_impute_raw.RData")
load("../output/Clean/indoor_clean_EC.RData")
names(indoor_pre_EC)[4] <- "visiting_date_EC"
#################################################################################
####################### combine with EC database  ###############################
# for the participants who didn't change there home address, EC database (only collected at 32w) can combine directly 
load("../output/Group/group1.RData")

EC_HC_group1 <- inner_join(HC_impute_raw,group1, by = "gid") 
names(EC_HC_group1)[2:8] <- c("id_mother","period_ids","period_start","period_end","place_id","id_child","conception_date")
EC_HC_group1 <- EC_HC_group1 %>%
  select(-c("id_mother.y","period_ids.y","period_start.y","period_end.y","place_id.y",
            "id_child.y","conception_date.y","week_es_n","week_ec_n","period","group")) %>%
  left_join(indoor_pre_EC, by=c("id_mother","id_child","conception_date"))


# for the remain cases, EC database ca only combine with the case at 32w (based on the visiting_date if within the period_start and period_end)

EC_HC_group2 <- anti_join(HC_impute_raw, group1, by = "gid") %>%
  left_join(indoor_pre_EC, by=c("id_mother","id_child","conception_date"))
EC_HC_group2$within_time_range <- with(EC_HC_group2,visiting_date_EC >= period_start & visiting_date_EC <= period_end)

# test <- select(EC_HC_group2, c(1:8,256,268)) # check one_by_one
# I detected that 6 participants all the EC correspond gis within are FALSE
# id_mother:10043511,10036511,10044911,10045311,12026711,12028811
# of which 5 participants have data at EC_HC_group1, ignore it (10043511,10044911,10045311,12026711,12028811)
### !!! check the id_mother: 10036511
### manually change the value of within to TRUE for last period_ids (gid_412)
EC_HC_group2 <- EC_HC_group2 %>%
  mutate(within_time_range = ifelse(gid == 412, TRUE, within_time_range)) %>%
  mutate(across(c("EC_1_32w","EC_2_32w","EC_3_32w","EC_4_32w"), ~ if_else(within_time_range == FALSE, NA,.))) %>%
  select(-c("within_time_range"))

### let the colname are same between EC_HC_grorp1 and EC_HC_group2 
colnames(EC_HC_group2) <- colnames(EC_HC_group1)

EC_HC_raw <- rbind(EC_HC_group1, EC_HC_group2)
save(EC_HC_raw, file = "../output/impute/EC_HC_raw.RData")

##################################################################################
#############-----------------------------------------------------###############
#############           impute HC (include EC) database           ###############
#############-----------------------------------------------------###############


##variables used to predict missings (from the other variables)
features <- c("ct8l_b","ct4","h4","ct8l_g","ct9m","maternal_age","educational_level",
              "employment_situation","spouse_finance","difficult_pay_food","difficult_pay_clothes",
              "difficult_pay_heating","difficult_pay_rent","ct6_m02","n7_1","n7_2",
              "ct7_m02","ct9a","ct9g","ct9h","ct9j","ct9k","ct8b_r03","ct8b_r05",
              "heating_livingroom","EC_2_32w","GS3_m02","GS3_m03","GS3_m04",
              "r6_nv_r02","r6_nv_r03","r6_nv_r04","r6_nv_r05","r6_nv_r06")

##variables to impute
to_impute <- c("ct1","ct3_8a","ct6_m01","ct7_m01","ct8ec","ct8m","ct8l_f","h3",
               "ct8g","ct8h","ct8i","ct8j","ct8k","ct8l_c","heating_bedroom",
               "heating_livingroom","ct8b_r01","EC_3_32w","EC_4_32w","GS3_m01") 

apply(EC_HC_raw[, to_impute], 2, function(x) sum(is.na(x))) ##number of missings
apply(EC_HC_raw[, to_impute], 2, function(x) round(100*sum(is.na(x))/nrow(EC_HC_raw),2)) ## % of missings

id <- c("gid","period_start") # PK or id variable
data <- EC_HC_raw[, c(id, features, to_impute)] #subset only to 
data[, id] <- NULL

predictorMatrix <- quickpred(data, mincor = 0.2, minpuc = 0.4)
prova_HC <- mice(data,  m = 100, print = F, seed = 1234, predictorMatrix = predictorMatrix)
EC_HC_imputed <- complete(prova_HC,include = F)
str(prova_HC)
###evaluate which one database is best
EC_HC_imputed <- cbind(EC_HC_raw[, id], EC_HC_imputed)

save(prova_HC, file = "../output/impute/prova_HC.RData")

EC_HC_raw[, to_impute] <- EC_HC_imputed[, to_impute] ## replace variables by imputed values
EC_HC_raw[, features] <- EC_HC_imputed[, features] ## replace variables by features values
EC_HC_imputed <- EC_HC_raw
### for the question "EC_3_32w","EC_4_32w", if the answer is "I do not have any cooling system at home", here should answer "NO"
EC_HC_imputed <- EC_HC_imputed %>%
  mutate(EC_3_32w = ifelse(EC_2_32w==4,3,EC_3_32w)) %>%
  mutate(EC_4_32w = ifelse(EC_2_32w==4,3,EC_4_32w))

### for the question of ct8l_f (area (m2) of all the bedroom windows), the value should be 0
### if for the variable ct8b_r01 (if there are windows in the bedroom)
window_area <- subset(EC_HC_imputed, ct8b_r01==0) %>%
  select("id_mother","id_child","gid","ct8b_r01","n7_1","ct8l_f") 
EC_HC_imputed <- EC_HC_imputed %>%
  mutate(ct8l_f = ifelse(ct8b_r01==0, 0, ct8l_f))

save(EC_HC_imputed, file = "../output/impute/EC_HC_imputed.RData")

#####################################################################################


#############-----------------------------------------------------###############
#############                      habit database                 ###############
#############-----------------------------------------------------###############
rm(list = ls())
load("../output/impute/EC_HC_imputed.RData")
load("../output/Clean/habit_clean.RData")
load("../output/Clean/habit_clean_1.RData")

indoor_habit_combine <- full_join(indoor_habit_clean,indoor_habit_1_clean,
                                  by=c("id_mother","id_child","conception_date","period","delivery_date"))


# create a new variable based on period_end and conception date
EC_HC_imputed$weeks_ce <- difftime(EC_HC_imputed$period_end,EC_HC_imputed$conception_date,
                                   units = "weeks")
EC_HC_imputed$weeks_ce <- as.numeric(gsub(".*?([0-9]+).*", "\\1", EC_HC_imputed$weeks_ce))

# create a new variable based on period_start and conception date
EC_HC_imputed$weeks_cs <- difftime(EC_HC_imputed$period_start,EC_HC_imputed$conception_date,
                                   units = "weeks")
EC_HC_imputed$weeks_cs <- as.numeric(gsub(".*?([0-9]+).*", "\\1", EC_HC_imputed$weeks_cs))

## considering the habit/behavior variables doesn't relate to the home characteristics
## 

### first trimester 0-12w, second trimester 13w-26w, third trimester after 27w
### combine the habit database with EC_HC database
EC_HC_habit <- full_join(EC_HC_imputed, indoor_habit_combine,
                          by=c("id_mother","id_child","conception_date","delivery_date"))

# create a new variable to select the cases
EC_HC_habit <- EC_HC_habit %>%
  mutate(select = case_when(
    weeks_cs <= 12 & weeks_ce <= 12 ~ "12w",
    weeks_cs > 26 & weeks_ce <= 26 ~ "22w",
    weeks_cs > 12 & weeks_ce <= 26 ~ "22w",
    weeks_cs >26 & weeks_ce >26 ~ "32w",
    weeks_cs <= 12 & weeks_ce >26 ~ "all",
    weeks_cs <= 12 & weeks_ce <= 26 ~ "both", # both means 12w and 22w
    weeks_cs > 12 & weeks_ce > 26 ~ "both_1")) # both_1 means 22w and 32w
 
# delete the case the value of select doesn't match with period, both means keep both 12w and 32w
EC_HC_habit_new <- EC_HC_habit %>%
  subset(select == "all" | (select == "both" & (period == "12w" | period == "22w"))
         | (select == "both_1" & (period == "22w" | period == "32w")) |(select == period)) 

save(EC_HC_habit_new, file = "../output/impute/EC_HC_habit_new.RData")

####################################################################################


## if there are no windows in the room, the open hour should be 0
EC_HC_habit_impute <- EC_HC_habit_new %>%
  mutate(r6_1 = ifelse(ct8b_r01 == 0, 0, r6_1),
         n7_1a = ifelse(ct8b_r01 == 0, 0, n7_1a),
         n7_1b = ifelse(ct8b_r01 == 0, 0, n7_1b),
         r6_2 = ifelse(r6_nv_r02 == 1, 0, r6_2),
         n7_2a = ifelse(n7_2 == 0, 0, n7_2a),
         n7_2b = ifelse(n7_2 == 0, 0, n7_2b),
         r6_3 = ifelse(r6_nv_r03 == 1, 0, r6_3),
         r6_4 = ifelse(r6_nv_r04 == 1, 0, r6_4),
         r6_5 = ifelse(r6_nv_r05 == 1, 0, r6_5),
         r6_6 = ifelse(r6_nv_r06 == 1, 0, r6_6))

#################################################################################
#############-----------------------------------------------------###############
#############                 impute habit database               ###############
#############-----------------------------------------------------###############

##variables used to predict missings (from the other variables)
features <- c("ct8l_b","ct4","h4","ct8l_g","ct9m","maternal_age","educational_level",
              "employment_situation","spouse_finance","difficult_pay_food","difficult_pay_clothes",
              "difficult_pay_heating","difficult_pay_rent","ct6_m02","n7_1","n7_2",
              "ct7_m02","ct9a","ct9g","ct9h","ct9j","ct9k","ct8b_r03","ct8b_r05",
              "heating_livingroom","EC_2_32w","GS3_m02","GS3_m03","GS3_m04",
              "r6_nv_r02","r6_nv_r03","r6_nv_r04","r6_nv_r05","r6_nv_r06",
              "ct1","ct3_8a","ct6_m01","ct7_m01","ct8ec","ct8m","ct8l_f","h3",
              "ct8g","ct8h","ct8i","ct8j","ct8k","ct8l_c","heating_bedroom","ct8b_r01",
              "EC_3_32w","EC_4_32w","GS3_m01")

##variables to impute
to_impute <- c("h5","N6_m01","N6_m03","n7_1a","n7_1b","n7_2a","n7_2b","n7_3a",
               "n7_3b","r6_1","r6_2","r6_3","r6_4","r6_5","r6_6") 


id <- c("gid","period_start") # PK or id variable
data <- EC_HC_habit_impute[, c(id, features, to_impute)] #subset only to 
data[, id] <- NULL

predictorMatrix <- quickpred(data, mincor = 0.2, minpuc = 0.4)
prova_habit <- mice(data,  m = 100, print = F, seed = 1234, predictorMatrix = predictorMatrix)
habit_EC_HC_imputed <- complete(prova_habit,include = F)
save(prova_habit, file = "../output/impute/prova_habit.RData")

pos <- match(EC_HC_habit_impute$id, row.names(habit_EC_HC_imputed))
table(pos == 1:nrow(EC_HC_habit_impute)) ## just check the order (in case not ordered, used pos variable)

EC_HC_habit_impute[, to_impute] <- habit_EC_HC_imputed[, to_impute] ## replace variables by imputed values
habit_EC_HC_imputed <- EC_HC_habit_impute

## if there are no window in the parent's bedroom, the open hour should be 0
habit_EC_HC_imputed <- habit_EC_HC_imputed %>%
  mutate(r6_1 = ifelse(ct8b_r01 == 0, 0, r6_1),
         n7_1a = ifelse(ct8b_r01 == 0, 0, n7_1a),
         n7_1b = ifelse(ct8b_r01 == 0, 0, n7_1b),
         r6_2 = ifelse(r6_nv_r02 == 1, 0, r6_2),
         n7_2a = ifelse(n7_2 == 0, 0, n7_2a),
         n7_2b = ifelse(n7_2 == 0, 0, n7_2b),
         r6_3 = ifelse(r6_nv_r03 == 1, 0, r6_3),
         r6_4 = ifelse(r6_nv_r04 == 1, 0, r6_4),
         r6_5 = ifelse(r6_nv_r05 == 1, 0, r6_5),
         r6_6 = ifelse(r6_nv_r06 == 1, 0, r6_6))


save(habit_EC_HC_imputed, file = "../output/impute/habit_EC_HC_imputed.RData")


#############-----------------------------------------------------###############
#############           select the HC and habit predictors        ###############
#############-----------------------------------------------------###############

habit_imputed <- habit_EC_HC_imputed %>%
  select("id_child","gid","id_mother","period","period_start","period_end",
         "r6_1","r6_2","r6_3","r6_4","r6_5","r6_6","h5",
         "N6_m01","N6_m03","n7_1a","n7_1b","n7_2a","n7_2b","n7_3a",
         "n7_3b")


EC_HC_imputed <- EC_HC_imputed %>%
  select("id_mother","id_child","gid","period_start","period_end","conception_date",
         "delivery_date","maternal_age","educational_level","employment_situation",
         "spouse_finance","difficult_pay_food","difficult_pay_clothes","difficult_pay_heating",
         "difficult_pay_rent","ct1","ct3_8a","ct6_m01","ct7_m01","ct8ec","ct8m",
         "ct8l_f","h3","ct8g","ct8h","ct8i","ct8j","ct8k","ct8l_c","GS3_m01",
         "heating_bedroom","heating_livingroom","EC_2_32w","EC_3_32w","EC_4_32w")

#############-----------------------------------------------------###############
#############   combine HC and habit dataset with meteo dataset   ###############
#############-----------------------------------------------------###############

load("../output/Predict/bisc_meteo_predictor.RData")
bisc_meteo_predictor <- bisc_meteo_predictor %>% # remove the period_id, period_start, period_end and period_ids
  select(-c("period_id"))

### combine with HC dataset
EC_HC_meteo <- full_join(bisc_meteo_predictor, EC_HC_imputed, by="gid")
EC_HC_meteo$within <- with(EC_HC_meteo,date >= period_start & date <= period_end)

EC_HC_meteo <- EC_HC_meteo %>%
  subset(within == TRUE)

### check the different rows between bisc_meteo_predictor and EC_HC_meteo 
missing_cases_1 <- anti_join(bisc_meteo_predictor, EC_HC_meteo,by = c("gid","date"))
table(missing_cases_1$gid)

### gid   id_mother
### 757   11007511  (just 3 case, because the period_end is 2020/07/05, so the date 07/06,07/07,07/08 didn't match)
### 796   12001812 (without the id_mother in the HC database)
### 1106  12029111 (without the id_mother in the HC database)
### 1141  12032411 (without the id_mother in the HC database)
### 1142  12032511 (without the id_mother in the HC database)
### 1156  12033811 (without the id_mother in the HC database)
### 1158  12034011 (without the id_mother in the HC database)


### combine with habit dataset
EC_HC_meteo$period <- difftime(EC_HC_meteo$conception_date,EC_HC_meteo$date, units = "weeks")
EC_HC_meteo$period <- as.numeric(gsub(".*?([0-9]+).*", "\\1",EC_HC_meteo$period))
EC_HC_meteo <- EC_HC_meteo %>%
  mutate(period = case_when(
    period <=12 ~ "12w",
    period >12 & period <= 26 ~ "22w",
    period >=27 ~ "32w"
  ))

EC_HC_meteo_habit <- full_join(EC_HC_meteo, habit_imputed, by=c("gid","id_mother","id_child","period","period_start","period_end"))
# check the missing for variable r6_1 to r6_6 - solved
# Missing <- EC_HC_meteo_habit %>% subset(is.na(r6_1))
# unique(Missing$subject_id) # 10031211 10032611 10036511 10037111 10037811 10040111 10044811 10046011 10046311 11008911
# The reason: some gid didn't show in habit_imupted database, but in EC_HC_meteo (didn't match these rows)
# The problem: define and select period ignore one option for 22w: weeks_cs > 12 & weeks_ce <= 26 ~ "22w" -- have been added

### check the different rows between EC_HC_meteo and EC_HC_meteo_habit 
missing_cases_2 <- anti_join(EC_HC_meteo_habit, EC_HC_meteo,by = c("gid","date"))
table(missing_cases_2$gid)
### 6 cases gid: 214 265 610 670 730 747(2) 936
table(missing_cases_2$id_mother)
### reason: in the "bisc_meteo_predictor" database the end of date is not equivalent to period_end (delivery date)

EC_HC_meteo_habit <- EC_HC_meteo_habit %>%
  subset(!is.na(date)) # delete the NA case # obs 295970 correct

## combine with habit_1 database, we only collect 12w and 32w
EC_HC_meteo_habit$period <- difftime(EC_HC_meteo_habit$conception_date,EC_HC_meteo_habit$date, units = "weeks")
EC_HC_meteo_habit$period <- as.numeric(gsub(".*?([0-9]+).*", "\\1",EC_HC_meteo_habit$period))
EC_HC_meteo_habit <- EC_HC_meteo_habit %>%
  mutate(period = case_when(
    period <=26 ~ "12w",
    period >=27 ~ "32w"
  ))



questionnaire_meteo_predictors <- EC_HC_meteo_habit
questionnaire_meteo_predictors$month <- month(questionnaire_meteo_predictors$date)
questionnaire_meteo_predictors$year <- year(questionnaire_meteo_predictors$date)
save(questionnaire_meteo_predictors, file = "../output/Predict/questionnaire_meteo_predictors.RData")

############################################################################################################

#############-----------------------------------------------------###############
#############              cooling and heating system             ###############
#############-----------------------------------------------------###############
### for the cooling or heating system, we supposed that they only used them in the summer and winter respectively
### for the cooling system we collected the how many hours open in the hot month, for the heating system we just collected whether there is heating in their home

## not in the hot month or average outdoor temperature no more than 25℃, EC_3_32w and EC_4_32w should answer "NO"
load("../output/Predict/questionnaire_meteo_predictors.RData")
questionnaire_meteo_predictors_adjust <- questionnaire_meteo_predictors %>%
  mutate(cooling_system_day_use = ifelse((month==6 | month==7 | month==8) & temperature_mean_home >=25,EC_3_32w,3)) %>%
  mutate(cooling_system_night_use = ifelse((month==6 | month==7 | month==8) & temperature_mean_home >=25,EC_4_32w,3)) %>%
  mutate(heating_bedroom_use = ifelse((month==1 | month==2 | month==12) & temperature_mean_home <=12 , heating_bedroom, 0)) %>% # 1 means they have heating and maybe use, 0 means they don't have heating or they don't need to use it
  mutate(heating_livingroom_use = ifelse((month==1 | month==2 | month==12) & temperature_mean_home <=12 , heating_livingroom, 0)) %>%
  mutate(cooling_system_day = ifelse(month==6 | month==7 | month==8, EC_3_32w,3)) %>%
  mutate(cooling_system_night = ifelse(month==6 | month==7 | month==8, EC_4_32w,3)) %>%
  mutate(heating_system_bedroom = ifelse(month==1 | month==2 | month==12, heating_bedroom, 0)) %>%
  mutate(heating_system_livingroom = ifelse(month==1 | month==2 | month==12, heating_livingroom, 0))
  

questionnaire_meteo_predictors_adjust <- questionnaire_meteo_predictors_adjust [
  , !names(questionnaire_meteo_predictors_adjust) %in% c("within","period")
]
save(questionnaire_meteo_predictors_adjust, file = "../output/Predict/questionnaire_meteo_predictors_adjust.RData")

######################           The END          ##############################
