# Create participant groups based on gid to predict indoor temperature.
# Run this script from the repository root.
# Updated 9/10/2024: added delivery date, noise variables, and greenspace variables.
# Factor variables are converted to a common class before imputation.


library(dplyr)
library(tidyverse)
library(lubridate)
rm(list = ls())

setwd("db")

# create period for the bisc_home database (denote which participant change their address and the date)
# I used other docs from GIS team to extract the home address information
bisc_home <- read_csv("input/GIS_team/20231009_092551_biscHome_ndvi.csv")
names(bisc_home)[2] <- "id_mother"
load("input/2024_10_17_Yu_Zhao_BiSC_24_010.RData")
temp <- yu_bisc_24_010_2024_10_17[,c("id_mother","id_child","fur_eco_1","f_parto")]
bisc_home <- left_join(bisc_home, temp, by="id_mother")
names(bisc_home)[12] <- "conception_date"
names(bisc_home)[13] <- "delivery_date"
bisc_home$delivery_date <- as.Date(bisc_home$delivery_date)
bisc_home <- bisc_home %>%
  select(-c("50","100","300","500")) 
bisc_home <- bisc_home %>% 
  filter(!is.na(id_child)|!is.na(conception_date))
length(unique(bisc_home$id_child)) #1080

######## --------------------------------------------------------------- ########
########          Create 3 group based on gid                            ########
######## --------------------------------------------------------------- ########

# create a variable to classify the database into three based on gid
# Group 1. one gid only has one corresponding visit/HC data   
# Group 2. one gid has more than one corresponding visits/HC data ---- which means can use impute for each other (change once (or just change the room) between 12w and 32w, or change more than once but they changed their home address between 12w and 32w)  
# Group 3. change their home address before 12w or we didn't collect the corresponding HC data (that means this gid without the HC information)
load("input/2024_08_02_Yu_Zhao_BiSC_24_010_HC.RData")

## convert the factor variables from the characters to numerics 
columnes_character_to_integer <- c("ct3","ct8c","ct8d","ct8e","ct8g","ct8j")
yu_hc_2024_08_02 <- yu_hc_2024_08_02 %>%
  mutate(ct3 = ifelse(ct3 == "7 (te entresuelo)"| ct3 == "7 real", "7", ct3))
yu_hc_2024_08_02[columnes_character_to_integer] <- lapply(yu_hc_2024_08_02[columnes_character_to_integer],
                                                                 function(x) as.integer(x))
# keep in line the value for each variables (NO OR YES) in the HC database (0_No,1_Yes)
convert_values <- function(x) {
  x <- recode(x, "No" = 0, "Yes" = 1)
  return(x)
}
## list of variables needs to be converted
variables_convert <- c("ct8b_r01","ct8b_r02","ct8b_r03","ct8b_r04","ct8b_r05","ct8b_r06",
                       "ct8f_r01","ct8f_r02","ct8f_r03","ct8f_r04","ct8f_r05","ct8f_r06",
                       "ct8k_r01","ct8k_r02","ct8k_r03","ct8k_r04","ct8k_r05","ct8k_r06",
                       "ct8k_r07","ct8k_r08","ct8k_r09")
yu_hc_2024_08_02 <- yu_hc_2024_08_02 %>%
  mutate_at(vars(all_of(variables_convert)),convert_values)

# convert the t_fecha from POSIXct to Date
yu_hc_2024_08_02 <- yu_hc_2024_08_02 %>%
  mutate(t_fecha = as.Date(t_fecha))

length(unique(yu_hc_2024_08_02$id_mother)) # 1257, more than 1080, because Alex told me at the 6m, the id_mother is id_child 
length(unique(yu_hc_2024_08_02$id_child)) # 1081
hc_fieldworker <- yu_hc_2024_08_02 %>%
  mutate(id_child = ifelse(is.na(id_child),id_mother,id_child)) %>%
  select(-c("id_mother")) 
length(unique(hc_fieldworker$id_child)) #1080
hc_fieldworker <- yu_bisc_24_010_2024_10_17[c("id_mother","id_child")] %>%
  full_join(hc_fieldworker,by="id_child") # add the correct id_mother
length(unique(hc_fieldworker$id_mother)) #1080
names(hc_fieldworker)[7] <- "period_w"

### 983 participants with the HC (fieldworker) information
################################################################################
temp <- yu_bisc_24_010_2024_10_17[,c("id_mother","id_child","fur_eco_1","f_parto","h1_32w","p1_q16_addressChange_yn_m_6m_8m")]
temp$f_parto <- as.Date(temp$f_parto)
hc_fieldworker <- left_join(hc_fieldworker,temp,by=c("id_mother","id_child"))
hc_home <- hc_fieldworker %>%
  select(c("id_mother","id_child","period_w","t_fecha","fur_eco_1","f_parto","h1_32w","p1_q16_addressChange_yn_m_6m_8m"))
hc_home$period_t_c <- difftime(hc_home$t_fecha,hc_home$fur_eco_1, units = "weeks")
hc_home$period_t_c <- as.numeric(gsub(".*?([0-9]+).*", "\\1",hc_home$period_t_c))

## check the visit time and correspond period
hc_home %>%
  group_by(period_w) %>%
  summarise(min = min(period_t_c),
            max = max(period_t_c))
## period   min   max
## 12w       12    26
## 32w       30    38
## 6m        64    95

## check all the cases with the visiting at 6m, if they changed their home address after delivery
## if they changed the home address between 32w to 6m, we can’t use 6m to impute for 12w or 32w
check_6m <- subset(hc_home,period_w=="6m") #177 participants
## 10046011, 10051511 they changed their home address at 6m, we can't use their HC data at 6m
## after check the redcap about the home address, 10046011 change their home address at 03-12-2022 after 6m visiting
## we just model for prenatal indoor temperature, so we omit 10051511 6m case
hc_fieldworker <- hc_fieldworker[!(hc_fieldworker$id_mother=="10051511" & hc_fieldworker$period_w == "6m"),]
save(hc_fieldworker, file = "../output/Group/HC.RData")

######## --------------------------------------------------------------- ########
#check the different values for the participants who didn't change their home address#
######## --------------------------------------------------------------- ########
counted <- bisc_home %>%
  group_by(id_mother,id_child) %>%
  summarise(repeat_times = n()) %>%
  subset(repeat_times>1)

count_1 <- bisc_home %>%
  anti_join(counted,by=c("id_mother","id_child")) %>%
  mutate(period = NA)

## for the group1 they didn't change their home address but some of them change the room in the same building
## Identify it based on the HC (fieldworker database)
hc_fieldworker_unique <- hc_fieldworker %>%
  anti_join(counted, by=c("id_mother","id_child"))
hc_fieldworker_unique <- subset(hc_fieldworker_unique,!is.na(period_w)) # filter the participants without any HC information
 
## detect which one collected the HC more than once
repeated_ids <- hc_fieldworker_unique$id_mother[duplicated(hc_fieldworker_unique$id_mother)]
repeated_cases <- hc_fieldworker_unique[hc_fieldworker_unique$id_mother %in% repeated_ids,]
length(unique(repeated_cases$id_mother)) #168
# 168 participants answer more than once for the group1
### most participants live at flat, the bedroom floor should equal to flat floor [but not necessary]
ct3_8a <- subset(repeated_cases,ct3!=ct8a) # 5 cases 

check_complementary <- function(x) {
  sum(is.na(x)) == 1
}

result <- repeated_cases %>%
  group_by(id_mother) %>%
  summarise(
    ct3_same = all(ct3 == ct3[1]),
    ct3_complementary = check_complementary(ct3),
    ct4_same = all(ct4 == ct4[1]),
    ct4_complementary = check_complementary(ct4),
    ct8a_same = all(ct8a == ct8a[1]),
    ct8a_complementary = check_complementary(ct8a)
  )
diffrent_ct3 <- result %>%
  filter(ct3_same == FALSE & ct3_complementary == FALSE)
diffrent_ct4 <- result %>%
  filter(ct4_same == FALSE & ct4_complementary == FALSE)
diffrent_ct8a <- result %>%
  filter(ct8a_same == FALSE & ct8a_complementary == FALSE)

### we checked four participants changed their home (but in the same or near the previous building) or changed their bedroom at the same house
### 10055611 changed the home at 6/1/2021 (June 1st)
### 10051911 changed the home at 6/10/2021 (June 10th)
### 10010011 and 10009511, we didn't collect the exact changed bedroom time, we set the date that participant answered the 32w exposure questionnaire
### 10010011: 26/08/2019; 10009511: 10/09/2019
bisc_home_change <- data.frame(
  gid = c(672,672,630,630,106,106,101,101),
  id_mother = c(10055611,10055611,10051911,10051911,10010011,10010011,10009511,10009511),
  period_ids = c("1, 2","1, 2","1, 2, 3","1, 2, 3","1, 2","1, 2","1, 2, 3","1, 2, 3"),
  period_start = as.Date(c("2020-12-28","2021-06-01","2020-11-11","2021-06-10","2019-01-03","2019-08-26","2018-12-31","2019-09-10")),
  period_end = as.Date(c("2021-05-31","2021-09-28","2021-06-09","2021-08-25","2019-08-25","2019-10-12","2019-09-09","2019-10-03")),
  place_id = c("hdp_h_H455","hdp_h_H455","hdp_h_H690","hdp_h_H690","hdp_h_H53","hdp_h_H53","hdp_h_H297","hdp_h_H297"),
  id_child = c(10055631,10055631,10051931,10051931,10010031,10010031,10009531,10009531),
  conception_date = as.Date(c("2020-12-28","2020-12-28","2020-11-11","2020-11-11","2019-01-03","2019-01-03","2018-12-31","2018-12-31")),
  delivery_date = as.Date(c("2021-09-27","2021-09-27","2021-08-24","2021-08-24","2019-10-11","2019-10-11","2019-10-02","2019-10-02"))
)

bisc_home <- bisc_home %>%
  filter(!id_mother %in% c(10055611,10051911,10010011,10009511))

save(bisc_home_change, file = "../output/Group/bisc_home_change.RData")
save(bisc_home, file = "../output/Group/bisc_home.RData")

######## --------------------------------------------------------------- ########
########       Combine the gid database and HC (fieldworker) database    ########
######## --------------------------------------------------------------- ########
## assign the HC data to each gid (identify if visiting date within the period --- period_start and period_end)
load("../output/Group/HC.RData")
HC_fieldworker_group <- full_join(bisc_home, hc_fieldworker,by=c("id_child","id_mother"))
HC_fieldworker_group$within_time_range <- with(HC_fieldworker_group,t_fecha >= period_start & t_fecha <= period_end)
HC_fieldworker_group$week_ec_n <- difftime(HC_fieldworker_group$period_end,HC_fieldworker_group$conception_date, units = "weeks") 
HC_fieldworker_group$week_ec_n <- as.numeric(gsub(".*?([0-9]+).*", "\\1",HC_fieldworker_group$week_ec_n))
HC_fieldworker_group <- HC_fieldworker_group %>%
  mutate(within_time_range = ifelse(period_w=="6m" & week_ec_n>=35,TRUE,within_time_range))
HC_fieldworker_group12 <- subset(HC_fieldworker_group,within_time_range==TRUE)

#################################################################################

HC_fieldworker_group_change <- full_join(bisc_home_change, hc_fieldworker,by=c("id_child","id_mother")) %>%
  subset(!is.na(gid))
HC_fieldworker_group_change$within_time_range <- with(HC_fieldworker_group_change,t_fecha >= period_start & t_fecha <= period_end)
HC_fieldworker_group_change$week_ec_n <- difftime(HC_fieldworker_group_change$period_end,HC_fieldworker_group_change$conception_date, units = "weeks") 
HC_fieldworker_group_change$week_ec_n <- as.numeric(gsub(".*?([0-9]+).*", "\\1",HC_fieldworker_group_change$week_ec_n))
HC_fieldworker_group_change <- HC_fieldworker_group_change %>%
  mutate(within_time_range = ifelse(period_w=="6m" & week_ec_n>=35,TRUE,within_time_range))

## If gid is the same that means we can impute
########----------------------------------------------------------------########
########                           Group 1                              ########
########----------------------------------------------------------------########
HC_fieldworker_group1_change <- subset(HC_fieldworker_group_change,within_time_range==TRUE)
HC_fieldworker_group1_unqie <- HC_fieldworker_group12[!duplicated(HC_fieldworker_group12$gid) & !duplicated(HC_fieldworker_group12$gid, fromLast = TRUE),]
HC_fieldworker_group1 <- rbind(HC_fieldworker_group1_change,HC_fieldworker_group1_unqie) 
length(unique(HC_fieldworker_group1$gid)) # gid:833 obs:837 because four participants didn't changed their home address

save(HC_fieldworker_group1, file = "../output/Group/HC_group1.RData")

########----------------------------------------------------------------########
########                           Group 2                              ########
########----------------------------------------------------------------########

HC_fieldworker_group2 <- anti_join(HC_fieldworker_group12,HC_fieldworker_group1, by="gid")
### for the group2, there are inconsistent value between 12w and 32w/6m
### See the project documentation for the checked participant values.
### for the id_mother(10036711), only keep 32w data
HC_fieldworker_group2 <- HC_fieldworker_group2 %>%
  filter(!(id_mother == 10036711 & period_w == "12w"))

### for the id_mother(12026911), based on 6m
### for the id_mother(12038211), the type of home is flat, 32w is right
HC_fieldworker_group2 <- HC_fieldworker_group2 %>%
  mutate(ct1 = ifelse(id_mother==12038211&period_w=="6m",3,ct1))

### for the id_mother(10041911,10042411,12029511,10046311), the number of rooms is different between 12w and 6m
### id_mother(10041911,10042411,12029511) based on 6m
### for the id_mother(10046311), after checking the original paper with Alex, we didn’t saw the 6m paper, ignore the 6m data
HC_fieldworker_group2 <- HC_fieldworker_group2 %>%
  filter(!(id_mother == 10046311 & period_w == "6m"))

### for the id_mother(10040211, 10041011, 10041211, 10044211, 10051911, 10052111, 10052311, 12038511, 12039311, 12040011, 12042711)
### the flat floor is different between two visits
HC_fieldworker_group2 <- HC_fieldworker_group2 %>%
  mutate(ct3 = ifelse(id_mother==10040211 | id_mother==10052311,2,ct3)) %>%
  mutate(ct3 = ifelse(id_mother==12040011 | id_mother==12042711,3,ct3)) %>%
  mutate(ct3 = ifelse(id_mother==10041211,4,ct3)) %>%
  mutate(ct3 = ifelse(id_mother==10041011 | id_mother==12039311 | id_mother==10043511,5,ct3)) %>%
  mutate(ct3 = ifelse(id_mother==10044211,7,ct3)) %>%
  mutate(ct3 = ifelse(id_mother==12038511 | id_mother==10052111,8,ct3))
####################################################################################################
### impute for the database
HC_fieldworker_group2_temp <- HC_fieldworker_group2[,c("gid","id_mother","id_child")] 
HC_fieldworker_group2_temp <- HC_fieldworker_group2_temp[!duplicated(HC_fieldworker_group2_temp$gid,fromLast = TRUE),]

HC_fieldworker_group2_12w <- subset(HC_fieldworker_group2,period_w=="12w")
HC_fieldworker_group2_32w <- subset(HC_fieldworker_group2,period_w=="32w")
HC_fieldworker_group2_6m <- subset(HC_fieldworker_group2,period_w=="6m") %>%
  right_join(HC_fieldworker_group2_temp, by=c("gid","id_mother","id_child"))

# Function to fill NA values across three data frames (based on 6m)
common_cols <- c("gid","id_child","id_mother","period_ids","period_start","period_end",
                 "place_id","conception_date","delivery_date","week_ec_n")
merged_group2 <- merge(HC_fieldworker_group2_6m,HC_fieldworker_group2_32w,all.x = TRUE, by = "id_child",suffixes = c(".6m",".32w"))
merged_group2 <- merge(merged_group2,HC_fieldworker_group2_12w,all.x = TRUE, by = "id_child",suffixes =c("",".12w"))
col_names <- setdiff(names(HC_fieldworker_group2),"id_child")
fill_missing <- function(col_name) {
  df1_col <- merged_group2[[paste0(col_name,".6m")]]
  df2_col <- merged_group2[[paste0(col_name,".32w")]]
  df3_col <- merged_group2[[paste0(col_name)]]
  filled_col <- ifelse(is.na(df1_col), ifelse(!is.na(df2_col), df2_col, df3_col), df1_col)
  filled_col <- ifelse(is.na(filled_col), df3_col, filled_col)
  return(filled_col)
}

filled_cols <- lapply(col_names, fill_missing)
HC_fieldworker_group2_fill <- data.frame(id_child = merged_group2$id_child)
HC_fieldworker_group2_fill[col_names] <- filled_cols

# check the missing value before and after filling up 
# install.packages("naniar")
# library(naniar)
# gg_miss_var(hc_fieldworker_group2_new)
# gg_miss_var(hc_fieldworker_group2_fill)
# hc_fieldworker_group1_new %>%
#   miss_var_summary()
sum(is.na(HC_fieldworker_group2_6m)) # 24571
sum(is.na(HC_fieldworker_group2_fill)) # 15695
# convert the type of date variables
HC_fieldworker_group2_fill <- HC_fieldworker_group2_fill %>%
  mutate(across(c(t_fecha, period_start, period_end, conception_date, delivery_date,
                  fur_eco_1, f_parto), ~ as.Date(., origin = "1970-01-01")))
save(HC_fieldworker_group2_fill, file = "../output/Group/HC_fill_group2.RData")


########----------------------------------------------------------------########
########                           Group 3                              ########
########----------------------------------------------------------------########
HC_fieldworker_group3 <- HC_fieldworker_group %>%
  anti_join(HC_fieldworker_group1, by = "gid") %>%
  anti_join(HC_fieldworker_group2, by = "gid") %>%
  distinct(gid, .keep_all = TRUE) %>%
  subset(!is.na(gid))

cols_name <- c("gid","id_child","id_mother","period_ids","period_start","period_end",
                 "place_id","conception_date","delivery_date","week_ec_n","h1_32w","t_fecha",
               "within_time_range","p1_q16_addressChange_yn_m_6m_8m","fur_eco_1","f_parto","period_w")
HC_fieldworker_group3 <- HC_fieldworker_group3 %>%
  mutate(across(-cols_name, ~ NA))

save(HC_fieldworker_group3, file = "../output/Group/HC_group3.RData")
######################################################################################################################################



################################################################################
############ Merge the group database with HC (participant) database  ##########
################################################################################

rm(list = ls())
##################################################################################

# regarding the heat related database (participant)
load("2024_10_17_Yu_Zhao_BiSC_24_010.RData")
load("../output/Group/bisc_home_change.RData")
load("../output/Group/bisc_home.RData")
sel_for_indoor <- yu_bisc_24_010_2024_10_17[,c(1,2,26:29,34,98:116,118,120:132,193,195:225,243,246,247,250:262,313)]
sel_for_indoor$f_parto <- as.Date(sel_for_indoor$f_parto)

### convert the factor variables from the characters to numerics 
columnes_character_to_integer <- c("S26_m01","S26_m02","S26_m03","S26_m04",
                                   "N6_m01","N6_m03","N6_32w_m02","N6_32w_m03",
                                   "GS3_m01","GS3_m02","GS3_m03","GS3_m04",
                                   "GS3_32w_m01","GS3_32w_m02","GS3_32w_m03","GS3_32w_m04")

sel_for_indoor[columnes_character_to_integer] <- lapply(sel_for_indoor[columnes_character_to_integer],
                                                    function(x) as.integer(x))

# keep in line the value for each variables (NO OR YES) in the HC database (0_No,1_Yes)
convert_values <- function(x) {
  x <- recode(x, "No" = 0, "Yes" = 1)
  return(x)
}
## list of variables needs to be converted
variables_convert <- c("n7_1","n7_2","n7_3","n7_1_32w","n7_2_32w","n7_3_32w")
sel_for_indoor <- sel_for_indoor %>%
  mutate_at(vars(all_of(variables_convert)),convert_values)

# separate the database into 12w and 32w (filled out by participants), 22w (second trimester) only collected the habit to open the window
indoor_pre_12w <- sel_for_indoor[,c(1:40,73,89)]
names(indoor_pre_12w)[7] <- "conception_date"
names(indoor_pre_12w)[25] <- "visiting_date"
names(indoor_pre_12w)[42] <- "delivery_date"

indoor_pre_22w <- sel_for_indoor[,c(1:2,7,47:52,89)]
names(indoor_pre_22w)[3:10] <- c('conception_date','r6_1','r6_2','r6_3','r6_4','r6_5','r6_6','delivery_date')

indoor_pre_32w <- sel_for_indoor[,c(1:7,41:46,53:64,73:89)]
names(indoor_pre_32w)[7:42] <- c('conception_date','visiting_date','H9','h3','h4b','h4','h5','r6_1','r6_2','r6_3',
                                 'r6_4','r6_5','r6_6','r6_nv_r01','r6_nv_r02',
                                 'r6_nv_r03','r6_nv_r04','r6_nv_r05','r6_nv_r06','h1_32w',
                                 "N6_m03","N6_m01","n7_1","n7_1a","n7_1b","n7_2",
                                 "n7_2a","n7_2b","n7_3","n7_3a","n7_3b","GS3_m01",
                                 "GS3_m02","GS3_m03","GS3_m04","delivery_date") # the order of N6 are different between 12w and 32w check the codebook
indoor_pre_12w$period <- "12w"
indoor_pre_22w$period <- "22w" # just represent second trimester, no real meanings
indoor_pre_32w$period <- "32w"

# for the variables h3-4, and r6_nv could be changed if they change their address
# for the variables h5,r6_1-6 collected at 12w, 22w, and 32w, which is habit don't related with address
# for the variables N6_m01 and N6_m03, n7_1/2/3a/b is also habit, but only collected at 12w and 32w
# EC_1_32w to EC_4_32w, only collected at 32w related with home characteristics and habit

############################## habit #####################################
# separate the database to clean and merge
indoor_pre_12w_habit <- select(indoor_pre_12w, c(1,2,7,13:18,42,43))
indoor_pre_32w_habit <- select(indoor_pre_32w, c(1,2,7,14:19,42,43))

indoor_pre_12w_habit_1 <- select(indoor_pre_12w, c(1:7,12,25:27,29:30,32:33,35:36,42:43)) # S26_m01 to S26_m04, h5, N6_m01 and N6_m03, n7_1/2/3a/b
indoor_pre_32w_habit_1 <- select(indoor_pre_32w, c(1:8,13,27:28,30,31,33,34,36,37,42,43))

# merge habit database (r6_1 to r6_6)
# convert the characters to numeric, let the class is the same between 12w and 32w 
# some value e.g., 0,15, we replaces commas with dots
# Specify the columns to transform
cols_to_transform <- c("r6_1", "r6_2", "r6_3", "r6_4", "r6_5", "r6_6")
# Transform only the specified columns
indoor_pre_12w_habit[cols_to_transform] <- lapply(indoor_pre_12w_habit[cols_to_transform], function(x) {
  x <- gsub(",", ".", x) 
  as.numeric(x)          
})
  
indoor_pre_habit <- dplyr::bind_rows(indoor_pre_12w_habit,indoor_pre_22w,indoor_pre_32w_habit)

indoor_pre_habit_1 <- bind_rows(indoor_pre_12w_habit_1,indoor_pre_32w_habit_1)

save(indoor_pre_habit, file = "../output/Clean/indoor_habit.RData")
save(indoor_pre_habit_1, file = "../output/Clean/indoor_habit_1.RData")


######################### home characteristics ###########################
indoor_pre_12w_hc <- select(indoor_pre_12w, c("id_mother", "id_child", "h3", "h4b",
                                              "h4", "r6_nv_r01", "r6_nv_r02", "r6_nv_r03",
                                              "r6_nv_r04", "r6_nv_r05", "r6_nv_r06",
                                              "visiting_date","GS3_m01", "GS3_m02", "h1_32w",
                                              "GS3_m03", "GS3_m04", "n7_1","n7_2","n7_3",
                                              "delivery_date","period"))
indoor_pre_32w_hc <- select(indoor_pre_32w, c("id_mother", "id_child", "h3", "h4b",
                                              "h4", "r6_nv_r01", "r6_nv_r02", "r6_nv_r03",
                                              "r6_nv_r04", "r6_nv_r05", "r6_nv_r06",
                                              "visiting_date","GS3_m01", "GS3_m02", "h1_32w",
                                              "GS3_m03", "GS3_m04", "n7_1","n7_2","n7_3",
                                              "delivery_date","period"))
# merge home characteristics database 
indoor_pre_hc <- rbind(indoor_pre_12w_hc,indoor_pre_32w_hc)
# filter the case without any information at 32w
# indoor_pre_hc <- indoor_pre_hc %>% 
#   filter(!is.na(h3)|!is.na(h4b)|!is.na(h4)|!is.na(r6_nv_r01)|!is.na(r6_nv_r02)|
#            !is.na(r6_nv_r03)|!is.na(r6_nv_r04)|!is.na(r6_nv_r05))
save(indoor_pre_hc, file = "../output/Group/indoor_hc.RData")
######################### EC collected at 32w ###########################
indoor_pre_EC <- select(sel_for_indoor, c("id_mother","id_child","fur_eco_1", "f_Hoy_3t_2",
                                          "EC_1_32w","EC_2_32w_r01","EC_2_32w_r02",
                                          "EC_2_32w_r03","EC_2_32w_r04","EC_2b_32w",
                                          "EC_3_32w", "EC_4_32w","f_parto"))
names(indoor_pre_EC)[3] <- "conception_date"
names(indoor_pre_EC)[4] <- "visiting_date"
names(indoor_pre_EC)[13] <- "delivery_date"
save(indoor_pre_EC, file = "../output/Clean/indoor_EC.RData")

######################################################################################################################################



############-------------------------------------------------------#############
#check the different value for the participants didn't change their home address#
############-------------------------------------------------------#############
load("../output/Group/indoor_hc.RData")
load("../output/Group/bisc_home_change.RData")
load("../output/Group/bisc_home.RData")
indoor_hc_group <- full_join(bisc_home,indoor_pre_hc, by=c("id_child","id_mother","delivery_date")) %>%
  subset(!is.na(gid))
indoor_hc_group_change <- full_join(bisc_home_change,indoor_pre_hc, by=c("id_child","id_mother","delivery_date")) %>%
  subset(!is.na(gid))

indoor_hc_group$visiting_date <- as.Date(indoor_hc_group$visiting_date, format = "%d/%m/%Y")
indoor_hc_group$within_time_range <- with(indoor_hc_group,visiting_date >= period_start & visiting_date <= period_end)

indoor_hc_group_change$visiting_date <- as.Date(indoor_hc_group_change$visiting_date, format = "%d/%m/%Y")
indoor_hc_group_change$within_time_range <- with(indoor_hc_group_change,visiting_date >= period_start & visiting_date <= period_end)



### check the different value between 12w and 32w if the participants answered they didn't change the home
### if participants didn't answered them if change home, based on the gis database
indoor_hc_dif <- indoor_hc_group %>%
  subset(!(is.na(h3)&is.na(h4)&is.na(r6_nv_r01)&is.na(r6_nv_r02))) %>%
  subset(h1_32w == 2 | is.na(h1_32w))

### for the participants they didn't change home, detect if the h3,h4,h5,r6_nv are the same between 12w and 32w
check_complementary <- function(x) {
  sum(is.na(x)) == 1
}

result_p <- indoor_hc_dif %>%
  group_by(gid) %>%
  summarise(
    h3_same = all(h3 == h3[1]),
    h3_complementary = check_complementary(h3),
    h4_same = all(h4 == h4[1]),
    h4_complementary = check_complementary(h4),
    # h5_same = all(h5 == h5[1]), move it to habit_1 database
    # h5_complementary = check_complementary(h5),
    r6_nv_r01_same = all(r6_nv_r01 == r6_nv_r01[1]),
    r6_nv_r01_complementary = check_complementary(r6_nv_r01),
    r6_nv_r02_same = all(r6_nv_r02 == r6_nv_r02[1]),
    r6_nv_r02_complementary = check_complementary(r6_nv_r02),
    r6_nv_r03_same = all(r6_nv_r03 == r6_nv_r03[1]),
    r6_nv_r03_complementary = check_complementary(r6_nv_r03),
    r6_nv_r04_same = all(r6_nv_r04 == r6_nv_r04[1]),
    r6_nv_r04_complementary = check_complementary(r6_nv_r04),
    r6_nv_r05_same = all(r6_nv_r05 == r6_nv_r05[1]),
    r6_nv_r05_complementary = check_complementary(r6_nv_r05),
    r6_nv_r06_same = all(r6_nv_r06 == r6_nv_r06[1]),
    r6_nv_r06_complementary = check_complementary(r6_nv_r06) 
  ) 

diffrent_h3 <- result_p %>%
  filter(h3_same == FALSE & h3_complementary == FALSE) # right, all same or complementary
### !!! but id_mother 12002911: she answered she changed the home address but based on gis database only one gid
diffrent_h4 <- result_p %>%
  filter(h4_same == FALSE & h4_complementary == FALSE) # 10000331 ## 10035811 change her home address

### for the question "Number of people living in your home", it could be different between 12w and 32w
### if they didn't change home, other question can impute for each other
# diffrent_h5 <- result_p %>%
  # filter(h5_same == FALSE & h5_complementary == FALSE) #38 participants, check the docs
diffrent_r6_nv <- result_p %>%
  filter((r6_nv_r01_same == FALSE & r6_nv_r01_complementary == FALSE) |
           (r6_nv_r02_same == FALSE & r6_nv_r02_complementary == FALSE) |
           (r6_nv_r03_same == FALSE & r6_nv_r03_complementary == FALSE) |
           (r6_nv_r04_same == FALSE & r6_nv_r04_complementary == FALSE) |
           (r6_nv_r05_same == FALSE & r6_nv_r05_complementary == FALSE) |
           (r6_nv_r06_same == FALSE & r6_nv_r06_complementary == FALSE)) #35 participants 


##############################################################################################################################

########----------------------------------------------------------------########
########                  Process the different date                    ########
########----------------------------------------------------------------########
# check the docs
indoor_hc_group <- indoor_hc_group %>% 
  mutate(h4 = ifelse(id_mother==10000311 & period == "32w", NA, h4)) %>%
  mutate(r6_nv_r01 = ifelse(id_mother==10042911 & period == "32w", 0, r6_nv_r01)) %>%
  mutate(r6_nv_r02 = ifelse((id_mother==10042911 | id_mother==12011711) & period == "12w", 0, r6_nv_r02)) %>%
  mutate(r6_nv_r02 = ifelse((id_mother==10026711 | id_mother==10042911) & period == "32w", 1, r6_nv_r02)) %>%
  mutate(r6_nv_r03 = ifelse((id_mother==10002311 | id_mother==10027411| id_mother==10048511 | id_mother==12007211) & r6_nv_r03==0, 1, r6_nv_r03)) %>%
  mutate(r6_nv_r04 = ifelse((id_mother==10030011 | id_mother==12004411) & period == "32w", 0, r6_nv_r04)) %>%
  mutate(r6_nv_r06 = ifelse((id_mother==10002311 | id_mother==11005511 | id_mother==12004411) & r6_nv_r04==0, 1, r6_nv_r06))
  
indoor_hc_group_change <- indoor_hc_group_change %>%
  mutate(h3 = ifelse(id_mother==10055611 & period == "32w", 3, h3))

####################################################################################  

########----------------------------------------------------------------########
########                  Process the visiting_date                     ########
########----------------------------------------------------------------########
### retrieve the filled out questionnaire date at 12w from the other questionnaire that sent to participants at the same time with the HC questionnaire
load("retrieve_dates/2024_09_17_retrieve_exp12_dates_from_ev12.RData")

yu_retrieve_exp12_dates$period <- "12w"
indoor_hc_visiting_date <- full_join(indoor_hc_group, yu_retrieve_exp12_dates, by=c("id_mother","period")) %>%
  mutate(visiting_date = ifelse(visiting_date=="1111-11-11", NA, visiting_date)) ## set the wrong date to NA
indoor_hc_visiting_date$fecha_de_hoy_32w <- as.Date(indoor_hc_visiting_date$fecha_de_hoy_32w, format = "%d/%m/%Y")
indoor_hc_visiting_date <- indoor_hc_visiting_date %>%  
  mutate(visiting_date = ifelse(!is.na(fecha_de_hoy_32w),fecha_de_hoy_32w,visiting_date)) %>%
  mutate(visiting_date = as.Date(visiting_date, origin = "1970-01-01"))
indoor_hc_visiting_date <- indoor_hc_visiting_date %>% select(-c("fecha_de_hoy_32w","fecha_de_hoy_exp"))  # delete the variables (fecha_de_hoy_32w and fecha_de_hoy_exp)

### for the case that 12w or 32w date still missing, we use the conception_date add mean days to instead 
### compute the means weeks for the existing date (visiting_date - conception_date)
indoor_hc_visiting_date$days <- difftime(indoor_hc_visiting_date$visiting_date,indoor_hc_visiting_date$conception_date, units = "days") 
indoor_hc_visiting_date$days <- as.numeric(gsub(".*?([0-9]+).*", "\\1",indoor_hc_visiting_date$days))

indoor_hc_visiting_date %>%
  group_by(period) %>%
  summarise(mean(days, na.rm = TRUE)) ## 12w: 134 days; 32w: 243 days

indoor_hc_visiting_date <- indoor_hc_visiting_date %>%
  mutate(visiting_date = ifelse(is.na(visiting_date) & period == "12w", conception_date + 134, visiting_date)) %>%
  mutate(visiting_date = ifelse(is.na(visiting_date) & period == "32w", conception_date + 243, visiting_date)) %>%
  mutate(visiting_date = as.Date(visiting_date, origin = "1970-01-01"))

indoor_hc_visiting_date <- indoor_hc_visiting_date %>% select(-"days") # delete the variable days

### recalculate the time range
indoor_hc_visiting_date$within_time_range <- with(indoor_hc_visiting_date,visiting_date >= period_start & visiting_date <= period_end)

######################################################################################

### select the participants who didn't change home address, but we need to keep both values of 12w and 32w (same gid) check the docs
gid_move_r6_nv <- data.frame(gid = c(96,257,429,456,481,499,597,687,733,826)) # check the docs

indoor_hc_group12 <- indoor_hc_visiting_date %>%
  anti_join(gid_move_r6_nv, by="gid") %>%
  filter(!(gid==810)) %>% #id_mother==12002911
  subset(!(is.na(h3)&is.na(h4)&is.na(r6_nv_r01)&is.na(r6_nv_r02)&is.na(r6_nv_r03)
           &is.na(r6_nv_r04)&is.na(r6_nv_r05)&is.na(r6_nv_r05)&is.na(n7_1)&is.na(n7_2)
           &is.na(n7_3)&is.na(GS3_m01)&is.na(GS3_m02)&is.na(GS3_m03)&is.na(GS3_m04))) %>% ### delete the cases all the value is NA
  subset(within_time_range == TRUE)

gid_810 <- data.frame(gid = 810)
indoor_hc_group_change_gid <- rbind(gid_move_r6_nv,gid_810) %>%
  left_join(indoor_hc_visiting_date, by = "gid") %>%
  distinct(gid, period, .keep_all = TRUE)


indoor_hc_group_change_nhome <- rbind(indoor_hc_group_change, indoor_hc_group_change_gid)


###########################################################################################

########----------------------------------------------------------------########
########                           Group 1                              ########
########----------------------------------------------------------------########

indoor_hc_group1_change <- subset(indoor_hc_group_change_nhome,within_time_range==TRUE) 
length(unique(indoor_hc_group1_change$gid)) #15 gid and participant (4 based on the HC_fieldworker, 10 based on the r6_nv, 1 is id_mother==12002911)
### except gid(672,630) -- id_mother(10055611,10051911), they change home not only just bedroom or decorate the house, for the remaining, we suppose they just change the bedroom not the whole home address (because we didn't collect their new address)
### for the remaining cases, the other values can be imputed between 12w and 32w
indoor_hc_group1_change_except <- indoor_hc_group1_change %>%
  filter(id_mother==10055611 | id_mother==10051911)

indoor_hc_group1_change_impute <- indoor_hc_group1_change %>%
  filter(!(id_mother==10055611 | id_mother==10051911))

# Use the same ID to impute missing values
indoor_hc_group1_change_impute <- indoor_hc_group1_change_impute %>%
  mutate(date_32w = ifelse(period=="32w", visiting_date, NA)) %>%
  group_by(gid) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), first(na.omit(.)), .))) %>%
  ungroup() %>%
  mutate(across(c(period_start, period_end, conception_date, delivery_date,visiting_date, date_32w), 
                ~ as.Date(., origin = "1970-01-01")))


### change the period_end to the 32w visiting_date

indoor_hc_group1_change_impute <- indoor_hc_group1_change_impute %>%
  mutate(period_end = ifelse(period=="12w",date_32w -1, period_end)) %>%
  mutate(period_start = ifelse(period=="32w", date_32w, period_start)) %>%
  mutate(across(c(period_start, period_end), 
                ~ as.Date(., origin = "1970-01-01")))
indoor_hc_group1_change_impute <- indoor_hc_group1_change_impute[-c(30)] # delete the date_32w

# save the docs for the next merge group
indoor_hc_group1_change_impute_1 <- indoor_hc_group1_change_impute %>% ### delete the 10010011 and 10009511
  filter(!(id_mother==10010011 | id_mother==10009511))
save(indoor_hc_group1_change_impute_1, file = "../output/Group/indoor_hc_gid_no_home_change.RData")

## after imputing the factors variables convert into numerics 
## when combine the two databases, the values will be NA (solve the problem need to convert all the variables into the same class at the begainning)
indoor_hc_group1_change_same_gid <- rbind(indoor_hc_group1_change_except,indoor_hc_group1_change_impute)
length(unique(indoor_hc_group1_change_same_gid$gid)) #15 correct
#################################################################################

indoor_hc_group1_unqie <- indoor_hc_group12[!duplicated(indoor_hc_group12$gid) & !duplicated(indoor_hc_group12$gid, fromLast = TRUE),]
length(unique(indoor_hc_group1_unqie$gid)) #414
indoor_hc_group1 <- rbind(indoor_hc_group1_change_same_gid,indoor_hc_group1_unqie) 
length(unique(indoor_hc_group1$gid)) #429

save(indoor_hc_group1, file = "../output/Group/indoor_hc_group1.RData")


########----------------------------------------------------------------########
########                           Group 2                              ########
########----------------------------------------------------------------########
indoor_hc_group2 <- subset(indoor_hc_group12, within_time_range==TRUE)
indoor_hc_group2 <- indoor_hc_group2[duplicated(indoor_hc_group2$gid) | duplicated(indoor_hc_group2$gid, fromLast = TRUE),]

indoor_hc_group2_12w <- subset(indoor_hc_group2, period == "12w")
indoor_hc_group2_32w <- subset(indoor_hc_group2, period == "32w")


### impute for the database
# Function to fill NA values across two data frames (based on 12w)
common_cols <- c("gid","id_child","id_mother","period_ids","period_start","period_end",
                 "place_id","conception_date","delivery_date","visiting_date")
merged_group2_p <- merge(indoor_hc_group2_12w,indoor_hc_group2_32w,all.x = TRUE, by = "id_child",suffixes = c(".12w",".32w"))
col_names <- setdiff(names(indoor_hc_group2),"id_child")
fill_missing <- function(col_name) {
  df1_col <- merged_group2_p[[paste0(col_name,".12w")]]
  df2_col <- merged_group2_p[[paste0(col_name,".32w")]]
  ifelse(is.na(df1_col), df2_col, df1_col)
}
filled_cols <- lapply(col_names, fill_missing)
indoor_hc_group2_fill <- data.frame(id_child = merged_group2_p$id_child)
indoor_hc_group2_fill[col_names] <- filled_cols

# check the missing value before and after filling up 
# install.packages("naniar")
library(naniar)
gg_miss_var(indoor_hc_group2_12w)
gg_miss_var(indoor_hc_group2_fill)
# hc_fieldworker_group1_new %>%
#   miss_var_summary()
sum(is.na(indoor_hc_group2_12w)) # 2159
sum(is.na(indoor_hc_group2_fill)) # 1868
# transfter the type of date variables
indoor_hc_group2_fill <- indoor_hc_group2_fill %>%
  mutate(across(c( period_start, period_end, conception_date,delivery_date, visiting_date), 
                ~ as.Date(., origin = "1970-01-01"))) 
length(unique(indoor_hc_group2_fill$gid)) #gid 531
save(indoor_hc_group2_fill, file = "../output/Group/indoor_hc_group2_fill.RData")



########----------------------------------------------------------------########
########                           Group 3                              ########
########----------------------------------------------------------------########

indoor_hc_group3 <- indoor_hc_visiting_date %>%
  anti_join(indoor_hc_group1, by = "gid") %>%
  anti_join(indoor_hc_group2_fill, by = "gid") %>%
  distinct(gid, .keep_all = TRUE) %>%
  subset(!is.na(gid)) 

cols_name <- c("gid","id_child","id_mother","period_ids","period_start","period_end",
               "place_id","conception_date","delivery_date","visiting_date","h1_32w","within_time_range",
               "period")
indoor_hc_group3 <- indoor_hc_group3 %>%
  mutate(across(-cols_name, ~ NA)) 
length(unique(indoor_hc_group3$gid)) #301

save(indoor_hc_group3, file = "../output/Group/indoor_hc_group3.RData")
# gid = 15+ 414+ 531 +301 = 1261 correct

##################################################################################




##################################################################################

############-------------------------------------------------------#############
############         combine both HC database and set logic        #############
############-------------------------------------------------------#############
rm(list = ls())

load("../output/Group/indoor_hc_group1.RData")
load("../output/Group/HC_group1.RData")
load("../output/Group/indoor_hc_group2_fill.RData")
load("../output/Group/HC_fill_group2.RData")
load("../output/Group/indoor_hc_group3.RData")
load("../output/Group/HC_group3.RData")
load("../output/Group/indoor_hc_gid_no_home_change.RData")

### merge the HC_fieldworker database
HC_fieldworker_original <- rbind(HC_fieldworker_group1,HC_fieldworker_group2_fill,HC_fieldworker_group3)

### process the gid (count twice without changing home address) in HC database answered by participant
### this process let the rows are same between HC_fieldworker database and HC_participants database
HC_fieldworker_1 <- HC_fieldworker_original %>%
  anti_join(indoor_hc_group1_change_impute_1, by="gid")
HC_fieldworker_2 <- HC_fieldworker_original %>%
  anti_join(HC_fieldworker_1, by="gid")

indoor_hc_group1_change_impute_1 <- indoor_hc_group1_change_impute_1 %>%
  select(c("gid", "period_start", "period_end")) # only keep the gid, period_start, and period_end of this database
HC_fieldworker_2 <- HC_fieldworker_2 %>% select(-c("period_start", "period_end"))
HC_fieldworker_2 <- left_join(indoor_hc_group1_change_impute_1,HC_fieldworker_2,by="gid")
HC_fieldworker <- rbind(HC_fieldworker_1,HC_fieldworker_2)

### merge the HC_participant database
HC_participant <- rbind(indoor_hc_group1,indoor_hc_group2_fill,indoor_hc_group3) 

# Merge the whole HC database
HC_combine <- full_join(HC_participant, HC_fieldworker, 
                        by=c("gid","id_child","id_mother","period_start","period_end",
                             "place_id","period_ids","conception_date","delivery_date"))
HC_combine <- HC_combine %>%
  select(-c("h1_32w.x", "within_time_range.x", "period", "quest_count",
            "address_change", "gestage_m_hc", "t_fecha", "t_visita", "fur_eco_1",
            "f_parto", "h1_32w.y", "p1_q16_addressChange_yn_m_6m_8m", "within_time_range.y",
            "week_ec_n"))

save(HC_fieldworker_original, file = "../output/Clean/HC_fieldworker_original.RData")
save(HC_fieldworker, file = "../output/Clean/HC_fieldworker.RData")
save(HC_participant, file = "../output/Clean/HC_participant.RData")
save(HC_combine, file = "../output/Clean/HC_combine.RData")

########### The End #############################################################


