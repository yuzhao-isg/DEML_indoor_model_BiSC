# Clean the home characteristics, indoor habit, and cooling system databases.
#
# Workflow:
# 1. Check questionnaire values and clean home-characteristic variables.
# 2. Convert invalid, missing, or inconsistent home responses to usable values.
# 3. Clean indoor-habit responses, including window-opening and household-size data.
# 4. Combine the cleaned habit data with the cooling-system questionnaire.
# 5. Save cleaned databases for the multiple-imputation step.
#
# Run this script from the repository root. Input and output databases are
# read from and written to the repository db directory.
library(dplyr)
library(tidyverse)
rm(list = ls())

setwd("db")
load("../output/Clean/HC_combine.RData")
load("../output/Clean/HC_fieldworker_original.RData")


############-----------------------------------------------------------##########
############          check frequency table (HC_fieldworker)           ##########
############-----------------------------------------------------------##########

num_HC_clean <- HC_fieldworker_original %>%
  select(where(is.numeric))
frequency_table_numeric <- num_HC_clean %>%
  pivot_longer(everything(),names_to = "variable",values_to = "value") %>%
  count(variable,value) %>%
  arrange(variable, value)
save(frequency_table_numeric, file = "../output/Clean/Frequency.RData")

###################################################################################

#############-----------------------------------------------------###############
#############                Clean the HC database                ###############
#############-----------------------------------------------------###############

# Clinapsis did not let us put "," or "." for decimal numbers, so, those m3 or m2 are multiplied by 1000
# the real value should divide by 1000
columnes_to_divide <- c("ct8m","ct8l_f","ct8l_g","ct8l_f2","ct8l_g2",
                        "ct8l_f3","ct8l_g3","ct8l_f4","ct8l_g4","ct9m",
                        "ct9l_f","ct9l_g","ct9l_f2","ct9l_g2",
                        "ct9l_f3","ct9l_g3","ct9l_f4","ct9l_g4")
HC_combine[columnes_to_divide] <- lapply(HC_combine[columnes_to_divide],
                                       function(x) as.numeric(x)/1000)

#######################################################################################################################

############-----------------------------------------------------------##########
############          check frequency table (HC_participant)           ##########
############-----------------------------------------------------------##########
table(HC_combine$n7_1, useNA = "ifany")
table(HC_combine$n7_2, useNA = "ifany")
table(HC_combine$n7_3, useNA = "ifany")
table(HC_combine$GS3_m01, useNA = "ifany")
table(HC_combine$GS3_m02, useNA = "ifany")
table(HC_combine$GS3_m03, useNA = "ifany")
table(HC_combine$GS3_m04, useNA = "ifany")

HC_clean <- HC_combine
save(HC_clean, file = "../output/Clean/HC_clean.RData")


#############-----------------------------------------------------###############
#############           outliers and unreasonable value            ###############
#############-----------------------------------------------------###############
### NA means the participants didn't answer the whole questionnaire
### all the answers are 0 means didn't answer this questions, which also belong to missing value
load("../output/Clean/HC_clean.RData")

#############-----------------------------------------------------###############
#############  check ct6 (type of floor) and ct7 (type of wall)   ###############
#############-----------------------------------------------------###############
### check the variable ct6, if they didn't choose anyone or tick more than one
### we need to create new variable based on it (ct6 and ct7)
table(HC_clean$ct6b) # read variable_check
HC_clean <- HC_clean %>%
  mutate(check_ct6_m01 = ct6_m01_r01 + ct6_m01_r02 + ct6_m01_r03 + ct6_m01_r04
         + ct6_m01_r05 + ct6_m01_r06)
HC_clean <- HC_clean %>%
  mutate(check_ct6_m02 = ct6_m02_r01 + ct6_m02_r02 + ct6_m02_r03 + ct6_m02_r04
         + ct6_m02_r05 + ct6_m02_r06)
table(HC_clean$check_ct6_m01) # 0:508, 1:520, 2:9
table(HC_clean$check_ct6_m02) # 0:512, 1:514, 2:11

### if all the option choose 0 means the participants didn't answer this question (but they answered the quetionnaire), belong to NA
HC_clean <- HC_clean %>%
  mutate(ct6_m01 = case_when(
    check_ct6_m01 == 1 & ct6_m01_r01 ==1 ~ 1,
    check_ct6_m01 == 1 & ct6_m01_r02 ==1 ~ 2,
    check_ct6_m01 == 1 & ct6_m01_r03 ==1 ~ 3,
    check_ct6_m01 == 1 & ct6_m01_r04 ==1 ~ 4,
    check_ct6_m01 == 1 & ct6_m01_r05 ==1 ~ 5,
    check_ct6_m01 == 1 & ct6_m01_r06 ==1 ~ 6,
    check_ct6_m01 == 2 ~ 7
  ))
table(HC_clean$ct6_m01, useNA = 'ifany')

HC_clean <- HC_clean %>%
  mutate(ct6_m02 = case_when(
    check_ct6_m02 == 1 & ct6_m02_r01 ==1 ~ 1,
    check_ct6_m02 == 1 & ct6_m02_r02 ==1 ~ 2,
    check_ct6_m02 == 1 & ct6_m02_r03 ==1 ~ 3,
    check_ct6_m02 == 1 & ct6_m02_r04 ==1 ~ 4,
    check_ct6_m02 == 1 & ct6_m02_r05 ==1 ~ 5,
    check_ct6_m02 == 1 & ct6_m02_r06 ==1 ~ 6,
    check_ct6_m02 == 2 ~ 7
  ))
table(HC_clean$ct6_m02, useNA = 'ifany')

####################################################################################
table(HC_clean$ct7b) # read variable_check
HC_clean <- HC_clean %>%
  mutate(check_ct7_m01 = ct7_m01_r01 + ct7_m01_r02 + ct7_m01_r03 + ct7_m01_r04
         + ct7_m01_r05 + ct7_m01_r06 + ct7_m01_r07 + ct7_m01_r08)
table(HC_clean$check_ct7_m01) # 0:507, 1:521, 2:9
HC_clean <- HC_clean %>%
  mutate(check_ct7_m02 = ct7_m02_r01 + ct7_m02_r02 + ct7_m02_r03 + ct7_m02_r04
         + ct7_m02_r05 + ct7_m02_r06 + ct7_m02_r07 + ct7_m02_r08)
table(HC_clean$check_ct7_m02) # 0:511, 1:507, 2:19

#### combine the ct7 into one variables
HC_clean <- HC_clean %>%
  mutate(ct7_m01 = case_when(
    check_ct7_m01 == 1 & ct7_m01_r01 ==1 ~ 1,
    check_ct7_m01 == 1 & ct7_m01_r02 ==1 ~ 2,
    check_ct7_m01 == 1 & ct7_m01_r03 ==1 ~ 3,
    check_ct7_m01 == 1 & ct7_m01_r04 ==1 ~ 4,
    check_ct7_m01 == 1 & ct7_m01_r05 ==1 ~ 5,
    check_ct7_m01 == 1 & ct7_m01_r06 ==1 ~ 6,
    check_ct7_m01 == 1 & ct7_m01_r07 ==1 ~ 7,
    check_ct7_m01 == 1 & ct7_m01_r08 ==1 ~ 8,
    check_ct7_m01 == 2 ~ 9
  ))
table(HC_clean$ct7_m01, useNA = 'ifany')

HC_clean <- HC_clean %>%
  mutate(ct7_m02 = case_when(
    check_ct7_m02 == 1 & ct7_m02_r01 ==1 ~ 1,
    check_ct7_m02 == 1 & ct7_m02_r02 ==1 ~ 2,
    check_ct7_m02 == 1 & ct7_m02_r03 ==1 ~ 3,
    check_ct7_m02 == 1 & ct7_m02_r04 ==1 ~ 4,
    check_ct7_m02 == 1 & ct7_m02_r05 ==1 ~ 5,
    check_ct7_m02 == 1 & ct7_m02_r06 ==1 ~ 6,
    check_ct7_m02 == 1 & ct7_m02_r07 ==1 ~ 7,
    check_ct7_m02 == 1 & ct7_m02_r08 ==1 ~ 8,
    check_ct7_m02 == 2 ~ 9
  ))
table(HC_clean$ct7_m02, useNA = 'ifany')
################################################################################

#############-----------------------------------------------------###############
#############                check ct8a (bedroom floor)            ###############
#############-----------------------------------------------------###############
### check and create a new variable for ct3 (flat floor) and ct8a (bedroom floor)
### ctc_8a (the bedroom floor) 
### for the ct1 (type of home) is flat, if the ct8a is NA, we use ct3/ct9a to fill up the question 
### detect cases that ct1=3 (flat), but the ct3 (flat floor) not the same with ct8a (bedroom floor) and ct9a (living room floor)

HC_clean <- HC_clean %>%
  mutate(ct3_8a = ifelse(ct1 == 3 & is.na(ct8a), ct3, ct8a)) %>%
  mutate(ct3_8a = ifelse(ct1 == 3 & is.na(ct3_8a), ct9a, ct3_8a)) 
table(HC_clean$ct3_8a, useNA = 'ifany')  # 0   1   2   3   4    5    6    7  8 9  10  11  12  13  14 20  NA
#                                         2  58  187  179  137 152  100  65 52 20  16  5   1   5   1   1  295

## check any different between ct3_8a and ct8a
room_floor <- subset(HC_clean,ct3_8a!=ct3 | ct3_8a!=ct9a) %>%
  select("id_mother","id_child","gid","ct1","ct3","ct3_8a","ct8a","ct9a") #47 obs


#############-----------------------------------------------------###############
#############    check ct8b_r01 (parent's bedroom with windows)   ###############
#############-----------------------------------------------------###############


###ct8b_r01 (Parent's bedroom with windows) should be opposite with ct8b_r02 (Room without windows)
HC_clean <- HC_clean %>%
  mutate(check_ct8b_r0102 = ct8b_r01 - ct8b_r02)
table(HC_clean$check_ct8b_r0102, useNA = 'ifany') #     -1   0   1  NA
#                                                        14  18 955 289

# check ct8b_r01 and n7_1 (parent's bedroom with windows), it should be the same answers
HC_clean <- HC_clean %>%
  mutate(check_ct8b_r01_n7_1 = ct8b_r01 - n7_1)
table(HC_clean$check_ct8b_r01_n7_1, useNA = 'ifany') # -1    0    1 <NA> 
#                                                      19  754   58  445

### check why the 18 cases are 0 and if the answer is consistent about the bedroom windows
### check the cases that value of check_ct9b_r01_n7_1 is not 0
### ct8l_b (how many windows on this wall--bedroom) r6_nv_r01 (if bedroom without a window)
### ct8l_f (the total area of the windows) n7_1(if bedroom with a window)
bedroom_windows <- subset(HC_clean, check_ct8b_r0102==0 | ct8b_r02==1 |
                            r6_nv_r01==1 | check_ct8b_r01_n7_1!=0) %>%
  select("id_mother","id_child","gid","ct8b_r01","ct8b_r02","ct8l_b","r6_nv_r01", 
         "ct8l_f","n7_1") #95 obs check one-by-one

## for the gid 106 (id_mother 10010011), we keep two case because she may change the bedroom 
## for the remaining cases, double check the variables related to the bedroom window
## r6_nv_r01=1 (means the parents' bedroom without window)
## n7_1=1 and ct8b_r01=1 (means the parents' bedroom with window)
table(HC_clean$ct8b_r01, useNA = 'ifany')  #    0    1 <NA> 
#                                               32  955  289 

HC_clean <- HC_clean %>%
  mutate(ct8b_r01 = ifelse((n7_1 == 1 | r6_nv_r01 ==0) & !is.na(ct8l_f), 1, ct8b_r01)) %>%
  mutate(ct8b_r01 = ifelse(n7_1 == 0 & r6_nv_r01==1, 0, ct8b_r01)) %>%
  mutate(ct8b_r01 = ifelse(ct8b_r02==1,0,ct8b_r01)) %>%
  mutate(ct8b_r01 = ifelse(n7_1 == 1 & ct8b_r02==0,1,ct8b_r01)) %>%
  mutate(ct8b_r01 = ifelse(ct8l_b >= 1 & !is.na(ct8l_f), 1, ct8b_r01)) %>%
  mutate(ct8b_r01 = ifelse(is.na(ct8b_r01) & !is.na(ct8l_f), 1, ct8b_r01)) %>%
  mutate(ct8b_r01 = ifelse(is.na(ct8b_r01) & (n7_1==0 | r6_nv_r01==1), 0, ct8b_r01)) %>%
  mutate(ct8b_r01 = ifelse(is.na(ct8b_r01) & ct8b_r02==0, 1, ct8b_r01))

table(HC_clean$ct8b_r01, useNA = 'ifany')  #    0    1  <NA> 
#                                               41  962  273 

#############-----------------------------------------------------###############
#############      check ct8c,ct8e,ct8l_a (wall face where)       ###############
#############-----------------------------------------------------###############
### combine the answer into 3 category (1-street, 2-courtyard, 3-inner)
HC_clean <- HC_clean %>%
  mutate(ct8ec = case_when(
    ct8c>=2 | ct8l_a<=3 ~ 1,
    ct8c==1 | ct8e<=2 | ct8l_a==4 ~ 2,
    ct8e>=3 | ct8l_a==4 ~ 3,
    TRUE ~ NA_real_
  ))

table(HC_clean$ct8ec, useNA = "ifany")  #    1    2    3 <NA> 
                                        #  632  376    5  263 


#############-----------------------------------------------------###############
############# check ct8l_f and ct8l_g (the area of window or wall)###############
#############-----------------------------------------------------###############

### set the logic: ct8l_f (window area) should less than ct8l_g (wall area)
HC_clean <- HC_clean %>%
  mutate(check_ct8l_fg = ifelse(ct8l_f >= ct8l_g, 1, 0))
table(HC_clean$check_ct8l_fg, useNA = 'ifany') # 0:496, 1:7 should be 0

# check the 7 cases
window_wall_area <- subset(HC_clean, check_ct8l_fg==1) %>%
  select("id_mother","id_child","gid","ct8l_f","ct8l_g")

HC_clean <- HC_clean %>%
  mutate(check_ct8l_fg2 = ifelse(ct8l_f2 >= ct8l_g2, 1, 0))
table(HC_clean$check_ct8l_fg2) # 0:22

### modify the outlier and the value without logic: see the docs
### for the column ct8l_f
new_values <- as.numeric(1.6,2.44,3.446,NA,NA,1.333,NA,3,1.6)
ids_to_update <- c("10010131","10011931","10013031","10014631","10018031",
                   "10019231","12009131","12022731","10011531")
for (i in seq_along(ids_to_update)) {
  HC_clean$ct8l_f[HC_clean$id_child == ids_to_update[i]] <- new_values[i]
}

new_values_1 <- as.numeric(7.68,NA,11.297,NA,7,7.58)
ids_to_update_1 <- c("10011931","10014631","10019231","12014031","12022731","10011531")
for (i in seq_along(ids_to_update_1)) {
  HC_clean$ct8l_g[HC_clean$id_child == ids_to_update_1[i]] <- new_values_1[i]
}

# if the bedroom without the window the ct8l_f should be 0
without_window <- HC_clean %>%
  subset(ct8b_r01 == 0) %>%
  select("id_mother","id_child","gid","ct8b_r01","ct8l_f")
HC_clean <- HC_clean %>%
  mutate(ct8l_f = ifelse(ct8b_r01 == 0, 0, ct8l_f))

#############-----------------------------------------------------###############
#############   check ct8m and h4 (the area of bedroom and home)  ###############
#############-----------------------------------------------------###############

### set the logic: ct8m (area of bedroom) should less than h4 (area of home)
HC_clean <- HC_clean %>%
  mutate(check_ct8m_h4 = ifelse(ct8m >= h4, 1, 0))
table(HC_clean$check_ct8m_h4) # 0:519, 1:3

# check the 3 cases
bedrrom_home_area <- subset(HC_clean, check_ct8m_h4==1) %>%
  select("id_mother","id_child","gid","ct8m","h4")

# modify the wrong value: see the docs
new_values_2 <- as.numeric(27.5222,NA,27.0609,NA)
ids_to_update_2 <- c("10021731","10024131","10031031","10028131")
for (i in seq_along(ids_to_update_2)) {
  HC_clean$ct8m[HC_clean$id_child == ids_to_update_2[i]] <- new_values_2[i]
}


#############-----------------------------------------------------###############
#############       check ct8k and ct9k (window's protection)     ###############
#############-----------------------------------------------------###############
# clean: if they choose None, they shouldn't choose the other options
HC_clean <- HC_clean %>%
  mutate(ct8k_r01 == ifelse(ct8k_r02==1|ct8k_r03==1|ct8k_r04==1|ct8k_r05==1|ct8k_r06==1|
                              ct8k_r07==1|ct8k_r08==1|ct8k_r09==1,0,ct8k_r01))
### check ct8k
HC_clean <- HC_clean %>%
  mutate(check_ct8k = ct8k_r01 + ct8k_r02 + ct8k_r03 +ct8k_r04
         + ct8k_r05 + ct8k_r06 + ct8k_r07 + ct8k_r08 + ct8k_r09)
table(HC_clean$check_ct8k, useNA = 'ifany') # 1:629, 2:58, 3:2 NA:587

HC_clean <- HC_clean %>%
  mutate(ct9k_r01 == ifelse(ct9k_r02==1|ct9k_r03==1|ct9k_r04==1|ct9k_r05==1|ct9k_r06==1|
                              ct9k_r07==1|ct9k_r08==1|ct9k_r09==1,0,ct9k_r01))
HC_clean <- HC_clean %>%
  mutate(check_ct9k = ct9k_r01 + ct9k_r02 + ct9k_r03 +ct9k_r04
         + ct9k_r05 + ct9k_r06 + ct9k_r07 + ct9k_r08 + ct9k_r09)
table(HC_clean$check_ct9k, useNA = 'ifany') # 0：3, 1:624, 2:58, 3:1 NA:590

# check the 3 cases 
window_protection <- subset(HC_clean, check_ct9k==0) %>%
  select("id_mother","id_child","gid","ct9k_r01","ct9k_r02","ct9k_r03","ct9k_r04",
         "ct9k_r05","ct9k_r06","ct9k_r07","ct9k_r08","ct9k_r09","ct8k_r01","ct8k_r02",
         "ct8k_r03","ct8k_r04","ct8k_r05","ct8k_r06","ct8k_r07","ct8k_r08","ct8k_r09")

# all the answers are 0 means she didn't answer this question,should set it to NA (create a new varible, it will set to NA)

## Create a new variable to combine the variables into one question (ct8k)
## 
HC_clean <- HC_clean %>%
  mutate(ct8k = case_when(
    check_ct8k == 1 & ct8k_r01 ==1 ~ 1,
    check_ct8k == 1 & ct8k_r02 ==1 ~ 2,
    check_ct8k == 1 & ct8k_r03 ==1 ~ 3,
    check_ct8k == 1 & ct8k_r04 ==1 ~ 4,
    check_ct8k == 1 & ct8k_r05 ==1 ~ 5,
    check_ct8k == 1 & ct8k_r06 ==1 ~ 6,
    check_ct8k == 1 & ct8k_r07 ==1 ~ 7,
    check_ct8k == 1 & ct8k_r08 ==1 ~ 8,
    check_ct8k == 1 & ct8k_r09 ==1 ~ 9,
    check_ct8k >= 2 ~ 10
  ))
table(HC_clean$ct8k, useNA = 'ifany')

HC_clean <- HC_clean %>%
  mutate(ct9k = case_when(
    check_ct9k == 0 ~ NA,
    check_ct9k == 1 & ct9k_r01 ==1 ~ 1,
    check_ct9k == 1 & ct9k_r02 ==1 ~ 2,
    check_ct9k == 1 & ct9k_r03 ==1 ~ 3,
    check_ct9k == 1 & ct9k_r04 ==1 ~ 4,
    check_ct9k == 1 & ct9k_r05 ==1 ~ 5,
    check_ct9k == 1 & ct9k_r06 ==1 ~ 6,
    check_ct9k == 1 & ct9k_r07 ==1 ~ 7,
    check_ct9k == 1 & ct9k_r08 ==1 ~ 8,
    check_ct9k == 1 & ct9k_r09 ==1 ~ 9,
    check_ct9k >= 2 ~ 10
  ))
table(HC_clean$ct9k, useNA = 'ifany') # NA:593 (right, because 3 cases all the answer is 0 means NA)


#############-----------------------------------------------------###############
#############     check bedroom and living room (if can impute)   ###############
#############-----------------------------------------------------###############

## check if the type of window protection, materials or type of wall and floor is the same between bedroom and living room (>50%)
# if almost same we can use the value of living room to impute for bedroom

## check the ct8g/ct9g (material of window frame) 
window_frame <- HC_clean %>%
  select("id_mother","id_child","gid","ct9g","ct8g") %>%
  mutate(same = ct9g - ct8g) # 0 means the same #575
table(window_frame$same, useNA = 'ifany') 
print(575/(1267-614)) ## 88% we can use ct9g to impute ct8g (NA)

## check the ct8h/ct9h (window gasket)
window_gasket <- HC_clean %>%
  select("id_mother","id_child","gid","ct9h","ct8h") %>%
  mutate(same = ct9h - ct8h) # 0 means the same
table(window_gasket$same, useNA = 'ifany') 
print(598/(1267-623)) # 92.9% we can

## check the ct8j/ct9j (window glass)
window_glass <-  HC_clean %>%
  select("id_mother","id_child","gid","ct9j","ct8j") %>%
  mutate(same = ct9j - ct8j) # 0 means the same
table(window_glass$same, useNA = 'ifany')
print(590/(1276-621)) # 90% we can

## check the ct8k/ct9k (window protection)
window_protection_1 <- HC_clean %>%
  select("id_mother","id_child","gid","ct9k","ct8k") %>%
  mutate(same = ct9k-ct8k) # 0 means the same
table(window_protection_1$same, useNA = 'ifany') 
print(462/(1276-622)) #70.1% we can use ct9k to impute ct8k (NA)

## check ct6_m01/ct6_m02 (type of floor)
type_floor <- HC_clean %>%
  select("id_mother","id_child","gid","ct6_m01","ct6_m02") %>%
  mutate(same = ct6_m01 - ct6_m02) # 0 means the same
table(type_floor$same, useNA = 'ifany') 
print(478/(1276-752)) # 91% we can

## check ct7_m01/ct7_m02 (type of wall)
type_wall <- HC_clean %>%
  select("id_mother","id_child","gid","ct7_m01","ct7_m02") %>%
  mutate(same = ct7_m01 - ct7_m02) # 0 means the same
table(type_wall$same, useNA = 'ifany') 
print(507/(1276-750)) # 96.4% we can


## we suppose that the type of wall and floor is the same between living room and bedroom
## we suppose the material or other information of the window are similar between bedroom and living room
fill_up_pairs <- list(
  c("ct8g","ct9g"),
  c("ct8h","ct9h"),
  c("ct8j","ct9j"),
  c("ct8k","ct9k"),
  c("ct6_m01","ct6_m02"),
  c("ct7_m01","ct7_m02")
)
fill_na <- function (df, var1, var2){
  df %>%
    mutate(!!sym(var1) := ifelse(is.na(!!sym(var1)), !!sym(var2), !!sym(var1)))
}

for (pair in fill_up_pairs) {
  HC_clean <- fill_na(HC_clean,pair[1],pair[2])
}

table(HC_clean$ct8g, useNA = 'ifany')
table(HC_clean$ct8h, useNA = 'ifany')
table(HC_clean$ct8j, useNA = 'ifany')
table(HC_clean$ct8k, useNA = 'ifany')
table(HC_clean$ct6_m01, useNA = 'ifany')
table(HC_clean$ct7_m01, useNA = 'ifany')
#############-----------------------------------------------------###############
#############       check the ct11 and ct15 (heating system)      ###############
#############-----------------------------------------------------###############

# combine ct11 and ct15 the question related to heating
# ct11_bedroom: do you have heating in your bedroom (1 yes, 0 no)
# ct11_house: do you have heating in your house (1 yes, 0 no)
### all the values are 0 means we collected participant's HC but we didn't collect these questions
HC_clean <- HC_clean %>%
  mutate(ct15_bedroom_1 = ct15_m01_r02 + ct15_m02_r02 + ct15_m03_r02 + ct15_m04_r02
         + ct15_m05_r02 + ct15_m06_r02 + ct15_m07_r02 + ct15_m08_r02 + ct15_m09_r02
         + ct15_m10_r02 + ct15_m11_r02) %>%
  mutate(ct15_bedroom = ifelse(ct15_bedroom_1 == 0, NA,ct15_bedroom_1)) %>%
  mutate(ct15_livingroom_1 = ct15_m01_r01 + ct15_m02_r01 + ct15_m03_r01 + ct15_m04_r01
         + ct15_m05_r01 + ct15_m06_r01 + ct15_m07_r01 + ct15_m08_r01 + ct15_m09_r01
         + ct15_m10_r01 + ct15_m11_r01) %>%
  mutate(ct15_livingroom = ifelse(ct15_livingroom_1 == 0, NA, ct15_livingroom_1))


table(HC_clean$ct15_bedroom, useNA = 'ifany') #   1   2  NA
#                                                482  6  788
table(HC_clean$ct15_livingroom, useNA = 'ifany') # 1   2  NA
#                                                 474 19 783 
table(HC_clean$ct11, useNA = 'ifany') # 1(yes)   2(no)  NA
#                                      266       264    746 

HC_clean <- HC_clean %>%
  mutate(heating_bedroom = ifelse(ct11 == 2 & ct15_m11_r02 == 1, 0,
                                  ifelse(ct11 == 1 | ct15_bedroom>=1, 1, NA))) %>%
  mutate(heating_livingroom = ifelse(ct11 == 2 & ct15_m11_r01 == 1, 0,
                                     ifelse(ct11 == 1 | ct15_livingroom>=1, 1, NA)))

table(HC_clean$heating_bedroom, useNA = 'ifany') # 0 (No) 1(Yes) <NA>
#                                                   108    407   761 
table(HC_clean$heating_livingroom, useNA = 'ifany') # 0 (No) 1(Yes) <NA>
#                                                     53      469    754

HC_clean_new <- HC_clean %>% # move the useless/temporary variable (check_)
  select(-c("check_ct7_m01","check_ct7_m02","check_ct8b_r0102","check_ct8b_r01_n7_1",
            "check_ct8l_fg","check_ct8l_fg2","check_ct8m_h4","==...","check_ct8k",
            "check_ct9k","ct15_bedroom_1","ct15_bedroom","ct15_livingroom_1","ct15_livingroom"))
save(HC_clean_new, file = "../output/Clean/HC_clean_new.RData")

#####################################################################################################
########################## The End for HC database ##################################################
#####################################################################################################

#############-----------------------------------------------------###############
#############                Clean the indoor habit               ###############
#############-----------------------------------------------------###############
rm(list = ls())
load("../output/Clean/indoor_habit.RData")
frequency_table_habit <- indoor_pre_habit %>%
  select(c("r6_1","r6_2","r6_3","r6_4","r6_5","r6_6")) %>%
  pivot_longer(everything(),names_to = "variable",values_to = "value") %>%
  count(variable,value) %>%
  arrange(variable, value)

# r6_1 to r6_6: the logic range is from 0 to 24, if their reply is more than 24 we should check the possible reason and then divide by 60 
# we suppose that they may answer by minutes, if their reply lower than 0, we set to NA
# two outline 25 and 500, we set to NA
clean_values <- function(x) {
  x <- ifelse(x < 0, NA, x)
  x <- ifelse(x == 25 | x ==500, NA, x)
  x <- ifelse(x > 24, x/60, x)
  return(x)
}
indoor_habit_clean <- indoor_pre_habit %>%
  mutate(across(c(r6_1,r6_2,r6_3,r6_4,r6_5,r6_6), clean_values))
save(indoor_habit_clean, file = "../output/Clean/habit_clean.RData")


#############-----------------------------------------------------###############
#############              Clean the indoor habit_1               ###############
#############-----------------------------------------------------###############
load("../output/Clean/indoor_habit_1.RData")
frequency_table_habit_1 <- indoor_pre_habit_1 %>%
  select(-c("id_mother","id_child","conception_date","visiting_date","delivery_date","period")) %>%
  pivot_longer(everything(),names_to = "variable",values_to = "value") %>%
  count(variable,value) %>%
  arrange(variable, value)

#############-----------------------------------------------------###############
#############          check h5 (number of people in home)        ###############
#############-----------------------------------------------------###############
table(indoor_pre_habit_1$h5, useNA = 'ifany') # 0    1    2    3    4    5    6    7    8    9   10 <NA> 
#                                              10   33  743   512  127   41   30  5    10    1    1  647
# The logic range is from 1 to 10, 0 set it to 1 (we suppose that they didn't count themselves)
indoor_habit_1_clean <- indoor_pre_habit_1 %>%
  mutate(h5 = ifelse(h5 == 0, 1, h5))

save(indoor_habit_1_clean, file = "../output/Clean/habit_clean_1.RData")


#############-----------------------------------------------------###############
#############             Combine the habit database              ###############
#############-----------------------------------------------------###############
rm(list = ls())
load("../output/Clean/habit_clean.RData")
load("../output/Clean/habit_clean_1.RData")

indoor_habit_combine <- full_join(indoor_habit_clean,indoor_habit_1_clean,
                                  by=c("id_mother","id_child","conception_date","period","delivery_date"))

# the n7_1a (Bedrooms (Parents): Open hours per night) (range:0-12) should less than r6_1 (Hours per day open / Bedroom (Parents)) range:0-24
# the same for n7_2a and r6_2
open_window_hour <- indoor_habit_combine %>%
  subset(n7_1a > r6_1 | n7_2a > r6_2) # 87 obs

save(indoor_habit_combine, file = "../output/Clean/indoor_habit_combine.RData")
#####################################################################################################
##########################      The End for habit database  #########################################
#####################################################################################################

#############-----------------------------------------------------###############
#############               Clean the cooling system              ###############
#############-----------------------------------------------------###############
load("../output/Clean/indoor_EC.RData")
load("../input/Heat_Exp_Quest.RData")
load("../output/Predict/Meteo_imputed_predict.RData")

# heat is that I cleaned the database before
# Heat exposure data should be placed in db/input before running this section.
names(heat)[1] <- "id_mother"
heat_1 <- heat[-c(10:29)]
indoor_pre_EC <- indoor_pre_EC[-c(5:12)]
indoor_pre_EC <- left_join(indoor_pre_EC,heat_1,by="id_mother")
#### combine EC_2_32w into one question 
cols_to_convert <- c("EC_1_32w","EC_2_32w_r01","EC_2_32w_r02","EC_2_32w_r03","EC_2_32w_r04")
indoor_pre_EC[cols_to_convert] <- lapply(indoor_pre_EC[cols_to_convert], as.numeric)
indoor_pre_EC <- indoor_pre_EC %>%
  mutate(check_EC_2_32w = EC_2_32w_r01 +EC_2_32w_r02+EC_2_32w_r03)
table(indoor_pre_EC$check_EC_2_32w)
indoor_pre_EC <- indoor_pre_EC %>%
  mutate(EC_2_32w = case_when(
    check_EC_2_32w == 1 & EC_2_32w_r01 ==1 ~ 1,
    check_EC_2_32w == 1 & EC_2_32w_r02 ==1 ~ 2,
    check_EC_2_32w == 1 & EC_2_32w_r03 ==1 ~ 3,
    EC_2_32w_r04 == 1 ~ 4,
    check_EC_2_32w >= 2 ~ 5
  ))
table(indoor_pre_EC$EC_2_32w)
indoor_pre_EC <- indoor_pre_EC[-c(15)]

### Clean the visiting_date as before
indoor_pre_EC <- indoor_pre_EC %>%
  mutate(visiting_date = as.Date(visiting_date, format = "%d/%m/%Y")) %>%
  mutate(visiting_date = ifelse(is.na(visiting_date), conception_date + 243, visiting_date)) %>%
  mutate(visiting_date = as.Date(visiting_date, origin = "1970-01-01"))

save(indoor_pre_EC, file = "../output/Clean/indoor_clean_EC.RData")
#################################################################################
# THE END



