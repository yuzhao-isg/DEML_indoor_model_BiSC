#-----------------------------------------------------------------------------#
#                 Simple-model weekly validation - BE                       #
#-----------------------------------------------------------------------------#
# Description:
# This script performs long-term weekly validation for the BE analysis. It
# creates participant-level test sets, trains the DEML base and meta-models, and
# saves temperature and humidity validation results.
#
# Outputs:
# - results/Model/Temporal_Validation_weekly_40_BE/Temperature/*
# - results/Model/Temporal_Validation_weekly_40_BE/Humidity/*
#-----------------------------------------------------------------------------#

rm(list = ls())
library(caret)
library(dplyr)
library(stringr)
library(data.table)
library(readr)
library(doParallel)
library(nnls)
library(CAST)
library(ranger)
library(gbm)
library(xgboost)

script_dir <- if (!is.null(sys.frames()[[1]]$ofile)) {
  dirname(normalizePath(sys.frames()[[1]]$ofile))
} else {
  getwd()
}

find_project_root <- function(start_dir) {
  current_dir <- normalizePath(start_dir, winslash = "/", mustWork = FALSE)
  repeat {
    if (dir.exists(file.path(current_dir, "results")) && dir.exists(file.path(current_dir, "db"))) {
      return(current_dir)
    }
    parent_dir <- dirname(current_dir)
    if (identical(parent_dir, current_dir)) {
      stop("Project root not found. Please check the folder structure.")
    }
    current_dir <- parent_dir
  }
}

project_root <- find_project_root(script_dir)
path_out <- file.path(project_root, "results", "Model", "Temporal_Validation_weekly_40_BE", "")
path_mod <- file.path(project_root, "db", "output", "Model")

for (subdir in c("Temperature", "Humidity", "table_figure/Temperature", "table_figure/Humidity")) {
  dir.create(file.path(path_out, subdir), recursive = TRUE, showWarnings = FALSE)
}

source(file.path(project_root, "script", "indoor_temp", "DEML", "Temporal_longterm_validation", "10_DEML_Function_weekly.R"))

# Load the database.
load(file.path(path_mod, "temp_preidctors_data_model.RData"))

temp_preidctors_data_model <- temp_preidctors_data_model %>%
  mutate(across(c("difficult_pay_heating", "ct1", "ct6_m01", "ct7_m01",
                  "ct8ec", "ct8k", "ct8g", "ct8h","ct8i","ct8j","heating_system_bedroom", 
                  "heating_system_livingroom", "EC_2_32w","GS3_m01","N6_m01","N6_m03",
                  "cooling_system_day","cooling_system_night","month"), as.factor)) %>%
  mutate(id_week = paste0(id_mother, "_", week))

# select the predictors
temp_hr_model <- temp_preidctors_data_model %>%
  select("gid", "id_week", "id_mother", "date","sr","apavg","wind_speed_10m",
         "wind_direction_10m","precipitation", "ndvi_300", "temperature_min_home",
         "temperature_mean_home", "temperature_max_home", "humidity_mean_home","hum_mean_lag1_avg",
         "temp_mean_lag1_avg", "indoor_temp","indoor_hr","year","month","yday","week")

# check which variables with missing value
# library(naniar)
# gg_miss_var(temp_preidctors_data_model_new) #n7_x, N6_mx, h5 with missing value - solved

# omit the NA value
# temp_model_new <- na.omit(temp_model) 

# check the missing id
# removed_rows <- setdiff(rownames(temp_preidctors_data_model_1), rownames(temp_preidctors_data_model_new))
# removed_data <- temp_preidctors_data_model_1[removed_rows, ]
# removed_ids <- unique(removed_data$id_mother) # 12042511 12042711 12042911 12043011 12043111 12043211

## set randomly one day in each week for each participant as testing dataset
set.seed(1203)  # Set a seed for reproducibility
id_two_weeks <- temp_hr_model %>%
  group_by(id_mother) %>%
  filter(all(c(12, 32) %in% week)) %>%
  distinct(id_mother) %>%
  pull(id_mother)

test_mothers <- sample(id_two_weeks, size = round(0.4 * length(id_two_weeks)))

test_selection <- tibble(
  id_mother = test_mothers,
  test_week = sample(c(12, 32), size = length(test_mothers), replace = TRUE)
)

testset <- temp_hr_model %>%
  inner_join(test_selection, by = "id_mother") %>%
  filter(week == test_week) %>%
  select(-test_week)

# Filter the dataset to keep only the last 7 days as the test dataset and others for training dataset
trainset <- temp_hr_model %>%
  anti_join(testset, by=c("gid","date","week"))

trainset_temp <- trainset %>% select(-indoor_hr)
testset_temp <- testset %>% select(-indoor_hr)

trainset_hr <- trainset 
testset_hr <- testset
################################################################################
# except random forest, other models needs to set dummy variables for factor variables

# Define variables to exclude from dummy conversion
vars_no_convert <- c("gid", "id_mother", "week", "date", "id_week")

# Extract variables to convert
vars_convert <- setdiff(names(trainset), vars_no_convert)

# Create dummyVars model based on training data
dummy_model <- dummyVars(~ ., data = trainset[, vars_convert])

# Apply to both trainset and testset
trainset_dummy <- cbind(
  trainset[, vars_no_convert],
  predict(dummy_model, newdata = trainset[, vars_convert]) |> as.data.frame()
)

testset_dummy <- cbind(
  testset[, vars_no_convert],
  predict(dummy_model, newdata = testset[, vars_convert]) |> as.data.frame()
)

saveRDS(trainset_dummy, file = paste0(path_out,"train_dummy_dataset.rds"))
trainset_dummy_temp <- trainset_dummy %>% select(-indoor_hr)
testset_dummy_temp <- testset_dummy %>% select(-indoor_hr)

trainset_dummy_hr <- trainset_dummy
testset_dummy_hr <- testset_dummy

#====================
pred_rf = list()
pred_xgb = list()
pred_svm = list()

pred_train_rf = list()
pred_train_xgb = list()
pred_train_svm = list()

# for base model combination
test_pred = list()
train_pred = list()
# meta predict results
pred_train_rf_meta = list()
pred_train_glm_meta = list()

pred_rf_meta = list()
pred_glm_meta = list()
# for meta model combination
test_pred_meta = list()
train_pred_meta = list()
# for deml
deml_train = list()
deml_test = list()

all_train = list()
all_test = list()

#========================================================================
#############-----------------------------------------------------###############
#############                     Fit the  Model                  ###############
#############-----------------------------------------------------###############

nthreads <- detectCores()

#=====================================================#
#                  Random Forest Model                #
#=====================================================#

#==================       train for temperature         ========================# 
temp_rf_basic <- rf_fine_tune(trainset_temp, "indoor_temp", nthreads)
cat(capture.output(print(temp_rf_basic$bestTune)), file = paste0(path_out,"Temperature/temp_model_results_log.txt"), append = TRUE)

# predict using the train set
temp_pred_train_rf <- predict(temp_rf_basic)

# predict using the test set
temp_pred_rf <- predict(temp_rf_basic, newdata = testset_temp)

# Extract feature importance
temp_varimp_rf <- varImp(temp_rf_basic)
temp_importance_rf <- data.frame(
  Variable = rownames(temp_varimp_rf$importance),
  Importance_rf = temp_varimp_rf$importance[, 1]  # Assuming only one importance column
)

saveRDS(temp_rf_basic, file = paste0(path_out,"Temperature/train_DEML_finalmod_rf.rds"))  # Save the best model
write_csv(temp_importance_rf, paste0(path_out,"table_figure/Temperature/varimp_DEML_train_rf_weekly.csv"))


#==================         train for humidity         ========================# 
hr_rf_basic <- rf_fine_tune(trainset_hr, "indoor_hr", nthreads)
cat(capture.output(print(hr_rf_basic$bestTune)), file = paste0(path_out,"Humidity/hr_model_results_log.txt"), append = TRUE)

# predict using the train set
hr_pred_train_rf <- predict(hr_rf_basic)

# predict using the test set
hr_pred_rf <- predict(hr_rf_basic, newdata = testset_hr)

# Extract feature importance
hr_varimp_rf <- varImp(hr_rf_basic)
hr_importance_rf <- data.frame(
  Variable = rownames(hr_varimp_rf$importance),
  Importance_rf = hr_varimp_rf$importance[, 1]  # Assuming only one importance column
)

saveRDS(hr_rf_basic, file = paste0(path_out,"Humidity/train_DEML_finalmod_rf.rds"))  # Save the best model
write_csv(hr_importance_rf, paste0(path_out,"table_figure/Humidity/varimp_DEML_train_rf_weekly.csv"))


#=====================================================#
#                    XGBoost Model                    #
#=====================================================#

#==================       train for temperature         ========================# 
temp_xgb_basic <- xgb_fine_tune(trainset_dummy_temp, "indoor_temp", nthreads)
cat(capture.output(print(temp_xgb_basic$bestTune)), file = paste0(path_out,"Temperature/temp_model_results_log.txt"), append = TRUE)


temp_train_xgb <-
  data.matrix(trainset_dummy_temp[, colnames(trainset_dummy_temp) %in% c("gid","id_mother","date","week","id_week","indoor_temp") == FALSE])
temp_train_label <-
  data.matrix(trainset_dummy_temp[, "indoor_temp"])

temp_test_xgb <-
  data.matrix(testset_dummy_temp[, colnames(testset_dummy_temp) %in% c("gid","id_mother","date","week","id_week","indoor_temp") == FALSE])
temp_test_label <-
  data.matrix(testset_dummy_temp[, "indoor_temp"])

# predict using the train set
temp_pred_train_xgb <- predict(temp_xgb_basic, newdata = temp_train_xgb)
# model prediction
temp_pred_xgb <- predict(temp_xgb_basic, newdata = temp_test_xgb)

# Extract feature importance
temp_varimp_xgb <- varImp(temp_xgb_basic)
temp_importance_xgb <- data.frame(
  Variable = rownames(temp_varimp_xgb$importance),
  Importance_xgb = temp_varimp_xgb$importance[, 1]  # Assuming only one importance column
)


saveRDS(temp_xgb_basic, file = paste0(path_out,"Temperature/train_DEML_finalmod_xgb.rds"))  # Save the best model
write_csv(temp_importance_xgb, paste0(path_out,"table_figure/Temperature/varimp_DEML_train_xgb_weekly.csv"))

#==================         train for humidity         ========================# 

hr_xgb_basic <- xgb_fine_tune(trainset_dummy_hr, "indoor_hr", nthreads)
cat(capture.output(print(hr_xgb_basic$bestTune)), file = paste0(path_out,"Humidity/hr_model_results_log.txt"), append = TRUE)

hr_train_xgb <-
  data.matrix(trainset_dummy_hr[, colnames(trainset_dummy_hr) %in% c("gid","id_mother","date","week","id_week","indoor_hr") == FALSE])
hr_train_label <-
  data.matrix(trainset_dummy_hr[, "indoor_hr"])

hr_test_xgb <-
  data.matrix(testset_dummy_hr[, colnames(testset_dummy_hr) %in% c("gid","id_mother","date","week","id_week","indoor_hr") == FALSE])
hr_test_label <-
  data.matrix(testset_dummy_hr[, "indoor_hr"])

# predict using the train set
hr_pred_train_xgb <- predict(hr_xgb_basic, newdata = hr_train_xgb)
# model prediction
hr_pred_xgb <- predict(hr_xgb_basic, newdata = hr_test_xgb)

# Extract feature importance
hr_varimp_xgb <- varImp(hr_xgb_basic)
hr_importance_xgb <- data.frame(
  Variable = rownames(hr_varimp_xgb$importance),
  Importance_xgb = hr_varimp_xgb$importance[, 1]  # Assuming only one importance column
)


saveRDS(hr_xgb_basic, file = paste0(path_out,"Humidity/train_DEML_finalmod_xgb.rds"))  # Save the best model
write_csv(hr_importance_xgb, paste0(path_out,"table_figure/Humidity/varimp_DEML_train_xgb_weekly.csv"))


#=====================================================#
#             Gradient Boosting Machine               #
#=====================================================#

#==================       train for temperature         ========================# 

temp_gbm_basic <- gbm_fine_tune(trainset_dummy_temp, "indoor_temp", nthreads)
cat(capture.output(print(temp_gbm_basic$bestTune)), file = paste0(path_out,"Temperature/temp_model_results_log.txt"), append = TRUE)

# predict using the train set
temp_pred_train_gbm <- predict(temp_gbm_basic)
# prediction SVR
temp_pred_gbm <- predict(temp_gbm_basic, newdata = testset_dummy_temp)

# Extract feature importance
temp_varimp_gbm <- varImp(temp_gbm_basic)
temp_importance_gbm <- data.frame(
  Variable = rownames(temp_varimp_gbm$importance),
  Importance_gbm = temp_varimp_gbm$importance[, 1]  # Assuming only one importance column
)

saveRDS(temp_gbm_basic, file = paste0(path_out,"Temperature/train_DEML_finalmod_gbm.rds"))  # Save the best model
write_csv(temp_importance_gbm, paste0(path_out,"table_figure/Temperature/varimp_DEML_train_gbm_weekly.csv"))

#==================         train for humidity         ========================# 
hr_gbm_basic <- gbm_fine_tune(trainset_dummy_hr, "indoor_hr", nthreads)
cat(capture.output(print(hr_gbm_basic$bestTune)), file = paste0(path_out,"Humidity/hr_model_results_log.txt"), append = TRUE)

# predict using the train set
hr_pred_train_gbm <- predict(hr_gbm_basic)
# prediction SVR
hr_pred_gbm <- predict(hr_gbm_basic, newdata = testset_dummy_hr)

# Extract feature importance
hr_varimp_gbm <- varImp(hr_gbm_basic)
hr_importance_gbm <- data.frame(
  Variable = rownames(hr_varimp_gbm$importance),
  Importance_gbm = hr_varimp_gbm$importance[, 1]  # Assuming only one importance column
)

saveRDS(hr_gbm_basic, file = paste0(path_out,"Humidity/train_DEML_finalmod_gbm.rds"))  # Save the best model
write_csv(hr_importance_gbm, paste0(path_out,"table_figure/Humidity/varimp_DEML_train_gbm_weekly.csv"))


#================================================================================


# combine the train prediction
#==================          temperature         ========================# 

temp_dummy_train_pred <-
  cbind(
    trainset_dummy_temp[, colnames(trainset_dummy_temp) %in% c("gid","id_mother","date","week") == FALSE],
    temp_rf_pred = temp_pred_train_rf,
    temp_xgb_pred = temp_pred_train_xgb,
    temp_gbm_pred = temp_pred_train_gbm
  )

temp_train_pred <-
  cbind(
    trainset_temp[, colnames(trainset_temp) %in% c("gid","id_mother","date","week") == FALSE],
    temp_rf_pred = temp_pred_train_rf,
    temp_xgb_pred = temp_pred_train_xgb,
    temp_gbm_pred = temp_pred_train_gbm
  )

# combine the test prediction
temp_dummy_test_pred <-
  cbind(
    testset_dummy_temp[, colnames(testset_dummy_temp) %in% c("gid","id_mother","date","week") == FALSE],
    temp_rf_pred = temp_pred_rf,
    temp_xgb_pred = temp_pred_xgb,
    temp_gbm_pred = temp_pred_gbm
  )

temp_test_pred <-
  cbind(
    testset_temp[, colnames(testset_temp) %in% c("gid","id_mother","date","week") == FALSE],
    temp_rf_pred = temp_pred_rf,
    temp_xgb_pred = temp_pred_xgb,
    temp_gbm_pred = temp_pred_gbm
  )
#==================          humidity         ========================# 

hr_dummy_train_pred <-
  cbind(
    trainset_dummy_hr[, colnames(trainset_dummy_hr) %in% c("gid","id_mother","date","week") == FALSE],
    hr_rf_pred = hr_pred_train_rf,
    hr_xgb_pred = hr_pred_train_xgb,
    hr_gbm_pred = hr_pred_train_gbm
  )

hr_train_pred <-
  cbind(
    trainset_hr[, colnames(trainset_hr) %in% c("gid","id_mother","date","week") == FALSE],
    hr_rf_pred = hr_pred_train_rf,
    hr_xgb_pred = hr_pred_train_xgb,
    hr_gbm_pred = hr_pred_train_gbm
  )
# combine the test prediction
hr_dummy_test_pred <-
  cbind(
    testset_dummy_hr[, colnames(testset_dummy_hr) %in% c("gid","id_mother","date","week") == FALSE],
    hr_rf_pred = hr_pred_rf,
    hr_xgb_pred = hr_pred_xgb,
    hr_gbm_pred = hr_pred_gbm
  )

hr_test_pred <-
  cbind(
    testset_hr[, colnames(testset_hr) %in% c("gid","id_mother","date","week") == FALSE],
    hr_rf_pred = hr_pred_rf,
    hr_xgb_pred = hr_pred_xgb,
    hr_gbm_pred = hr_pred_gbm
  )
#############-----------------------------------------------------###############
#############                          Meta                       ###############
#############-----------------------------------------------------###############

#=====================================================#
#                  Random Forest Model                #
#=====================================================#

#==================          temperature         ========================# 

temp_rf_meta <- rf_fine_tune(temp_train_pred, "indoor_temp", nthreads)
cat(capture.output(print(temp_rf_meta$bestTune)), file = paste0(path_out,"Temperature/temp_model_results_log.txt"), append = TRUE)


# predict using the train set
temp_pred_train_rf_meta <- predict(temp_rf_meta)

# predict using the test set
temp_pred_rf_meta <- predict(temp_rf_meta, newdata = temp_test_pred)

saveRDS(temp_rf_meta, file = paste0(path_out,"Temperature/train_DEML_finalmod_rf_meta.rds"))

#==================          humidity         ========================# 

hr_rf_meta <- rf_fine_tune(hr_train_pred, "indoor_hr", nthreads)
cat(capture.output(print(hr_rf_meta$bestTune)), file = paste0(path_out,"Humidity/hr_model_results_log.txt"), append = TRUE)


# predict using the train set
hr_pred_train_rf_meta <- predict(hr_rf_meta)

# predict using the test set
hr_pred_rf_meta <- predict(hr_rf_meta, newdata = hr_test_pred)

saveRDS(hr_rf_meta, file = paste0(path_out,"Humidity/train_DEML_finalmod_rf_meta.rds"))


#=====================================================#
#                    XGBoost Model                    #
#=====================================================#

#==================          temperature         ========================# 

temp_train_xgb_meta <-
  data.matrix(temp_dummy_train_pred[, colnames(temp_dummy_train_pred) %in% c("gid","id_mother","date","week","id_week","indoor_temp") == FALSE])
temp_train_label_meta <-
  data.matrix(temp_dummy_train_pred[, "indoor_temp"])

temp_test_xgb_meta <-
  data.matrix(temp_dummy_test_pred[, colnames(temp_dummy_test_pred) %in% c("gid","id_mother","date","week","id_week","indoor_temp") == FALSE])
temp_test_label_meta <-
  data.matrix(temp_dummy_test_pred[, "indoor_temp"])

temp_xgb_meta <- xgb_fine_tune(temp_dummy_train_pred, "indoor_temp", nthreads)
cat(capture.output(print(temp_xgb_meta$bestTune)), file = paste0(path_out,"Temperature/temp_model_results_log.txt"), append = TRUE)


# predict using the train set
temp_pred_train_xgb_meta <- predict(temp_xgb_meta, newdata = temp_train_xgb_meta)

# predict using the test set
temp_pred_xgb_meta <- predict(temp_xgb_meta, newdata = temp_test_xgb_meta)

saveRDS(temp_xgb_meta, file = paste0(path_out,"Temperature/train_DEML_finalmod_xgb_meta.rds"))

#==================          humidity         ========================# 

hr_train_xgb_meta <-
  data.matrix(hr_dummy_train_pred[, colnames(hr_dummy_train_pred) %in% c("gid","id_mother","date","week","id_week","indoor_hr") == FALSE])
hr_train_label_meta <-
  data.matrix(hr_dummy_train_pred[, "indoor_hr"])

hr_test_xgb_meta <-
  data.matrix(hr_dummy_test_pred[, colnames(hr_dummy_test_pred) %in% c("gid","id_mother","date","week","id_week","indoor_hr") == FALSE])
hr_test_label_meta <-
  data.matrix(hr_dummy_test_pred[, "indoor_hr"])

hr_xgb_meta <- xgb_fine_tune(hr_dummy_train_pred, "indoor_hr", nthreads)
cat(capture.output(print(hr_xgb_basic$bestTune)), file = paste0(path_out,"Humidity/hr_model_results_log.txt"), append = TRUE)


# predict using the train set
hr_pred_train_xgb_meta <- predict(hr_xgb_meta, newdata = hr_train_xgb_meta)

# predict using the test set
hr_pred_xgb_meta <- predict(hr_xgb_meta, newdata = hr_test_xgb_meta)

saveRDS(hr_xgb_meta, file = paste0(path_out,"Humidity/train_DEML_finalmod_xgb_meta.rds"))

#=====================================================#
#                     GLM Model                       #
#=====================================================#

#==================          temperature         ========================# 
temp_dummy_train_pred_glm <- temp_dummy_train_pred %>% select(-id_week)
temp_dummy_test_pred_glm <- temp_dummy_test_pred %>% select(-id_week)

set.seed(1234)
temp_glm_meta <- glm(
  indoor_temp ~ .,
  family = gaussian,
  data = temp_dummy_train_pred_glm
)

# predict using the train set
temp_pred_train_glm_meta <-
  stats::predict(temp_glm_meta, temp_dummy_train_pred_glm, type = "response")

# predict using the test set
temp_pred_glm_meta <-
  stats::predict(temp_glm_meta, temp_dummy_test_pred_glm)

save(temp_glm_meta, file = paste0(path_out,"Temperature/train_DEML_finalmod_glm_meta.rds"))


#==================          humidity         ========================# 
hr_dummy_train_pred_glm <- hr_dummy_train_pred %>% select(-id_week)
hr_dummy_test_pred_glm <- hr_dummy_test_pred %>% select(-id_week)

set.seed(1234)
hr_glm_meta <- glm(
  indoor_hr ~ .,
  family = gaussian,
  data = hr_dummy_train_pred_glm
)

# predict using the train set
hr_pred_train_glm_meta <-
  stats::predict(hr_glm_meta, hr_dummy_train_pred_glm, type = "response")

# predict using the test set
hr_pred_glm_meta <-
  stats::predict(hr_glm_meta, hr_dummy_test_pred_glm)

save(hr_glm_meta, file = paste0(path_out,"Humidity/train_DEML_finalmod_glm_meta.rds"))


#================================================================================

# combine meta model results

#==================          temperature         ========================# 

temp_train_pred_meta <-
  as.matrix(x = data.table(temp_pred_rf_meta = temp_pred_train_rf_meta,
                           temp_pred_xgb_meta = temp_pred_train_xgb_meta,
                           temp_pred_glm_meta = temp_pred_train_glm_meta))

temp_test_pred_meta <-
  as.matrix(x = data.table(temp_pred_rf_meta = temp_pred_rf_meta,
                           temp_pred_xgb_meta = temp_pred_xgb_meta,
                           temp_pred_glm_meta = temp_pred_glm_meta))


#==================          humidity         ========================# 

hr_train_pred_meta <-
  as.matrix(x = data.table(hr_pred_rf_meta = hr_pred_train_rf_meta,
                           hr_pred_xgb_meta = hr_pred_train_xgb_meta,
                           hr_pred_glm_meta = hr_pred_train_glm_meta))

hr_test_pred_meta <-
  as.matrix(x = data.table(hr_pred_rf_meta = hr_pred_rf_meta,
                           hr_pred_xgb_meta = hr_pred_xgb_meta,
                           hr_pred_glm_meta = hr_pred_glm_meta))


#############-----------------------------------------------------###############
#############    using NNLS to obtain the weights of meta models  ###############
#############-----------------------------------------------------###############

#==================          temperature         ========================# 

temp_y = as.matrix(temp_train_pred[, "indoor_temp"])

temp_x = as.matrix(x = data.table(temp_train_pred_meta))
temp_nnls_weight <- nnls::nnls(A = temp_x, b = temp_y)
# extract weights 
weights <- temp_nnls_weight$x
cat("weights: ", paste(round(weights, 4), collapse = ", "), "\n",
    file = paste0(path_out, "Temperature/temp_model_results_log.txt"),
    append = TRUE)

temp_deml_train <- data.frame(temp_deml_train = temp_x %*% temp_nnls_weight$x)


# nnls for testing data
temp_x_new = temp_test_pred_meta

temp_deml_test <- data.frame(temp_deml_test = temp_x_new %*% temp_nnls_weight$x)

temp_all_train <-
  data.frame(
    data.table(
      temp_rf_pred = temp_pred_train_rf,
      temp_xgb_pred = temp_pred_train_xgb,
      temp_gbm_pred = temp_pred_train_gbm,
      temp_train_pred_meta,
      temp_deml_train
    ))%>%
  bind_cols(trainset_temp[, c("gid",
                              "id_mother",
                              "date",
                              "indoor_temp")])

temp_all_test <- data.frame(
  data.table(
    temp_rf_pred = temp_pred_rf,
    temp_xgb_pred = temp_pred_xgb,
    temp_gbm_pred = temp_pred_gbm,
    temp_test_pred_meta,
    temp_deml_test
  )
) %>%
  bind_cols(testset_temp[, c("gid",
                             "id_mother",
                             "date",
                             "indoor_temp")])

# save performance
write_csv(temp_all_test, paste0(path_out,"Temperature/temp_perf_DEML_weekly.csv")) 
write_csv(temp_all_train, paste0(path_out,"Temperature/temp_perf_DEML_train.csv")) 

# save the train and test database for the variability plot
temp_all_test <- temp_all_test %>%
  rename(temp_deml = temp_deml_test)

temp_all_train <- temp_all_train %>%
  rename(temp_deml = temp_deml_train)
temp_indoor_model_data <- rbind(temp_all_train,temp_all_test)
save(temp_indoor_model_data, file = paste0(path_out,"Temperature/temp_indoor_pre_obs.rds"))
save(temp_all_test, file = paste0(path_out,"Temperature/indoor_temp_test.rds"))


#==================          humidity         ========================# 


hr_y = as.matrix(hr_train_pred[, "indoor_hr"])

hr_x = as.matrix(x = data.table(hr_train_pred_meta))
hr_nnls_weight <- nnls::nnls(A = hr_x, b = hr_y)
# extract weights 
weights <- hr_nnls_weight$x
cat("weights: ", paste(round(weights, 4), collapse = ", "), "\n",
    file = paste0(path_out, "Humidity/hr_model_results_log.txt"),
    append = TRUE)

hr_deml_train <- data.frame(hr_deml_train = hr_x %*% hr_nnls_weight$x)

# nnls for testing data
hr_x_new = hr_test_pred_meta

hr_deml_test <- data.frame(hr_deml_test = hr_x_new %*% hr_nnls_weight$x)

hr_all_train <-
  data.frame(
    data.table(
      hr_rf_pred = hr_pred_train_rf,
      hr_xgb_pred = hr_pred_train_xgb,
      hr_gbm_pred = hr_pred_train_gbm,
      hr_train_pred_meta,
      hr_deml_train
    ))%>%
  bind_cols(trainset_hr[, c("gid",
                            "id_mother",
                            "date",
                            "indoor_hr")])

hr_all_test <- data.frame(
  data.table(
    hr_rf_pred = hr_pred_rf,
    hr_xgb_pred = hr_pred_xgb,
    hr_gbm_pred = hr_pred_gbm,
    hr_test_pred_meta,
    hr_deml_test
  )
) %>%
  bind_cols(testset_hr[, c("gid",
                           "id_mother",
                           "date",
                           "indoor_hr")])

# save performance
write_csv(hr_all_test, paste0(path_out,"Humidity/hr_perf_DEML_weekly.csv")) 
write_csv(hr_all_train, paste0(path_out,"Humidity/hr_perf_DEML_train.csv")) 


# save the train and test database for the variability plot
hr_all_test <- hr_all_test %>%
  rename(hr_deml = hr_deml_test)

hr_all_train <- hr_all_train %>%
  rename(hr_deml = hr_deml_train)
hr_indoor_model_data <- rbind(hr_all_train,hr_all_test)
save(hr_indoor_model_data, file = paste0(path_out,"Humidity/hr_indoor_pre_obs.rds"))
save(hr_all_test, file = paste0(path_out,"Humidity/indoor_hr_test.rds"))
#################################################################################
# End