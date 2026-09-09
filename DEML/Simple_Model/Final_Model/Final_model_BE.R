#-----------------------------------------------------------------------------#
#                    Final BE model - weekly DEML                            #
#-----------------------------------------------------------------------------#
# Description:
# This script trains the final indoor temperature and humidity DEML models for
# the BE analysis. It loads precomputed base-model predictions, trains the
# meta-models, combines them with NNLS weights, and saves the final outputs.
# Outputs:
# - results/Model/Final_model_weekly_BE/Temperature/*
# - results/Model/Final_model_weekly_BE/Humidity/*
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
path_temp <- Sys.getenv(
  "DEML_TRAIN_PREDICTOR_DIR",
  unset = file.path(project_root, "results", "Model", "Temporal_Validation_weekly_40", "")
)
path_out <- file.path(project_root, "results", "Model", "Final_model_weekly_BE", "")
path_mod <- file.path(project_root, "db", "output", "Model")

for (subdir in c("Temperature", "Humidity")) {
  dir.create(file.path(path_out, subdir), recursive = TRUE, showWarnings = FALSE)
}

source(file.path(project_root, "script", "indoor_temp", "DEML", "Temporal_longterm_validation", "10_DEML_Function_weekly.R"))

# Load the database.
load(file.path(path_mod, "temp_preidctors_data_model.RData"))

# select the predictors
temp_hr_model <- temp_preidctors_data_model %>%
  mutate(across(c("year","month","yday"), as.factor),
         id_week = paste0(id_mother, "_", week)) %>%
  dplyr::select("gid", "id_week", "id_mother", "date","sr","apavg","wind_speed_10m",
         "wind_direction_10m","precipitation", "ndvi_300", "temperature_min_home",
         "temperature_mean_home", "temperature_max_home", "humidity_mean_home","hum_mean_lag1_avg",
         "temp_mean_lag1_avg", "indoor_temp","indoor_hr","year","month","yday","week")

trainset_temp <- temp_hr_model %>% select(-indoor_hr)
trainset_hr <- temp_hr_model
nthreads <- detectCores()
################################################################################
# # except random forest, other models needs to set dummy variables for factor variables
# 
# # Define variables to exclude from dummy conversion
vars_no_convert <- c("gid", "id_mother", "week", "date", "id_week")
# 
# # Extract variables to convert
train_x <- temp_hr_model[, !(names(temp_hr_model) %in% c("gid", "id_mother", "week", "date", "id_week","indoor_temp", "indoor_hr"))]
all_year_levels <- as.character(2018:2025)
train_x$year <- factor(train_x$year, levels = all_year_levels)
all_yday_levels <- as.character(1:366)
train_x$yday <- factor(train_x$yday, levels = all_yday_levels)
dummy_model <- dummyVars(~ ., data = train_x)
# 
# # Apply to trainset 
train_dummy <- predict(dummy_model, newdata = train_x) |> as.data.frame()
train_final <- cbind(temp_hr_model[, c("gid", "id_mother", "week", "date", "id_week","indoor_temp","indoor_hr")],
                     train_dummy)

trainset_dummy_temp <- train_final %>% select(-indoor_hr)
trainset_dummy_hr <- train_final

# 
# #====================
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
# 
#========================================================================
#############-----------------------------------------------------###############
#############                     Fit the  Model                  ###############
#############-----------------------------------------------------###############


#=====================================================#
#                  Random Forest Model                #
#=====================================================#

#==================       train for temperature         ========================#
temp_rf_basic <- rf_fine_tune(trainset_temp, "indoor_temp", nthreads)
cat(capture.output(print(temp_rf_basic$bestTune)), file = paste0(path_out,"Temperature/temp_model_results_log.txt"), append = TRUE)

# predict using the train set
temp_pred_train_rf <- predict(temp_rf_basic)
saveRDS(temp_rf_basic, file = paste0(path_out,"Temperature/train_DEML_finalmod_rf.rds"))  # Save the best model

#==================         train for humidity         ========================#
hr_rf_basic <- rf_fine_tune(trainset_hr, "indoor_hr", nthreads)
cat(capture.output(print(hr_rf_basic$bestTune)), file = paste0(path_out,"Humidity/hr_model_results_log.txt"), append = TRUE)

# predict using the train set
hr_pred_train_rf <- predict(hr_rf_basic)

saveRDS(hr_rf_basic, file = paste0(path_out,"Humidity/train_DEML_finalmod_rf.rds"))  # Save the best model

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

# predict using the train set
temp_pred_train_xgb <- predict(temp_xgb_basic, newdata = temp_train_xgb)

saveRDS(temp_xgb_basic, file = paste0(path_out,"Temperature/train_DEML_finalmod_xgb.rds"))  # Save the best model

#==================         train for humidity         ========================#

hr_xgb_basic <- xgb_fine_tune(trainset_dummy_hr, "indoor_hr", nthreads)
cat(capture.output(print(hr_xgb_basic$bestTune)), file = paste0(path_out,"Humidity/hr_model_results_log.txt"), append = TRUE)

hr_train_xgb <-
  data.matrix(trainset_dummy_hr[, colnames(trainset_dummy_hr) %in% c("gid","id_mother","date","week","id_week","indoor_hr") == FALSE])
hr_train_label <-
  data.matrix(trainset_dummy_hr[, "indoor_hr"])

# predict using the train set
hr_pred_train_xgb <- predict(hr_xgb_basic, newdata = hr_train_xgb)

saveRDS(hr_xgb_basic, file = paste0(path_out,"Humidity/train_DEML_finalmod_xgb.rds"))  # Save the best model

#=====================================================#
#             Gradient Boosting Machine               #
#=====================================================#

#==================       train for temperature         ========================#

temp_gbm_basic <- gbm_fine_tune(trainset_dummy_temp, "indoor_temp", nthreads)
cat(capture.output(print(temp_gbm_basic$bestTune)), file = paste0(path_out,"Temperature/temp_model_results_log.txt"), append = TRUE)

# predict using the train set
temp_pred_train_gbm <- predict(temp_gbm_basic)

saveRDS(temp_gbm_basic, file = paste0(path_out,"Temperature/train_DEML_finalmod_gbm.rds"))  # Save the best model

#==================         train for humidity         ========================#
hr_gbm_basic <- gbm_fine_tune(trainset_dummy_hr, "indoor_hr", nthreads)
cat(capture.output(print(hr_gbm_basic$bestTune)), file = paste0(path_out,"Humidity/hr_model_results_log.txt"), append = TRUE)

# predict using the train set
hr_pred_train_gbm <- predict(hr_gbm_basic)

saveRDS(hr_gbm_basic, file = paste0(path_out,"Humidity/train_DEML_finalmod_gbm.rds"))  # Save the best model

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

saveRDS(
  temp_dummy_train_pred,
  file = paste0(path_temp, "Temperature/temp_dummy_train_pred.rds")
)
saveRDS(
  temp_train_pred,
  file = paste0(path_temp, "Temperature/temp_train_pred.rds")
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

saveRDS(
  hr_dummy_train_pred,
  file = paste0(path_temp, "Humidity/hr_dummy_train_pred.rds")
)
saveRDS(
  hr_train_pred,
  file = paste0(path_temp, "Humidity/hr_train_pred.rds")
)

#############-----------------------------------------------------###############
#############                          Meta                       ###############
#############-----------------------------------------------------###############
temp_dummy_train_pred <- readRDS(
  paste0(path_temp, "Temperature/temp_dummy_train_pred.rds")
)
temp_train_pred <- readRDS(
  paste0(path_temp, "Temperature/temp_train_pred.rds")
  )
hr_dummy_train_pred <- readRDS(
  paste0(path_temp, "Humidity/hr_dummy_train_pred.rds")
  )
hr_train_pred <- readRDS(paste0(path_temp, "Humidity/hr_train_pred.rds")
                         )


#=====================================================#
#                  Random Forest Model                #
#=====================================================#

#==================          temperature         ========================# 

temp_rf_meta <- rf_fine_tune(temp_train_pred, "indoor_temp", nthreads)
cat(capture.output(print(temp_rf_meta$bestTune)), file = paste0(path_out,"Temperature/temp_model_results_log.txt"), append = TRUE)


# predict using the train set
temp_pred_train_rf_meta <- predict(temp_rf_meta)

saveRDS(temp_rf_meta, file = paste0(path_out,"Temperature/train_DEML_finalmod_rf_meta.rds"))

#==================          humidity         ========================# 

hr_rf_meta <- rf_fine_tune(hr_train_pred, "indoor_hr", nthreads)
cat(capture.output(print(hr_rf_meta$bestTune)), file = paste0(path_out,"Humidity/hr_model_results_log.txt"), append = TRUE)

# predict using the train set
hr_pred_train_rf_meta <- predict(hr_rf_meta)

saveRDS(hr_rf_meta, file = paste0(path_out,"Humidity/train_DEML_finalmod_rf_meta.rds"))


#=====================================================#
#                    XGBoost Model                    #
#=====================================================#

#==================          temperature         ========================# 

temp_train_xgb_meta <-
  data.matrix(temp_dummy_train_pred[, colnames(temp_dummy_train_pred) %in% c("gid","id_mother","date","week","id_week","indoor_temp") == FALSE])
temp_train_label_meta <-
  data.matrix(temp_dummy_train_pred[, "indoor_temp"])

temp_xgb_meta <- xgb_fine_tune(temp_dummy_train_pred, "indoor_temp", nthreads)
cat(capture.output(print(temp_xgb_meta$bestTune)), file = paste0(path_out,"Temperature/temp_model_results_log.txt"), append = TRUE)

# predict using the train set
temp_pred_train_xgb_meta <- predict(temp_xgb_meta, newdata = temp_train_xgb_meta)

saveRDS(temp_xgb_meta, file = paste0(path_out,"Temperature/train_DEML_finalmod_xgb_meta.rds"))

#==================          humidity         ========================# 

hr_train_xgb_meta <-
  data.matrix(hr_dummy_train_pred[, colnames(hr_dummy_train_pred) %in% c("gid","id_mother","date","week","id_week","indoor_hr") == FALSE])
hr_train_label_meta <-
  data.matrix(hr_dummy_train_pred[, "indoor_hr"])

hr_xgb_meta <- xgb_fine_tune(hr_dummy_train_pred, "indoor_hr", nthreads)
cat(capture.output(print(hr_xgb_basic$bestTune)), file = paste0(path_out,"Humidity/hr_model_results_log.txt"), append = TRUE)

# predict using the train set
hr_pred_train_xgb_meta <- predict(hr_xgb_meta, newdata = hr_train_xgb_meta)

saveRDS(hr_xgb_meta, file = paste0(path_out,"Humidity/train_DEML_finalmod_xgb_meta.rds"))

#=====================================================#
#                     GLM Model                       #
#=====================================================#

#==================          temperature         ========================# 
temp_dummy_train_pred_glm <- temp_dummy_train_pred %>% select(-id_week)

set.seed(1234)
temp_glm_meta <- glm(
  indoor_temp ~ .,
  family = gaussian,
  data = temp_dummy_train_pred_glm
)

# predict using the train set
temp_pred_train_glm_meta <-
  stats::predict(temp_glm_meta, temp_dummy_train_pred_glm, type = "response")

save(temp_glm_meta, file = paste0(path_out,"Temperature/train_DEML_finalmod_glm_meta.rds"))


#==================          humidity         ========================# 
hr_dummy_train_pred_glm <- hr_dummy_train_pred %>% select(-id_week)

set.seed(1234)
hr_glm_meta <- glm(
  indoor_hr ~ .,
  family = gaussian,
  data = hr_dummy_train_pred_glm
)

# predict using the train set
hr_pred_train_glm_meta <-
  stats::predict(hr_glm_meta, hr_dummy_train_pred_glm, type = "response")

save(hr_glm_meta, file = paste0(path_out,"Humidity/train_DEML_finalmod_glm_meta.rds"))


#================================================================================

# combine meta model results

#==================          temperature         ========================# 

temp_train_pred_meta <-
  as.matrix(x = data.table(temp_pred_rf_meta = temp_pred_train_rf_meta,
                           temp_pred_xgb_meta = temp_pred_train_xgb_meta,
                           temp_pred_glm_meta = temp_pred_train_glm_meta))


#==================          humidity         ========================# 

hr_train_pred_meta <-
  as.matrix(x = data.table(hr_pred_rf_meta = hr_pred_train_rf_meta,
                           hr_pred_xgb_meta = hr_pred_train_xgb_meta,
                           hr_pred_glm_meta = hr_pred_train_glm_meta))

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
write_csv(temp_all_train, paste0(path_out,"Temperature/temp_perf_DEML_train.csv")) 

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
write_csv(hr_all_train, paste0(path_out,"Humidity/hr_perf_DEML_train.csv")) 
#################################################################################
# End