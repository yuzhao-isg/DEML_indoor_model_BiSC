#-----------------------------------------------------------------------------#
#                Weekly DEML helper functions for model tuning                 #
#-----------------------------------------------------------------------------#
# Description:
# This file contains the model-tuning helpers used by the weekly DEML validation
# workflow. It tunes and returns the base learners used in the ensemble: random
# forest, XGBoost, and gradient boosting machine. The tuning is performed with
# spatial-temporal cross-validation based on the participant-week grouping.
#
# This file does not contain project-specific paths or sensitive machine details.
#-----------------------------------------------------------------------------#

make_spacetime_cv <- function(data, k = 10, seed = 1234) {
  CreateSpacetimeFolds(data, spacevar = "id_week", k = k, seed = seed)
}

# Random forest (RF)
rf_fine_tune <- function(data, outcomevar, nthreads) {
  predictors <- setdiff(names(data), c(outcomevar, "gid", "id_mother", "date", "week", "id_week"))

  cat("Fine-tuning RF parameters...\n")

  ranger_grid <- expand.grid(
    mtry = seq(2, round(length(predictors) * 3 / 4), 4),
    min.node.size = c(5, 10, 15),
    splitrule = "variance"
  )

  cv_index <- make_spacetime_cv(data)

  rf_tuned <- train(
    x = data[, predictors],
    y = unlist(data[, outcomevar]),
    method = "ranger",
    num.trees = 100,
    seed = 1234,
    num.threads = nthreads,
    metric = "RMSE",
    tuneGrid = ranger_grid,
    preProcess = NULL,
    trControl = trainControl(method = "cv", index = cv_index$index)
  )

  cat("Best mtry value selected: ", rf_tuned$bestTune$mtry, "\n")
  cat("Best min.node.size value selected: ", rf_tuned$bestTune$min.node.size, "\n")

  cat("Fitting final model...\n")

  final_grid <- expand.grid(
    mtry = rf_tuned$bestTune$mtry,
    min.node.size = rf_tuned$bestTune$min.node.size,
    splitrule = "variance"
  )

  finalmod_rf <- train(
    x = data[, predictors],
    y = unlist(data[, outcomevar]),
    method = "ranger",
    num.trees = 500,
    seed = 1234,
    num.threads = nthreads,
    metric = "RMSE",
    importance = "permutation",
    keep.inbag = TRUE,
    quantreg = TRUE,
    tuneGrid = final_grid,
    trControl = trainControl(method = "none")
  )

  return(finalmod_rf)
}

# XGBoost
xgb_fine_tune <- function(data, outcomevar, nthreads) {
  xgb_data <- data.matrix(data[, !(colnames(data) %in% c("gid", "id_mother", "date", "week", "id_week", outcomevar))])
  xgb_label <- as.numeric(as.character(data[[outcomevar]]))

  cat("Fine-tuning XGBoost parameters...\n")

  param_grid <- expand.grid(
    nrounds = c(100, 300, 500),
    max_depth = c(3, 5, 7),
    eta = c(0.01, 0.05, 0.1),
    gamma = c(0.1, 0.5),
    colsample_bytree = c(0.6, 0.8),
    min_child_weight = c(2, 4, 6),
    subsample = c(0.6, 0.8)
  )

  cv_index <- make_spacetime_cv(data)

  xgb_tuned <- train(
    x = xgb_data,
    y = xgb_label,
    method = "xgbTree",
    seed = 1234,
    nthread = nthreads,
    trControl = trainControl(method = "cv", index = cv_index$index),
    tuneGrid = param_grid
  )

  cat("Best max_depth value selected: ", xgb_tuned$bestTune$max_depth, "\n")
  cat("Best eta value selected: ", xgb_tuned$bestTune$eta, "\n")

  cat("Fitting final model...\n")

  final_grid <- expand.grid(
    nrounds = xgb_tuned$bestTune$nrounds,
    max_depth = xgb_tuned$bestTune$max_depth,
    eta = xgb_tuned$bestTune$eta,
    gamma = xgb_tuned$bestTune$gamma,
    colsample_bytree = xgb_tuned$bestTune$colsample_bytree,
    min_child_weight = xgb_tuned$bestTune$min_child_weight,
    subsample = xgb_tuned$bestTune$subsample
  )

  finalmod_xgb <- train(
    x = xgb_data,
    y = xgb_label,
    method = "xgbTree",
    seed = 1234,
    trControl = trainControl(method = "none"),
    nthread = nthreads,
    tuneGrid = final_grid
  )

  return(finalmod_xgb)
}

# Gradient boosting machine (GBM)
gbm_fine_tune <- function(data, outcomevar, nthreads) {
  predictors <- setdiff(names(data), c("gid", "id_mother", "date", "week", "id_week", outcomevar))
  gbm_data <- as.data.frame(data[, predictors])
  gbm_label <- data[[outcomevar]]

  cat("Fine-tuning GBM parameters...\n")

  tune_grid <- expand.grid(
    n.trees = c(100, 300, 500, 1000),
    interaction.depth = c(1, 3, 5),
    shrinkage = c(0.01, 0.05, 0.1),
    n.minobsinnode = c(10, 20)
  )

  cv_index <- make_spacetime_cv(data)

  gbm_tuned <- caret::train(
    x = gbm_data,
    y = gbm_label,
    method = "gbm",
    tuneGrid = tune_grid,
    trControl = trainControl(method = "cv", index = cv_index$index),
    metric = "RMSE"
  )

  cat("Best interaction.depth value selected: ", gbm_tuned$bestTune$interaction.depth, "\n")
  cat("Best shrinkage value selected: ", gbm_tuned$bestTune$shrinkage, "\n")
  cat("Best n.minobsinnode value selected: ", gbm_tuned$bestTune$n.minobsinnode, "\n")

  cat("Fitting final model...\n")

  final_grid <- expand.grid(
    n.trees = gbm_tuned$bestTune$n.trees,
    interaction.depth = gbm_tuned$bestTune$interaction.depth,
    shrinkage = gbm_tuned$bestTune$shrinkage,
    n.minobsinnode = gbm_tuned$bestTune$n.minobsinnode
  )

  set.seed(1234)

  finalmod_gbm <- caret::train(
    x = gbm_data,
    y = gbm_label,
    method = "gbm",
    tuneGrid = final_grid,
    trControl = trainControl(method = "none"),
    verbose = FALSE,
    metric = "RMSE"
  )

  return(finalmod_gbm)
}

# End