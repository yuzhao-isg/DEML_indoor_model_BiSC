#-----------------------------------------------------------------------------#
#                  Short-term daily DEML helper functions                     #
#-----------------------------------------------------------------------------#
# Description:
# This script contains the tuning functions used by the short-term daily DEML
# workflow. It prepares the spatial-temporal folds, tunes the base learners, and
# returns the trained models used to build the ensemble predictions.
#
# Outputs:
# - trained RF, XGBoost, and GBM model objects
#-----------------------------------------------------------------------------#

make_daily_cv <- function(data, n_folds = 5, seed = 1234) {
  data <- data %>% mutate(row_id = row_number())
  set.seed(seed)

  folds <- map(seq_len(n_folds), function(i) {
    val_index <- data %>%
      group_by(id_mother, week) %>%
      filter(n() >= 5) %>%
      sample_n(1) %>%
      ungroup() %>%
      pull(row_id)

    train_index <- setdiff(seq_len(nrow(data)), val_index)
    list(train = train_index, val = val_index)
  })

  list(
    index = lapply(folds, `[[`, "train"),
    indexOut = lapply(folds, `[[`, "val")
  )
}

# Random forest (RF)
rf_fine_tune <- function(data, outcomevar, nthreads) {
  data <- data %>% mutate(row_id = row_number())
  predictors <- setdiff(names(data), c(outcomevar, "gid", "id_mother", "date", "week", "id_week", "row_id"))

  cat("Fine-tuning RF parameters...\n")

  ranger_grid <- expand.grid(
    mtry = seq(2, round(length(predictors) * 3 / 4), 4),
    min.node.size = c(5, 10, 15),
    splitrule = "variance"
  )

  cv_index <- make_daily_cv(data)

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
  data <- data %>% mutate(row_id = row_number())
  xgb_data <- data.matrix(data[, !(colnames(data) %in% c("gid", "id_mother", "date", "week", "row_id", "id_week", outcomevar))])
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

  cv_index <- make_daily_cv(data)

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

# Support vector machine (SVM)
svm_fine_tune <- function(data, outcomevar, nthreads) {
  data <- data %>% mutate(row_id = row_number())
  vars_to_remove <- c("gid", "id_mother", "date", "week", "row_id", "id_week", outcomevar)
  vars_to_use <- setdiff(colnames(data), vars_to_remove)

  x_data <- data[, vars_to_use]
  pre_proc <- preProcess(x_data, method = c("center", "scale"))
  x_data <- predict(pre_proc, newdata = x_data)

  y_data <- data[[outcomevar]]

  cat("Fine-tuning SVM parameters...\n")

  tune_grid <- expand.grid(
    sigma = c(0.001, 0.01, 0.1),
    C = c(0.01, 0.1, 1, 10)
  )

  cv_index <- make_daily_cv(data)

  svm_tuned <- train(
    x = x_data,
    y = y_data,
    seed = 1234,
    method = "svmRadial",
    tuneGrid = tune_grid,
    nthread = nthreads,
    metric = "RMSE",
    trControl = trainControl(method = "cv", index = cv_index$index)
  )

  cat("Best sigma value selected: ", svm_tuned$bestTune$sigma, "\n")
  cat("Best C value selected: ", svm_tuned$bestTune$C, "\n")

  final_grid <- expand.grid(
    sigma = svm_tuned$bestTune$sigma,
    C = svm_tuned$bestTune$C
  )

  finalmod_svm <- train(
    x = x_data,
    y = y_data,
    seed = 1234,
    method = "svmRadial",
    nthread = nthreads,
    tuneGrid = final_grid,
    trControl = trainControl(method = "none"),
    metric = "RMSE"
  )

  return(finalmod_svm)
}

# Gradient boosting machine (GBM)
gbm_fine_tune <- function(data, outcomevar, nthreads) {
  data <- data %>% mutate(row_id = row_number())
  predictors <- setdiff(names(data), c("gid", "id_mother", "date", "week", "row_id", "id_week", outcomevar))

  gbm_data <- as.data.frame(data[, predictors])
  gbm_label <- data[[outcomevar]]

  cat("Fine-tuning GBM parameters...\n")

  tune_grid <- expand.grid(
    n.trees = c(100, 300, 500, 1000),
    interaction.depth = c(1, 3, 5),
    shrinkage = c(0.01, 0.05, 0.1),
    n.minobsinnode = c(10, 20)
  )

  cv_index <- make_daily_cv(data)

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
#   # Prepare data
#   xgb_data <- data.matrix(data[, !colnames(data) %in% c("gid", "id_mother", "date", "week", outcomevar)])
#   xgb_label <- as.numeric(as.character(data[[outcomevar]]))
#   
#   cat("Advanced XGBoost tuning with L1/L2 regularization...\n")
#   
#   # Create DMatrix
#   dtrain <- xgb.DMatrix(data = xgb_data, label = xgb_label)
#   
#   # Aggressive regularization grid
#   param_combinations <- expand.grid(
#     max_depth = c(4, 5, 6),
#     eta = c(0.01, 0.05),
#     min_child_weight = c(2, 5),
#     subsample = c(0.7, 0.8),
#     reg_alpha = c(1, 2, 5),
#     reg_lambda = c(1, 2, 5),
#     colsample_bytree = c(0.7, 0.8),
#     gamma = c(0.1, 0.5)
#   )
#   
#   # Sample smaller grid for efficiency
#   set.seed(1234)
#   n_sample <- min(50, nrow(param_combinations))
#   param_sample <- param_combinations[sample(nrow(param_combinations), n_sample), ]
#   
#   cat("Testing", n_sample, "parameter combinations with strong regularization...\n")
#   
#   best_rmse <- Inf
#   best_params <- NULL
#   best_nrounds <- NULL
#   
#   for (i in 1:nrow(param_sample)) {
#     params <- list(
#       objective = "reg:squarederror",
#       max_depth = param_sample$max_depth[i],
#       eta = param_sample$eta[i],
#       min_child_weight = param_sample$min_child_weight[i],
#       subsample = param_sample$subsample[i],
#       reg_alpha = param_sample$reg_alpha[i],
#       reg_lambda = param_sample$reg_lambda[i],
#       colsample_bytree = param_sample$colsample_bytree[i],
#       gamma = param_sample$gamma[i],
#       nthread = nthreads
#     )
#     
#     # Cross-validation
#     cv_result <- xgb.cv(
#       params = params,
#       data = dtrain,
#       nrounds = 1000,
#       nfold = 10,
#       early_stopping_rounds = 50,
#       verbose = 0,
#       seed = 1234
#     )
#     
#     test_rmse <- min(cv_result$evaluation_log$test_rmse_mean)
#     train_rmse <- cv_result$evaluation_log$train_rmse_mean[cv_result$best_iteration]
#     overfitting_ratio <- test_rmse / train_rmse
#     
#     if (test_rmse < best_rmse && overfitting_ratio > 1.05 && overfitting_ratio < 2) { # Can set by yourself to define the overfitting
#       best_rmse <- test_rmse
#       best_params <- params
#       best_nrounds <- cv_result$best_iteration
#     }
#     
#     if (i %% 5 == 0) cat("Completed", i, "of", n_sample, "\n")
#   }
#   
#   cat("\nBest parameters with strong regularization:\n")
#   print(best_params)
#   cat("Best nrounds:", best_nrounds, "\n")
#   
#   # Train final model
#   final_model <- xgb.train(
#     params = best_params,
#     data = dtrain,
#     nrounds = best_nrounds
#   )
#   
#   return(final_model)
# }