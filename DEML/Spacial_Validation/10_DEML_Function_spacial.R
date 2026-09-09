#-----------------------------------------------------------------------------#
#                  Spatial validation DEML helper functions                  #
#-----------------------------------------------------------------------------#
# Description:
# This script contains the tuning functions used by the spatial DEML workflow.
# Models are tuned with participant-level spatial folds and returned for the
# temperature and humidity ensemble predictions.
#
# Outputs:
# - trained RF, XGBoost, SVM, and GBM model objects
#-----------------------------------------------------------------------------#

# Random forest (RF)
rf_fine_tune <- function(data, outcomevar, nthreads){
  # Candidate covariates
  predictors <- setdiff(names(data), c(outcomevar,"gid","id_mother","date","week","id_week"))
  
  # Finetune mtry
  cat("Fine-tuning RF parameters...\n")
  
  # Ensure predictors and target are properly formatted
  
  rangerGrid <- expand.grid(
    mtry = seq(2, round(length(predictors)*3/4), 4),
    min.node.size = c(5, 10, 15),
    splitrule = "variance"
  )
  cvindinces <- CreateSpacetimeFolds(data, spacevar = "id_mother", k = 10, seed = 1234)
  rf_tuned <- train(data[,predictors], unlist(data[,outcomevar]), 
                    method = "ranger", num.trees = 100, seed = 1234,
                    num.threads = nthreads, metric = "RMSE",
                    tuneGrid = rangerGrid,
                    preProcess = NULL,  # Ensure no preprocessing is applied
                    trControl = trainControl(method = "cv", index = cvindinces$index))
  # Print the best mtry value and min.node.size
  cat("Best mtry value selected: ", rf_tuned$bestTune$mtry, "\n")
  cat("Best min.node.size value selected: ", rf_tuned$bestTune$min.node.size, "\n")
  
  # Fitting final model with inbag OOB, and variable importance
  cat("Fitting final model...")
  rangerGrid <- expand.grid(mtry = rf_tuned$bestTune$mtry,
                            min.node.size = rf_tuned$bestTune$min.node.size, splitrule = "variance")
  
  finalmod_rf <- train(data[,predictors], unlist(data[,outcomevar]), method="ranger",
                       num.trees=500, seed=1234,
                       num.threads=nthreads, metric="RMSE",
                       importance="permutation", keep.inbag=T, quantreg=T,
                       tuneGrid=rangerGrid,
                       trControl=trainControl(method = "none"))
  return(finalmod_rf)
}


# XGBoost
xgb_fine_tune <- function(data, outcomevar, nthreads){
  # Candidate covariates
  xgb_data <-
    data.matrix(data[, colnames(data) %in% c("gid","id_mother","date","week","id_week", outcomevar) == FALSE])
  
  xgb_label <-
    data.matrix(data[, outcomevar])
  
  # Finetune
  cat("Fine-tuning XGBoost parameters...\n")
  param_grid <- expand.grid(
    nrounds = c(100, 300, 500),
    max_depth = c(3, 5, 7),
    eta = c(0.01, 0.05,0.1),
    gamma = c(0.1, 0.5),             # Minimum loss reduction
    colsample_bytree = c(0.6, 0.8), # Fraction of features used per tree
    min_child_weight = c(2,4,6),     # Minimum instance weight per child
    subsample =  c(0.6, 0.8)         # Sub sampling ratio
  )
  cvindinces <- CreateSpacetimeFolds(data, spacevar = "id_mother", k = 10, seed = 1234)
  xgb_label <- as.numeric(as.character(xgb_label))
  
  xgb_tuned <- train(
    x = xgb_data,                   # Feature matrix
    y = xgb_label,                  # Target variable
    method = "xgbTree",
    seed = 1234,
    nthread = nthreads,
    trControl = trainControl(method = "cv", index = cvindinces$index),
    tuneGrid = param_grid)
  # Print the best max_depth value, eta, and nrounds
  cat("Best max_depth value selected: ", xgb_tuned$bestTune$max_depth, "\n")
  cat("Best eta value selected: ", xgb_tuned$bestTune$eta, "\n")
  
  # Fitting final model with inbag OOB, and variable importance
  cat("Fitting final model...")
  
  param_grid <- expand.grid(
    nrounds = xgb_tuned$bestTune$nrounds,
    max_depth = xgb_tuned$bestTune$max_depth,
    eta = xgb_tuned$bestTune$eta,
    gamma = xgb_tuned$bestTune$gamma,             # Minimum loss reduction
    colsample_bytree = xgb_tuned$bestTune$colsample_bytree, # Fraction of features used per tree
    min_child_weight = xgb_tuned$bestTune$min_child_weight,     # Minimum instance weight per child
    subsample =  xgb_tuned$bestTune$subsample         # Subsampling ratio
  )
  
  finalmod_xgb <- train(
    x = xgb_data,                   # Feature matrix
    y = xgb_label,                 # Target variable
    method = "xgbTree",
    seed = 1234,
    trControl = trainControl(method = "none"),
    nthread = nthreads,
    tuneGrid = param_grid)
  
  return(finalmod_xgb)
}


# Support vector machine (SVM)
svm_fine_tune <- function(data, outcomevar, nthreads){
  
  # Remove the columns that are not involved in the modeling
  vars_to_remove <- c("gid", "id_mother", "date", "week","id_week", outcomevar)
  vars_to_use <- setdiff(colnames(data), vars_to_remove)
  
  x_data <- data[, vars_to_use]
  
  # Standardize (centralize + scale) all features
  pre_proc <- preProcess(x_data, method = c("center", "scale"))
  x_data <- predict(pre_proc, newdata = x_data)
  
  y_data <- data[[outcomevar]]
  # Define tuning grid for SVM with RBF kernel
  # Finetune
  cat("Fine-tuning SVM parameters...\n")
  cvindinces <- CreateSpacetimeFolds(data, spacevar = "id_mother", k = 10, seed = 1234)
  tune_grid <- expand.grid(
    sigma = c(0.001,0.01, 0.1),   # Gamma (automatically calculated for RBF)
    C = c(0.01, 0.1, 1, 10)          # Cost parameter
  )
  
  svm_tuned <- train(
    x = x_data,
    y = y_data,
    seed = 1234,
    method = "svmRadial",
    tuneGrid = tune_grid,
    nthread = nthreads,
    trControl = trainControl(method = "cv", index = cvindinces$index),
    metric = "RMSE"  # For classification; use RMSE for regression
  )
  # Print the best sigma, and C
  cat("Best sigma value selected: ", svm_tuned$bestTune$sigma, "\n")
  cat("Best C value selected: ", svm_tuned$bestTune$C, "\n")
  
  tune_grid <- expand.grid(
    sigma = svm_tuned$bestTune$sigma,   # Gamma (automatically calculated for RBF)
    C = svm_tuned$bestTune$C          # Cost parameter
  )
  
  finalmod_svm <- train(
    x = x_data,
    y = y_data,
    seed = 1234,
    method = "svmRadial",
    nthread = nthreads,
    tuneGrid = tune_grid,
    trControl = trainControl(method = "none"),
    metric = "RMSE"  # For classification; use RMSE for regression
  )
  
  return(finalmod_svm)
}

# Gradient boosting machine (GBM)
gbm_fine_tune <- function(data, outcomevar, nthreads) {
  # Ensure target variable and predictors are properly formatted
  
  # Exclude unwanted columns (e.g., "gid", "id_mother", "date", "week")
  predictors <- setdiff(names(data), c("gid", "id_mother", "date", "week","id_week", outcomevar))
  
  # Convert predictors and target into matrices
  gbm_data <- as.data.frame(data[, predictors])  # Predictor variables as a data frame
  gbm_label <- data[[outcomevar]]            # Target variable as a numeric vector
  
  # Define a grid of hyperparameters for tuning
  tune_grid <- expand.grid(
    n.trees = c(100, 300, 500,1000),        # Number of trees
    interaction.depth = c(1, 3, 5),   # Depth of each tree
    shrinkage = c(0.01, 0.05, 0.1),   # Learning rate
    n.minobsinnode = c(10, 20)        # Minimum number of observations in terminal nodes
  )
  cvindinces <- CreateSpacetimeFolds(data, spacevar = "id_mother", k = 10, seed = 1234)
  # Train GBM model with cross-validation
  cat("Fine-tuning GBM parameters...\n")
  gbm_tuned <- caret::train(
    x = gbm_data,                     # Feature matrix
    y = gbm_label,                    # Target variable
    method = "gbm",                   # Gradient Boosting Machine
    tuneGrid = tune_grid,             # Grid of hyperparameters
    trControl = trainControl(
      method = "cv",                  # Cross-validation
      index = cvindinces$index
    ),
    metric = "RMSE"                   # Use RMSE for regression problems
  )
  # Print the best interaction.depth, shrinkage, and n.minobsinnode
  cat("Best interaction.depth value selected: ", gbm_tuned$bestTune$interaction.depth, "\n")
  cat("Best shrinkage value selected: ", gbm_tuned$bestTune$shrinkage, "\n")
  cat("Best n.minobsinnode value selected: ", gbm_tuned$bestTune$n.minobsinnode, "\n")
  
  # Fitting final model with inbag OOB, and variable importance
  cat("Fitting final model...")
  
  tune_grid <- expand.grid(
    n.trees = gbm_tuned$bestTune$n.trees,        # Number of trees
    interaction.depth = gbm_tuned$bestTune$interaction.depth,   # Depth of each tree
    shrinkage = gbm_tuned$bestTune$shrinkage,   # Learning rate
    n.minobsinnode = gbm_tuned$bestTune$n.minobsinnode        # Minimum number of observations in terminal nodes
  )
  
  set.seed(1234)
  
  finalmod_gbm <- caret::train(
    x = gbm_data,                     # Feature matrix
    y = gbm_label,                    # Target variable
    method = "gbm",                   # Gradient Boosting Machine
    tuneGrid = tune_grid,             # Grid of hyperparameters
    trControl = trainControl(method = "none"),
    verbose = FALSE,
    metric = "RMSE"                  # Use RMSE for regression problems
  )
  
  return(finalmod_gbm)
  
}

