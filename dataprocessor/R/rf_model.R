rf_model <- function() {

  # Load libraries
  library(dplyr)
  library(caret)
  library(xgboost)
  library(Metrics)

  # Load in data (if necessary)
  load_if_missing("ml_data", "data/ml_data.rda")

  # Set seed for reproducibility
  set.seed(123)

  # Select input data to use
  input_data <- ml_data %>%
    dplyr::select(LFC, gc_content, log_fpkm, start_position, end_position,
                  strand, dplyr::starts_with("base"))

  # Create training indices
  train_index <- createDataPartition(ml_data$LFC, p = 0.7, list = FALSE)

  # Split data
  train_data <- input_data[train_index, ]
  test_data  <- input_data[-train_index, ]

  # Create model matrices
  X <- as.matrix(train_data %>% dplyr::select(-LFC))
  y <- train_data$LFC

  summary(X)
  summary(y)

  X_test <- as.matrix(test_data %>% dplyr::select(-LFC))
  y_test <- test_data$LFC

  dtrain <- xgb.DMatrix(data = X, label = y)

  # Perform parameter tuning
  grid <- expand.grid(
    max_depth = c(4, 6, 8),
    colsample_bynode = c(0.4, 0.6, 0.8),
    subsample = c(0.6, 0.8, 1.0),
    num_parallel_tree = c(100, 300, 500)
  )

  best_rmse <- Inf
  best_model <- NULL
  best_params <- list()

  for (i in 1:nrow(grid)) {
    params <- list(
      objective = "reg:squarederror",
      eta = 1,
      max_depth = grid$max_depth[i],
      num_parallel_tree = grid$num_parallel_tree[i],
      colsample_bynode = grid$colsample_bynode[i],
      subsample = grid$subsample[i],
      tree_method = "hist",
      verbosity = 0
    )

  # Fit random forest model
    model <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = 1,
      evals = list(train = dtrain),
      verbose = 0
    )

    preds <- predict(model, X_test)
    rmse_val <- rmse(y_test, preds)

    if (rmse_val < best_rmse) {
      best_rmse <- rmse_val
      best_model <- model
      best_params <- params
    }
  }

  cat("Best RMSE:", best_rmse, "\n")
  print(best_params)

  # Make final predictions with best model
  final_preds <- predict(best_model, X_test)

  # Print a comparison of actual vs. predicted values (first few rows)
  comparison <- data.frame(
    Expected = round(y_test, 3),
    Predicted = round(final_preds, 3)
  )

  cat("\nSample of Expected vs. Predicted values:\n")
  print(head(comparison, 10))

  # Store results into a variable
  results <- list(model = best_model, predicted = preds, expected = y_test)
  saveRDS(results, file = "data/results.rds")

  return(results)

}
