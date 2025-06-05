net_model <- function() {

  # Load libraries
  library(dplyr)
  library(caret)
  library(glmnet)
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

  # Set up parameter tuning of alpha
  alphas <- seq(0, 1, by = 0.1)
  best_rmse <- Inf
  best_model <- NULL
  best_alpha <- NA
  best_lambda <- NA
  final_preds <- NULL

  # Function for R2
  rsq <- function(actual, predicted) {
    ss_res <- sum((actual - predicted)^2)
    ss_tot <- sum((actual - mean(actual))^2)
    return(1 - ss_res / ss_tot)
  }

  # Parameter tuning of alpha
  for (a in alphas) {
    cv_model <- cv.glmnet(x = X, y = y, alpha = a, nfolds = 5,
                          type.measure = "mse", family = "gaussian")
    preds <- predict(cv_model, newx = X_test, s = "lambda.min")
    rmse_val <- rmse(y_test, preds)
    r2 <- rsq(y_test, preds)

    # Store the best model's stats
    if (rmse_val < best_rmse) {
      best_rmse <- rmse_val
      best_model <- cv_model
      best_alpha <- a
      best_lambda <- cv_model$lambda.min
      best_r2 <- r2
      final_preds <- preds
    }
  }

  cat("Best RMSE:", best_rmse, "\n")
  cat("Best alpha:", best_alpha, "\n")
  cat("Best lambda:", best_lambda, "\n")
  cat("Best R2:", best_r2, "\n")

  # Print a comparison of actual vs. predicted values (first few rows)
  comparison <- data.frame(
    Expected = round(y_test, 3),
    Predicted = round(final_preds, 3)
  )

  cat("\nSample of Expected vs. Predicted values:\n")
  print(head(comparison, 10))

  # Store results into a variable
  results <- list(model = best_model, predicted = final_preds,
                  xpected = y_test, alpha = best_alpha,
                  lambda = best_lambda, r2= best_r2, rmse = best_rmse)
  saveRDS(results, file = "data/net_results.rds")

  return(results)

}
