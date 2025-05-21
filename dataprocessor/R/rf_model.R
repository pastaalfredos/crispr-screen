rf_model <- function() {

  # Load libraries
  library(dplyr)
  library(caret)

  # Load in data (if necessary)
  load_if_missing("ml_data", "data/ml_data.rda")

  # Set seed for reproducibility
  set.seed(123)

  # Create training indices
  train_index <- createDataPartition(ml_data$LFC, p = 0.7, list = FALSE)

  # Split data
  train_data <- ml_data[train_index, ]
  test_data  <- ml_data[-train_index, ]

  # Select predictors
  predictors <- train_data %>%
    dplyr::select(gc_content, log_fpkm, start_position, end_position,
                  strand)
  seq_predictors <- train_data %>%
    dplyr::select(dplyr::starts_with("base"))

  # Create model matrices
  X <- as.matrix(seq_predictors)
  y <- train_data$LFC

  dtrain <- xgb.DMatrix(data = X, label = y)

  # Set parameters
  params <- list(
    objective = "reg:squarederror",
    eta = 1,
    max_depth = 10,
    num_parallel_tree = 1000,
    subsample = 0.8,
    colsample_bynode = 0.8,
    tree_method = "hist",
    verbosity = 1
  )

  # Fit random forest model
  xgb_rf_model <- xgb.train(
    params,
    data = dtrain,
    nrounds = 1,
  )


  preds <- predict(xgb_rf_model, X)
  head(preds)
  head(y)

}
