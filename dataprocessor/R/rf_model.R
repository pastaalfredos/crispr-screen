# Load libraries
library(dplyr)
library(xgboost)

# Uncomment to load load_if_missing function
# source("R/data_prep.R")

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
predictors <- ml_data %>%
  dplyr::select(gc_content, log_fpkm, start_position, end_position,
                strand, dplyr::starts_with("base"))

# Create model matrices
X <- as.matrix(predictors)
y <- ml_data$LFC

dtrain <- xgb.DMatrix(data = X, label = y)

# Set parameters
params <- list(
  objective = "reg:squarederror",
  eta = 1,
  max_depth = 5,
  num_parallel_tree = 100,
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
