# Load libraries
library(dplyr)
library(caret)
library(xgboost)
library(Metrics)
library(ggplot2)

# Load in data (if necessary)
# load_if_missing("ml_data", "data/ml_data.rda")
# SLURM version: load directly
load(file='~/ml_data.rda')

# Set seed for reproducibility
set.seed(123)

# Select input data to use
input_data <- ml_data %>%
  dplyr::select(LFC, gc_content, log_fpkm, start_position, end_position,
                strand, dplyr::starts_with("base"))

# Plot LFC to check distribution
ggplot(ml_data, aes(x = LFC)) +
  geom_histogram(bins = 50, fill = "lightblue", color = "black") +
  labs(
    title = "LFC Distribution",
    x = "LFC",
    y = "Count"
  ) +
  theme_minimal()

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

# Function for R2
rsq <- function(actual, predicted) {
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  return(1 - ss_res / ss_tot)
}

# Perform parameter tuning
# Decrease the max depth to max out at 8 unless
# you have high RAM/you hate yourself
grid <- expand.grid(
  max_depth = c(10, 12, 14),
  colsample_bynode = c(0.4, 0.6, 0.8),
  subsample = c(0.6, 0.8, 1.0),
  num_parallel_tree = c(500, 1000, 1500)
)

best_rmse <- Inf
best_r2 <- -Inf
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
  mae_val <- mae(y_test, preds)
  r2_val <- rsq(y_test, preds)

  if (r2_val > best_r2) {
    best_r2 <- r2_val
    best_rmse <- rmse_val
    best_mae <- mae_val
    best_model <- model
    best_params <- params
  }
}

cat("RMSE:", best_rmse, "\n")
print(best_params)
cat("MAE :", round(best_mae, 4), "\n")
cat("R²  :", round(best_r2, 4), "\n")

# Make final predictions with best model
final_preds <- predict(best_model, X_test)

# Print a comparison of actual vs. predicted values (first few rows)
comparison <- data.frame(
  Expected = round(y_test, 3),
  Predicted = round(final_preds, 3)
)

cat("\nSample of Expected vs. Predicted values:\n")
print(head(comparison, 10))

# Calculate model importance
importance <- xgb.importance(model = best_model)
print(importance)
xgb.plot.importance(importance)


# Store results into a variable
results <- list(model = best_model, predicted = preds, expected = y_test,
                importance = importance, r2 = best_r2, mae = best_mae,
                rmse = best_rmse)
saveRDS(results, file = "~/results.rds")
