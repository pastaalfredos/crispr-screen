model_eval <- function() {

  library(ggplot2)
  library(SHAPforxgboost)
  library(corrplot)
  library(data.table)

  # Load in the results from trained model
  results <- readRDS("data/results.rds")
  preds <- results$predicted
  expected <- results$expected

  plot_data <- data.frame(Expected = expected, Predicted = preds)

  ggplot(plot_data, aes(x = Expected, y = Predicted)) +
    geom_point(alpha = 0.6, color = "#2C3E50") +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    theme_minimal() +
    labs(
      title = "Predicted vs. Expected LFC",
      x = "Expected LFC",
      y = "Predicted LFC"
    )

  # Residual plot
  residuals <- preds - expected

  ggplot(data = data.frame(Predicted = preds, Residuals = residuals),
         aes(x = Predicted, y = Residuals)) +
    geom_point(alpha = 0.4, color = "steelblue") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(title = "Residuals vs Predicted",
         x = "Predicted LFC",
         y = "Residuals")

  # Calculate SHAP values
  shap_values <- shap.values(
    xgb_model = results$model, X_train = results$X_train)

  # Convert shap_score to data.table
  shap_dt <- as.data.table(shap_values$shap_score)

  # Drop BIAS column if it exists (often the last column)
  if (ncol(shap_dt) == ncol(results$X_train) + 1) {
    shap_dt <- shap_dt[, 1:ncol(results$X_train), with = FALSE]
  }

  # Set correct column names
  setnames(shap_dt, colnames(results$X_train))

  # Check column match
  stopifnot(all(names(shap_dt) == colnames(results$X_train)))

  shap_long <- shap.prep(shap_contrib = shap_dt, X_train = results$X_train)

  # Plot summary
  shap.plot.summary(shap_long)

  # Correlation heatmap
  corrplot(corr_matrix, method = "color", tl.cex = 0.6, number.cex = 0.5,
           type = "lower", order = "hclust", diag = FALSE,
           col = colorRampPalette(c("blue", "white", "red"))(200),
           addCoef.col = "black")
}
