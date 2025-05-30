model_eval <- function() {

  library(ggplot2)
  library(corrplot)

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


  # Calculate correlation matrix
  corr_matrix <- cor(results$X_train, use = "pairwise.complete.obs")

  # Correlation heatmap
  corrplot(corr_matrix, method = "color", tl.cex = 0.6, number.cex = 0.5,
           type = "lower", order = "hclust", diag = FALSE,
           col = colorRampPalette(c("blue", "white", "red"))(200),
           addCoef.col = "black")
}
