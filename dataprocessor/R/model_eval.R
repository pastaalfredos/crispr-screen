model_eval <- function() {

  library(ggplot2)

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

}
