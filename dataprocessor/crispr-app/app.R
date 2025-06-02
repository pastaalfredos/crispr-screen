library(shiny)
library(bslib)
library(ggplot2)
library(corrplot)
library(xgboost)

# Load the data
print(getwd())
results <- readRDS("results.rds")

# Define UI ----
ui <- fluidPage(
  titlePanel("CRISPR Screen sgRNA Model Evaluation"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("top_corr", "Number of variables (sorted by importance):",
                  min = 2, max = ncol(results$X_train), value = 10),
      checkboxGroupInput(
        "selected_vars",
        "Select variables to show in Correlation Plot:",
        choices = colnames(results$X_train),
        selected = colnames(results$X_train)[1:5])
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Predicted vs Expected",
                 plotOutput("pred_vs_exp_plot")),
        tabPanel("Residual Plot", plotOutput("residual_plot")),
        tabPanel("Correlation Plot", plotOutput("corr_plot")),
        tabPanel("Variable Importance Plot", plotOutput("var_plot"))
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {

  # Most important variable calculation for corr plot
  importance <- xgb.importance(model = results$model)

  top_vars <- reactive({
    head(importance$Feature, n = input$top_corr)
    })



  # Update checkboxGroupInput based on top variables
  observe({
    updateCheckboxGroupInput(session,
                             inputId = "selected_vars",
                             choices = top_vars(),
                             selected = top_vars()
    )
  })


  # Predicted vs expected plot
  output$pred_vs_exp_plot <- renderPlot({

    plot_data <- data.frame(Expected = results$expected,
                            Predicted = results$predicted)

    ggplot(plot_data, aes(x = Expected, y = Predicted)) +
      geom_point(alpha = 0.6, color = "black") +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      theme_minimal() +
      labs(
        title = "Predicted vs. Expected LFC",
        x = "Expected LFC",
        y = "Predicted LFC"
      )
  })

  # Residual plot

  output$residual_plot <- renderPlot({

    residuals <- results$predicted - results$expected
    res_data <- data.frame(
      Predicted = results$predicted,
      Residuals = residuals)

    ggplot(res_data, aes(x = Predicted, y = Residuals)) +
      geom_point(alpha = 0.4, color = "steelblue") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(title = "Residuals vs Predicted",
           x = "Predicted LFC",
           y = "Residuals")
  })

  # Correlation plot

  output$corr_plot <- renderPlot({
    selected_vars <- input$selected_vars

  # Failsafe if too few variables
    if (length(selected_vars) < 2) {
      plot.new()
      title("Please select at least two variables for the correlation plot.")
      return()
    }

    X_selected <- results$X_train[, selected_vars, drop = FALSE]

    # Calculate correlation matrix
    corr_matrix <- cor(X_selected, use = "pairwise.complete.obs")

    # Correlation heatmap
    corrplot(corr_matrix, method = "color", tl.cex = 0.6, number.cex = 0.5,
             type = "lower", order = "hclust", diag = FALSE,
             col = colorRampPalette(c("green", "white", "red"))(200),
             addCoef.col = "black")
  })

  # Variable importance plot

  output$var_plot <- renderPlot({
    xgb.plot.importance(results$importance, top_n = input$top_corr)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
