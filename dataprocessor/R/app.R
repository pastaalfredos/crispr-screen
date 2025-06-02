library(shiny)
library(bslib)
library(ggplot2)
library(corrplot)

# Load the data
print(getwd())
results <- readRDS("../data/results.rds")

# Define UI ----
ui <- fluidPage(
  titlePanel("CRISPR Screen sgRNA Model Evaluation"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("top_corr", "Number of Top Correlated Variables:",
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
        tabPanel("Correlation Plot", plotOutput("corr_plot"))
      )
    )
  )
)

# Define server logic ----
server <- function(input, output) {

}

# Run the app ----
shinyApp(ui = ui, server = server)
