# Set CRAN repo as global
options(repos = c(CRAN = "https://cloud.r-project.org"))

# List of required packages
required_packages <- c(
  "dplyr", "readxl", "purrr", "caret", "xgboost", "Metrics", "ggplot2",
  "shiny", "bslib", "tidyverse", "glmnet", "RSQLite",
  "pkgdown", "pkgload", "rmarkdown"
)

# Install any missing packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing:", pkg))
    install.packages(pkg, dependencies = TRUE)
  } else {
    message(paste("Already installed:", pkg))
  }
}

invisible(lapply(required_packages, install_if_missing))

# biomaRt installation
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("biomaRt")
BiocManager::install("GEOquery")
