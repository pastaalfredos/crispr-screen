# Packages for machine learning model

# List of required packages
required_packages <- c("dplyr", "caret", "xgboost", "Metrics", "ggplot2")

# Identify packages that are not installed
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

# Install missing packages
if(length(missing_packages)) {
  install.packages(missing_packages)
}

# Load all required packages
invisible(lapply(required_packages, library, character.only = TRUE))


