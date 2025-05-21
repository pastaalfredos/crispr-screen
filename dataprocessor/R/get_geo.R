get_geo <- function() {

  # Load necessary libraries
  library(GEOquery)
  library(ggplot2)
  library(tidyverse)

  gse <- getGEO("GSE169614",GSEMatrix=TRUE)
  gse_data <- gse[[1]]

  # Make a folder for supplemental data and list the RNA-seq data files
  getGEOSuppFiles("GSE169614", makeDirectory = TRUE)
  files <- list.files("GSE169614", pattern = "^GSE169614_526.*\\.tab\\.gz$", full.names = TRUE)


  # Read the expression files
  read_expression_file <- function(file) {

    # Name the sample
    sample_name <- str_extract(basename(file), "526\\d+")

    # Read the file
    # Select gene name and expression
    # Rename the column after the sample name
    expr_data <- read_tsv(file, col_types = cols()) %>%
      dplyr::select(geneName, fpkm.mapped) %>%
      rename(!!sample_name := fpkm.mapped)

    return(expr_data)
  }

  expr_list <- lapply(files, read_expression_file)

  # Merge the files together based on geneName
  expr_data <- reduce(expr_list, full_join, by = "geneName")

  # Take mean expression across samples
  avg_expr_data <- expr_data %>%
    rowwise() %>%
    mutate(mean_fpkm = mean(c_across(-geneName), na.rm = TRUE)) %>%
    ungroup() %>%
    dplyr::select(geneName, mean_fpkm)

  # Plot the data to check log distribution
  ggplot(avg_expr_data, aes(x = log1p(mean_fpkm))) +
    geom_histogram(bins = 50, fill = "lightblue", color = "black") +
    labs(title = "Log-Transformed Mean FPKM Expression",
         x = "log1p(Mean FPKM)",
         y = "Count") +
    theme_minimal()

  # Add log transform data to reflect the distribution
  avg_expr_data <- avg_expr_data %>%
    mutate(log_fpkm = log1p(mean_fpkm))

  # Save the data into the project
  save(avg_expr_data, file = "data/avg_expr_data.rda")

}
