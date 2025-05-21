seq_merge <- function() {

  # Load libraries to read and manipulate data
  library(readxl)
  library(readr)
  library(dplyr)
  library(stringr)

  load_data <- function(dropout_path, a_path, b_path) {

    # Read the datasets
    dropout_data <- read_excel(dropout_path, sheet = "Dropout")
    gecko_a <- read_csv(a_path)
    gecko_b <- read_csv(b_path)

    return(list(
      dropout_data = dropout_data,
      gecko_a = gecko_a,
      gecko_b = gecko_b
    ))
  }

  merge_data <- function(dropout_data, gecko_a, gecko_b) {

    # Create new column that tracks A or B
    dropout_data <- dropout_data %>%
      mutate(lib = if_else(str_starts(sgrna, "HGLibA"), "A", "B"))

    # Separate into two datasets based on belonging to A or B
    # Then join with respective Gecko table
    dropout_a <- dropout_data %>%
      filter(lib == "A") %>%
      left_join(gecko_a, by = c("sgrna" = "UID"))

    dropout_b <- dropout_data %>%
      filter(lib == "B") %>%
      left_join(gecko_b, by = c("sgrna" = "UID"))

    # Combine the tables again
    merged_data <- bind_rows(dropout_a, dropout_b)
    # Reorder the columns and remove duplicates for a cleaner table
    merged_data <- merged_data %>%
      dplyr::select(-lib, -gene_id, -'...17') %>%
      dplyr::select(sgrna, Gene, seq, everything())


    return(merged_data)
  }

  # Set file path to datasets
  dropout_path <- "data/13059_2020_1940_MOESM3_ESM.xlsx"
  a_path <- "data/human_geckov2_library_a_09mar2015.csv"
  b_path <- "data/human_geckov2_library_b_09mar2015.csv"

  # Load datasets and unpack into their own variables
  data_list <- load_data(dropout_path, a_path, b_path)
  list2env(data_list, envir = .GlobalEnv)

  # Merge the data sets
  merged_data <- merge_data(dropout_data, gecko_a, gecko_b)

  # Save the data into the project
  save(merged_data, file = "data/merged_data.rda")

}
