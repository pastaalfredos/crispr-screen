# Load necessary libraries
library(dplyr)
library(stringr)

# Function to load data files (if not already loaded)
load_if_missing <- function(object_name, file_path) {
  if (!exists(object_name, envir = .GlobalEnv)) {
    load(file_path, envir = .GlobalEnv)
    message(paste("Loaded", object_name, "from", file_path))
  } else {
    message(paste(object_name, "already exists in environment."))
  }
}

# Load in all the data (if necessary)
load_if_missing("avg_expr_data", "data/avg_expr_data.rda")
load_if_missing("annotations_unique", "data/annotation_data.rda")
load_if_missing("merged_data", "data/merged_data.rda")

# Match annotation data with the sgRNA data
full_data <- merged_data %>%
  left_join(annotations_unique, by = c("Gene" = "external_gene_name"))

# Match with RNA-seq data
full_data <- full_data %>%
  left_join(avg_expr_data, by = c("ensembl_gene_id" = "geneName"))

# Add GC content as a column
full_data <- full_data %>%
  mutate(gc_content = str_count(seq, "[GCgc]") / str_length(seq))

# Omit rows with NA values
full_data <- na.omit(full_data)

# Save the data into the project
save(full_data, file = "data/full_data.rda")

# Simplify dataset to only have variables used for the predictive model
ml_data <- full_data %>%
  dplyr::select(sgrna, Gene, seq, gc_content, LFC, log_fpkm, start_position,
         end_position, strand, transcript_biotype, gene_biotype)
save(ml_data, file = "data/ml_data.rda")
