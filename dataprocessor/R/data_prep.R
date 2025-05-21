data_prep <- function() {

  # Load necessary libraries
  library(dplyr)
  library(stringr)
  library(purrr)

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

  # One-hot encoding of sgRNA sequences
  one_hot_encode_seq <- function(seq) {
    if (nchar(seq) != 20) stop("Sequence must be 20 nucleotides")
    bases <- c("A", "C", "G", "T")
    split_seq <- strsplit(seq, "")[[1]]
    mat <- sapply(bases, function(base) as.integer(split_seq == base))
    as.vector(t(mat))
  }

  # Apply one-hot encoding
  one_hot_matrix <- full_data$seq %>%
    map(one_hot_encode_seq) %>%
    do.call(rbind, .)

  # Convert matrix to a data frame and name the columns
  colnames(one_hot_matrix) <- paste0("base_", seq_len(ncol(one_hot_matrix)))
  one_hot_df <- as.data.frame(one_hot_matrix)

  # Simplify dataset to only have variables used for the predictive model
  ml_data <- full_data %>%
    dplyr::select(sgrna, Gene, gc_content, LFC, log_fpkm, start_position,
                  end_position, strand, transcript_biotype, gene_biotype)

  # Add sequence data
  ml_data <- bind_cols(ml_data, one_hot_df)

  # Save data
  save(ml_data, file = "data/ml_data.rda")

}
