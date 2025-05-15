# Load required packages
library(biomaRt)
library(dplyr)
library(stringr)

# Connect to Ensembl
ensembl <- useEnsembl(biomart = "genes", dataset = "hsapiens_gene_ensembl")

# Get the annotation data

annotations <- getBM(
  attributes = c("ensembl_gene_id", "external_gene_name", "chromosome_name",
                 "start_position", "end_position", "strand",
                 "transcript_biotype", "gene_biotype"
  ),
  mart = ensembl
)

# Avoid duplicates by only keeping unique names
annotations_unique <- annotations %>%
  group_by(external_gene_name) %>%
  slice(1) %>%
  ungroup()

# Match annotation data with the existing data
data_updated <- merged_data %>%
  left_join(annotations_unique, by = c("Gene" = "external_gene_name"))

