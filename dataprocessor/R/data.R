#' sgRNA dataset from a CRISPR screen on dropout data
#'
#' @format A data frame with 118461 rows and 13 variables:
#' \describe{
#'   \item{sgrna}{sgRNA ID as assigned by the HGLib dataset}
#'   \item{Gene}{The target gene symbol}
#'   \item{GeCKO library_count}{Read count in the pre-dropout cell population.}
#'   \item{Dropout_count}{Read count in the post-dropout cell population.}
#'   \item{LFC}{Log fold change between dropout and library counts}
#'   \item{control_var}{Control variance estimate}
#'   \item{ajd_var}{Adjusted variance for each sgRNA}
#'   \item{score}{Score reflecting sgRNA essentiality or dropout significance.}
#'   \item{p.low}{Lower-tail p-value for testing dropout significance.}
#'   \item{p.high}{Upper-tail p-value for testing enrichment}
#'   \item{p.twosided}{Two-sided p-value combining p.low and p.high}
#'   \item{FDR}{False discovery rate-adjusted p-value}
#'   \item{high_in_Dropout}{Logical indicator of significant enrichment}
#' }
#'
"dropout_data"

#' Genome annotation data
#'
#' A data set containing genome annotation information of sgRNA-associated genes.
#'
#' @docType data
#' @format A data frame with 41165 rows and 8 variables:
#' \describe{
#'   \item{ensembl_gene_id}{ENSEMBL Gene ID}
#'   \item{external_gene_name}{Alternative gene name}
#'   \item{chromosome_name}{Chromosome gene appears in}
#'   \item{start_position}{Start position of gene in human genome}
#'   \item{end_position}{End position of gene in human genome}
#'   \item{strand}{Strand direction}
#'   \item{transcript_biotype}{Biotype of transcript (i.e. protein_coding, lncRNA)}
#'   \item{gene_biotype}{Biotype of gene (i.e. protein_coding, lncRNA)}
#' }
"annotations_unique"

#' RNA-seq expression data
#'
#' A data set containing normalized RNA-seq data.
#'
#' @docType data
#' @format A data frame with 64258 rows and 3 variables:
#' \describe{
#'   \item{gene_name}{ENSEMBL Gene ID}
#'   \item{mean_fpkm}{Fragments Per Kilobase Million: Mean based on 9 replicates}
#'   \item{log_fpkm}{Log transformed version of mean_fpkm (for better distribution)}
#' }
"avg_expr_data"

#' Data set for machine learning algorithm
#'
#' A data set combining all other data sets, including positional data.
#'
#' @docType data
#' @format A data frame with 99389 rows and 90 variables:
#' \describe{
#'   \item{base_1}{1st base in sgRNA sequence (one hot encoded): A}
#'   \item{base_2}{1st base in sgRNA sequence (one hot encoded): C}
#'   \item{base_3}{1st base in sgRNA sequence (one hot encoded): G}
#'   \item{base_4}{1st base in sgRNA sequence (one hot encoded): G}
#'   \item{base_77}{20th base in sgRNA sequence (one hot encoded): A}
#' }
"ml_data"
