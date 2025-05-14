#' sgRNA dataset from a CRISPR screen on dropout data
#'
#' @format A data frame with 118461 rows and 13 variables:
#' \describe{
#' \item{sgrna}{sgRNA ID}
#' \item{Gene}{Gene name}
#' \item{GeCKO library_count}{Library count}
#' }
#'
#'
#'
#'
"dropout_data"

#' Essential sgRNAs Dataset
#'
#' A dataset containing information about essential sgRNAs.
#'
#' @docType data
#' @usage data(essential_sgrnas)
#' @format A tibble with 100 rows and 4 variables:
#' \describe{
#'   \item{seq}{Sequence of the sgRNA}
#'   \item{gene}{Gene targeted by the sgRNA}
#'   \item{lfc}{Log fold change}
#'   \item{fdr}{False discovery rate}
#' }
"essential_sgrnas"
