#' Find essential sgRNAs
#'
#' @param data A defined Excel sheet with sgRNA screen data using readxl
#' @param lfc_threshold The threshold of log fold change (LFC) to be considered essential; defaults to -1
#' @param fdr_threshold The threshold of False Discovery Rate (FDR) to be considered statistically significant; defaults to 0.05
#'
#' @return A dataset of sgRNAs considered essential. Includes sgRNA ID, gene name, LFC & FDR. The head of the dataset is printed.
#' @export
#'
#' @examples find_essential_genes(data_file)
#' @examples find_essential_genes(data_file, lfc_threshold = -2, fdr_threshold = 0.01)




find_essential_sgrnas <- function(data, lfc_threshold = -1, fdr_threshold = 0.05) {
  essential_sgrnas <- data |>
    dplyr::select(sgrna = 1, gene = 2, lfc = 5, fdr = 12) |>
    dplyr::filter(lfc < lfc_threshold, fdr < fdr_threshold) |>
    dplyr::arrange(fdr)

  # View the output
  print(head(essential_sgrnas))
}

