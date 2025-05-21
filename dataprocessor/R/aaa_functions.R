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
    dplyr::select(seq = 3, gene = 2, lfc = 6, fdr = 13) |>
    dplyr::filter(lfc < lfc_threshold, fdr < fdr_threshold) |>
    dplyr::arrange(fdr)

  # View the output
  print(head(essential_sgrnas))

  return(essential_sgrnas)
}

#' Load data files if not already loaded into environment
#'
#' @param object_name The name of the variable the data set is stored in
#' @param file_path The file path for the data set
#'
#' @return A message detailing whether the data was just loaded in or already existed in the environment
#' @export
#'
#' @examples load_if_missing("my_data_set", "data/mds.rda")


load_if_missing <- function(object_name, file_path) {
  if (!exists(object_name, envir = .GlobalEnv)) {
    load(file_path, envir = .GlobalEnv)
    message(paste("Loaded", object_name, "from", file_path))
  } else {
    message(paste(object_name, "already exists in environment."))
  }
}
