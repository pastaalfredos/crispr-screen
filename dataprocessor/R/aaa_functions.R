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
