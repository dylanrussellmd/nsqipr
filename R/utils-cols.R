#' Collect all column names
#'
#' @param files list of files from which to collect column names
#'
#' @details Reads the first line of each input file to collect column names.
#' Creates a character vector and selects only unique entries after converting
#' to lower case. This results in a vector of all possible column names.
#'
#' @keywords internal
#' @return a character vector
#'
collect_column_names <- function(files) {
  files %>%
    lapply(., get_headers) %>%
    unlist() %>%
    unique()
}

#' Collect column names
#'
#' @param file a file to collect names from
#'
#' @details Reads the first line of an input file to collect column names.
#' Creates a character vector and converts to lower case.
#'
#' @keywords internal
#' @return a character vector
#' @describeIn collect_column_names
#'
get_headers <- function(file) {
  data.table::fread(file, nrows = 0, data.table = FALSE, showProgress = FALSE) %>%
    names() %>%
    stringi::stri_trans_tolower()
}
