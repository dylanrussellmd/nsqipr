#' Create multiple columns from comma-separated column
#'
#' Given a single column containing multiple comma-separated values,
#' this function will create new columns and split the comma-separated values.
#'
#' @param df the dataframe in which the current comma-separated column is stored.
#' @param from a character vector representing the name of the comma-separated column.
#' @param to a character vector of new column names into which the comma-separated column
#' will be split.
#'
#' @details This function will create new columns in the data frame but it will not remove
#' the original column (the `from` column).
#'
#' @keywords internal
#'
#' @examples
#' #' @examples
#' x <- data.table::data.table(
#' hep_con_ablation_140101 = c("Microwave ablation", "RFA ablation", "Other ablation",
#' "Microwave ablation,Other ablation", "RFA ablation,Microwave ablation",
#' "RFA ablation,Other ablation", "RFA ablation,Alcohol ablation", "Alcohol ablation",
#' "Microwave ablation,Alcohol ablation", "Cryoablation", NA, "RFA ablation,Cryoablation"))
#'
#' hep_invasive_type_cols <- paste("hep_invasive_type", 1:5, sep = "")
#'
#' make_long_cols(x, "hep_con_ablation_140101", hep_invasive_type_cols)
#'
make_long_cols <- function(df, from, to) {
  if(from %qsin% names(df)) {
    mat <- stringi::stri_split_regex(df[[from]], ",\\s?", simplify = NA, n = length(to), omit_empty = TRUE, opts_regex = list(case_insensitive = TRUE))
    for(j in seq_along(to)) data.table::set(df, j = to[[j]], value = mat[, j])
  }
}
