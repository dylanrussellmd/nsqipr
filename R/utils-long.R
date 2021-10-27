#' Convert columns to a wide format
#'
#' This is a complex function that accepts specified lists of columns and melts them
#' into a long format from a wide format. This is designed for those columns in the original
#' NSQIP dataset that are stored in a wide format, such as the CPT, readmission and
#' reoperation columns; this is not meant for columns that store data in a comma-separated
#' fashion (such as \code{anesthes_other}).
#'
#' @param df a data.table containing the columns in a wide format
#' @param ... character vectors of column names that will be melted together into single columns.
#' @param variable.name desired output variable name; this is typically the numbering variable (optional)
#' @param na.cols character vector of column names indicating which records
#' should be removed when NA values are present in these columns (optional).
#' @param removeFALSE character vector of a column name indicating which records
#' should be removed when FALSE values are present in this column (optional).
#' @param reorder logical vector indicating whether records should be reordered after removal
#' of NA and/or FALSE records. Will reorder the \code{variable.name} column.
#' @param fn a function that can be used for processing the dataset after melting
#'
#' @details The \code{...} parameter accepts multiple character vectors that should each contain
#' the names of columns that will be melted together to create a single column in the long format
#' output. For example, a character vector named "reoperation" will contain the names "reoperation1",
#' "reoperation2", and "reoperation3", indicating these three columns in the original dataset will
#' be melted into a single column called "reoperation" in the long format.
#'
#' The \code{removeFALSE} character vector should only specify a single logical column.
#'
#' The \code{fn} parameter allows an anonymous function to be passed that operates on the melted
#' data frame. It applies this function prior to any NA or FALSE columns have been removed and
#' before the \code{variable.name} has been reordered (if desired).
#'
#' @importFrom tibble lst
#' @return data.table
#' @seealso make_commas_long
#'
#' @keywords internal
#'
make_cols_long <- function(df, ..., variable.name, na.cols, removeFALSE, reorder = FALSE, fn) {

  measure.vars <- lst(...) # lst is used because it automatically names the elements of the list

  if(check_if_in(c(...), df)) {

    for(j in setdiff(c(...), names(df))) data.table::set(df, j = j, value = NA) # If any of the provided columns don't exist in the data frame already, add them filled with NA.

    if(rlang::is_missing(variable.name)) variable.name <- paste("n", names(measure.vars)[[1]], sep = "") # If a specific name isn't provided, the enumerating column is the first column name appended to "n".

    melted <- suppressWarnings(data.table::melt(df, id.vars = "caseid",
                                                measure.vars =  measure.vars, # a list where each element is a character vector of column names that are melted into one column
                                                variable.name = variable.name,
                                                value.name = names(measure.vars),
                                                variable.factor = FALSE,
                                                value.factor = TRUE))

    if(!rlang::is_missing(fn)) fn(melted) # Allow a user-defined anonymous function to be applied to the data frame before removing or reordering variables.
    if(!rlang::is_missing(na.cols)) melted <- na.omit(melted, na.cols) # Omit rows with NA values in the selected columns.
    if(!rlang::is_missing(removeFALSE)) melted <- melted[melted[[removeFALSE]], ] # Remove rows with FALSE in the selected column.
    if(reorder) melted[, (variable.name) := data.table::rowid(caseid)] else convert_col_to_integer(melted, variable.name) # After either NAs, FALSEs, or both are removed, reorder the variable.name in sequential order, grouped by caseid.

    data.table::setorder(melted, caseid) # Orders the table by caseid

    return(melted)
  }
}

#' Convert comma-separated columns to a wide format
#'
#' This is a complex function that accepts a column containing multiple comma-separated values and
#' converts it into a long format. This is designed for those columns in the original
#' NSQIP dataset that store multiple values in a single column separated by commas,
#' such as the \code{anesthes_other} column; this is not meant for columns that store data in a wide
#' format (such as the reoperation, readmission, and CPT columns).
#'
#' @param df a data.table containing the comma-separated column.
#' @param cols character vectors of column names that contain the comma-separated values.
#' @param variable.name desired output variable name (optional).
#' @param levels levels for factoring the resulting long table (optional).
#'
#' @details The \code{cols} parameter should be a single character vector of a length equal to the number
#' of possible unique values in the comma-separated column. The contents of the vector are the name of the
#' comma-separated column with "n" appended, where "n" is equal to the number of the column. The symbol should
#' be named the same as the comma-separated column.
#'
#' For example, in the original NSQIP, \code{anesthes_other} is a comma-separated column. It may contain
#' a value like "General,Spinal,MAC/IV". There are 8 possible unique values that may exist in the
#' comma-separated column. So \code{cols} is passed a character vector named \code{anesthes_other}
#' that is made up of 8 values - "anesthes_other1", "anesthes_other2", etc.
#'
#' Because the usual method of factoring columns in the dataframe (via \code{conv_factor}) is not possible
#' for the new columns generated in this long table (because they are created within this function long after
#' \code{conv_factor} has been called), they must be factored here. If a factor output is desired, the levels
#' must be passed as a list to the \code{levels} parameter.
#'
#' @return data.table
#' @seealso make_cols_long
#'
#' @keywords internal
#'
make_commas_long <- function(df, cols, variable.name, levels) {
  value.name <- deparse(substitute(cols)) # The value.name is set equal to the name of the object containing the columns

  if(check_if_in(value.name, df)) {

    make_long_cols(df, value.name, cols) # Create comma separated columns in data frame

    if(rlang::is_missing(variable.name)) variable.name <- paste("n", value.name, sep = "")

    melted <- suppressWarnings(data.table::melt(df, id.vars = "caseid",
                                                measure.vars = cols,
                                                variable.name = variable.name,
                                                value.name = value.name,
                                                na.rm = TRUE, # Rows with NA anesthes_other are removed after melting to long
                                                variable.factor = FALSE))

    if(!rlang::is_missing(levels)) data.table::set(melted, j  = value.name, value = melted[[value.name]] %^% levels) # Factor with provided levels, if desired
    extract_number_from_column(melted, variable.name) # Extracts number from column name
    data.table::setorder(melted, caseid) # Orders the table by caseid

    return(melted)
  }
}

#' Check if columns in data frame
#'
#' Conducts a quick case-insensitive search using \code{%qsin%} of provided column names
#' and returns TRUE if any of these columns are present in the provided data frame.
#'
#' @param cols a character vector of column names
#' @param df a data.table
#'
#' @return a logical vector
#' @seealso %qsin%
#'
#' @keywords internal
#'
check_if_in <- function(cols, df) {
  any(cols %qsin% names(df))
}

#' Extract number from value
#'
#' This function takes a character vector of the form 'name1' where '1' may be any number.
#' It extracts the number and resets that character vector to just the number. It then
#' converts the number to an integer.
#'
#' @param df a data.table
#' @param col column from which to extract a number
#'
#' @keywords internal
#'
extract_number_from_column <- function(df, col) {
  data.table::set(df, j = col, value = stringi::stri_extract_all_regex(df[[col]], "\\d+", simplify = TRUE))
  convert_col_to_integer(df, col)
}

#' Convert a column to integers
#'
#' Converts a column of character vectors to integer vectors.
#'
#' @param df a data.table
#' @param col column to convert to integer
#'
#' @keywords internal
#'
convert_col_to_integer <- function(df, col) {
  data.table::set(df, j = col, value = as.integer(df[[col]]))
}

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
  mat <- stringi::stri_split_regex(df[[from]], ",\\s?", simplify = NA, n = length(to), omit_empty = TRUE, opts_regex = list(case_insensitive = TRUE))
  for(j in seq_along(to)) data.table::set(df, j = to[[j]], value = mat[, j])
}

# Works so far on:
# reop - X
# readm - X
# anesthes_other - X
# amylase
# cpt - X
# hep_con_ablation
# hep_invasive_type
# hep_neotherapy
# pan_percdrainage

