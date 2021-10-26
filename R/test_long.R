make_cols_long <- function(df, ..., variable.name, na.cols, removeFALSE, reorder = FALSE) {

  measure.vars <- tibble::lst(...)

  if(check_if_in(c(...), df)) {

    for(j in setdiff(c(...), names(df))) data.table::set(df, j = j, value = NA)

    if(rlang::is_missing(variable.name)) variable.name <- paste("n", names(measure.vars)[[1]], sep = "")

    melted <- suppressWarnings(data.table::melt(df, id.vars = "caseid",
                                                measure.vars =  measure.vars, # a list where each element is a character vector of column names that are melted into one column
                                                variable.name = variable.name,
                                                value.name = names(measure.vars),
                                                variable.factor = FALSE,
                                                value.factor = TRUE))

    #extract_number_from_column(melted, variable.name)
    convert_col_to_integer(melted, variable.name)

    if(!rlang::is_missing(na.cols)) melted <- na.omit(melted, na.cols) # Omit rows with NA values in the selected columns.
    if(!rlang::is_missing(removeFALSE)) melted <- melted[melted[[removeFALSE]], ] # Remove rows with FALSE in the selected column.
    if(reorder) melted[, (variable.name) := data.table::rowid(caseid)] # After either NAs, FALSEs, or both are removed, reorder the variable.name in sequential order, grouped by caseid.

    data.table::setorder(melted, caseid) # Orders the table by caseid

    return(melted)
  }
}

check_if_in <- function(cols, df) {
  any(cols %qsin% names(df))
}

extract_number_from_column <- function(df, col) {
  data.table::set(df, j = col, value = stringi::stri_extract_all_regex(df[[col]], "\\d+", simplify = TRUE))
}

convert_col_to_integer <- function(df, col) {
  data.table::set(df, j = col, value = as.integer(df[[col]]))
}

# make_amylase_long <- function(df) {
#
#   melted <- make_cols_long(df, amylase = pan_amylase_cols, id.vars = c("caseid", "damylase"), variable.name = "pod", na.cols = "amylase")
#
#   data.table::set(melted, j = c("damylase","pod"), value = list(
#     ifelse(melted[["pod"]] == 1, 1, melted[["damylase"]]),
#     NULL))
#
#   return(melted)
# }

# Works so far on:
# reop
# readm
# anesthes_other
# amylase
# cpt
# hep_con_ablation
# hep_invasive_type
# hep_neotherapy
# pan_percdrainage
