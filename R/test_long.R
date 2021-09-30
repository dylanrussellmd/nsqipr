make_cols_long <- function(df, ..., id.vars = "caseid", removeFALSE = FALSE) {
  vars <- rlang::ensyms(...)
  var.name <- as.character(vars[[1]])
  variable.name <- paste("n", var.name, sep = "")

  if(length(intersect(c(...), names(df))) > 0) {
    melted <- suppressWarnings(data.table::melt(df, id.vars = id.vars,
                                                measure.vars =  list(...),
                                                variable.name = variable.name,
                                                value.name = as.character(vars),
                                                variable.factor = FALSE,
                                                value.factor = TRUE))

    # Extracts the number from the variable name column
    data.table::set(melted, j = variable.name, value = stringi::stri_extract_all_regex(melted[[variable.name]], "\\d", simplify = TRUE))

    melted <- na.omit(melted, cols = var.name) # Removes all rows with NA values

    if(removeFALSE && is.logical(melted[[var.name]])) { # if removeFalse is TRUE and the var.name column is a logical
      melted <- melted[melted[[var.name]], ] # Returns only those rows where an outcome occurred (removes those where outcome is FALSE)
    }

    data.table::set(melted, j = variable.name, value = as.integer(melted[[variable.name]])) # Converts the "n" column from character to integer.
    data.table::setorder(melted, caseid) # Orders the table by caseid

    return(melted)
  }
}

# Works so far on:
# hep_con_ablation
# hep_invasive_type
# hep_neotherapy

# TODO
# - Validate this works on all long tables so far
# - Maybe add in ability to customize the column names?
