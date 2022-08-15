#' Add missing columns
#'
#' @param df a data table
#' @param cols a character vector of all possible columns
#'
#' @details This function \bold{modifies by reference}.
#' This function compares a data tables columns to a character vector of all
#' possible column names. Any columns not present in \code{df} that are present in \code{cols}
#' are added to \code{df} with a value of \code{NA_character}.
#'
#' @keywords internal
#' @return a data table
#'
#' @examples
#' x <- data.table::data.table(a = c(1,2,3))
#' cols <- c("a","b","c")
#' nsqipr:::addmissingcolumns(x, cols)
#' x
addmissingcolumns <- function(df, cols) {
  for(j in setdiff(cols, names(df))) data.table::set(df, j=j, value=NA_character_)
}

#' Convert column names to lower case
#'
#' @param df a data table to convert names to lower case
#' @return a data table
#' @keywords internal
#'
#' @details This function \bold{modifies by reference}.
#'
#' @examples
#'
#' x <- data.table::data.table(X = NA, Y = NA, Z = NA)
#' nsqipr:::setlowernames(x)
#' names(x)
#'
setlowernames <- function(df) {
  data.table::setnames(df, stringi::stri_trans_tolower(names(df)))
}

#' Apply a given function to specified columns in a data table
#'
#' @param df a data table
#' @param cols a character vector of column names to which the function \code{f} is applied
#' @param f a function that applies to each column.
#' @param ... arguments to pass to function \code{f}
#' @param newcol a character vector of new column names to which the output of \code{f} will be applied.
#'
#' @details This function \bold{modifies by reference}. If passed a column name that does not exist in the
#' provided data table, this will simply return the original data.table unmodified without error.
#'
#' @keywords internal
#'
#' @examples
#' x <- data.table::data.table(x = rep("APPLE", 10), y = rep("BANANA", 10), z = rep("CHERRY", 10))
#' nsqipr:::conv_(x, c("x","y"), tolower)
#' x
#'
#' x <- data.table::data.table(x = rep("APPLE", 10), y = rep("BANANA", 10), z = rep("CHERRY", 10))
#' nsqipr:::conv_(x, c("x","y"), tolower, newcol =  c("X","Y"))
#' x
#'
#' x <- data.table::data.table(x = rep("APPLE", 10), y = rep("BANANA", 10), z = rep("CHERRY", 10))
#' nsqipr:::conv_(x, "x", paste, "JUICE", sep = " ", newcol = "drink")
#'
conv_ <- function(df, cols, f, ..., newcol) {
  if(missing(newcol)) {
    for(j in intersect(cols, names(df))) data.table::set(df, j = j, value = f(df[[j]], ...))
  } else {
    for(j in intersect(cols, names(df))) data.table::set(df, j = newcol[[which(cols == j)]], value = f(df[[j]], ...))
  }
  invisible(df)
}

#' Convert yes/no columns to logicals
#'
#' @param vec a character vector to convert yes/no columns
#' @return a logical vector
#'
#' @details Matches the case-insensitive fixed string \code{"yes"}. NA will return NA.
#'
#' @keywords internal
#' @examples
#'
#' x <- data.frame(x = rep("yes", 10), y = rep("YES", 10), z = rep("no", 10),
#' xx = rep("NO", 10), yy = rep(NA, 10))
#' lapply(x , nsqipr:::conv_yesno)
#'
conv_yesno <- function(vec) {
  stringi::stri_detect_fixed(vec, "yes", opts_fixed = list(case_insensitive = TRUE))
}

#' Convert "no" columns to logicals
#'
#' @param vec a character vector to convert "no" columns
#' @return a logical vector
#'
#' @details Matches the case-insensitive fixed string \code{"no"} and \code{"none"} and negates the result.
#' Any string that does not match either of these will return TRUE. NA will return NA.
#'
#' @keywords internal
#' @examples
#'
#' x <- data.frame(x = rep("no", 10), y = rep("NO", 10), z = rep("yes", 10),
#' xx = rep("", 10), yy = rep(NA, 10), zz = rep("NONE", 10))
#' lapply(x, nsqipr:::conv_notno)
#'
conv_notno <- function(vec) {
  !stringi::stri_detect_regex(vec, "^no$", opts_regex = list(case_insensitive = TRUE)) &
    !stringi::stri_detect_regex(vec, "^none$", opts_regex = list(case_insensitive = TRUE))
}

#' Convert complication columns to logicals
#'
#' @param vec a character vector to convert complication columns
#' @return a logical vector
#'
#' @details Matches the case-insensitive fixed string \code{"no complication"} and negates the result.
#' Any string that does not match will return TRUE. NA will return NA.
#'
#' @keywords internal
#' @examples
#'
#' x <- data.frame(
#' x = rep("no complication", 10),
#' y = rep("NO COMPLICATION", 10),
#' z = rep("complication", 10),
#' xx = rep("", 10),
#' yy = rep(NA, 10)
#' )
#'
#' lapply(x , nsqipr:::conv_complication)
#'
conv_complication <- function(vec) {
  !stringi::stri_detect_fixed(vec, "no complication", opts_fixed = list(case_insensitive = TRUE))
}

#' Extract number scale from vector
#'
#' @param vec a character vector to extract number scales from
#' @return an integer vector
#'
#' @details Matches the regex pattern \code{"^.*?\\d"}. NA will return NA.
#'
#' @keywords internal
#' @examples
#'
#' x <- data.frame(x = rep("1 - test", 10), y = rep("2- test", 10), z = rep("  3 test", 10),
#' xx = rep("4            5", 10), yy = rep(NA, 10))
#' lapply(x , nsqipr:::conv_numscale)
#'
conv_numscale <- function(vec) {
  as.integer(stringi::stri_extract_first_regex(vec,"^.*?\\d", opts_regex = list(case_insensitive = TRUE)))
}


#' Converts a single year to a date
#'
#' @param vec a character vector containing dates in the format "yyyy"
#' @return a Date vector
#'
#' @keywords internal
#' @examples
#'
#' x <- c("2000", "1900", "2020", "1950")
#' nsqipr:::conv_date(x)
#'
conv_date <- function(vec) {
  as.Date(ifelse(!is.na(vec),
         paste(stringi::stri_extract_all_charclass(vec, "[0-9]"), 1, 1, sep = "-"),
         NA))
}

#' Add a PUF year column
#'
#' This column notes the file from which the record came.
#'
#' @param df a data table to append the new column to
#' @param filename the file name from which the record was derived
#'
#' @details Matches the year of the file from the file name using a regular expression.
#' Requires that the original file names \bold{are not changed}.
#'
#' @return a data table with a new ordered factor column called \code{pufyear}
#'
#' @keywords internal
#'
#' @examples
#' x <- data.table::data.table(x = rep("name", 10))
#' nsqipr:::get_pufyear(x, "acs_nsqip_puf12.txt")
#' x$pufyear < "2013"
#' x$pufyear > "2005-2006"
#'
get_pufyear <- function(df, filename) {
  yrs <- factor(stringi::stri_match_last_regex(filename, ".*(\\d{2})", opts_regex = list(case_insensitive = TRUE))[,2],
                levels = c("06","07","08","09","10","11","12","13","14","15","16","17","18","19","20"),
                labels = c("2005-2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020"),
                ordered = TRUE)
  data.table::set(df, j = "pufyear", value = yrs)
}

#' Factor pipe
#'
#' A pipe that allows easy conversion of a vector into a factor with specified levels.
#' @name %^%
#' @rdname factorpipe
#' @keywords internal
#' @export
#' @usage lhs \%^\% rhs
#'
#' @param lhs a character vector to be converted into a factor vector
#' @param rhs  a named list specifying how to rename the levels
#'
#' @examples
#' nsqipr:::`%^%`(
#' c("apple","cherry","pork"),
#' list(fruit = c("apple", "banana", "cherry"), meat = c("steak", "chicken", "pork"))
#' )
#'
`%^%` <- function(lhs, rhs) {
  x <- factor(stringi::stri_replace_all_fixed(lhs,"  ", " "))
  levels(x) <- rhs
  return(x)
}

#' Convert factor columns
#'
#' @param df a data table in which to convert factor columns
#' @param factor_cols columns to be converted to a factor
#' @return a data table
#'
#' @details This function \bold{modifies by reference}.
#' This function checks for which columns to factor by comparing against a character vector called \code{factor_cols}.
#' The function then fetches a variable from the calling environment with the same name as the column being converted to a factor.
#' This variable should reference a named list specifying how to rename the levels (see \code{\link[nsqipr:factorpipe]{\%^\%}}
#' for further details).
#'
#' @keywords internal
#' @examples
#'
#' x <- data.table::data.table(foods = c("apple","banana","cherry","steak","chicken","pork"),
#'                             drinks = c("milk","water","oj","beer","vodka","rum"))
#' foods <- list(fruit = c("apple", "banana", "cherry"), meat = c("steak","chicken","pork"))
#' drinks <- list(`non-alcoholic` = c("milk","water","oj"), alcoholic = c("beer","vodka","rum"))
#' factor_cols <- c("foods", "drinks")
#' nsqipr:::conv_factor(x, factor_cols)
#' x
#'
conv_factor <- function(df, factor_cols) {
  for(j in intersect(factor_cols, names(df))) data.table::set(df, j = j, value = df[[j]] %^% get(j, envir = rlang::caller_env()))
  invisible(df)
}

#' Set data table columns to a specified order
#'
#' @param df a data table to be ordered
#' @param col_order a character vector of column names in the desired order.
#' @return a data table
#'
#' @details This function \bold{modifies by reference}. If a column that is not present is supplied
#' to \code{col_order}, it will simply skip over it without error.
#'
#' @keywords internal
#' @examples
#' x <- data.table::data.table(c = c(1,2,3), b = c(1,2,3), a = c(1,2,3))
#' col_order <- c("a","b","c")
#' nsqipr:::colorder(x, col_order)
#' identical(names(x), col_order)
#'
colorder <- function(df, col_order) {
  data.table::setcolorder(df, intersect(col_order, names(df)))
}

#' Remove undesired columns from a data table
#'
#' @param df a data table from which to remove columns
#' @param undesired_cols a character vector of column names identifying columns to be removed
#' @return a data table
#'
#' @details This function \bold{modifies by reference}.
#'
#' @keywords internal
#' @examples
#' x <- data.table::data.table(a = c(1,2,3), b = c(1,2,3), c = c(1,2,3))
#' orignames <- names(x)
#' undesired_cols <- c("a","b", "d")
#' nsqipr:::remove_undesired(x, undesired_cols)
#' identical(names(x), setdiff(orignames, undesired_cols))
#'
#' @importFrom data.table :=
remove_undesired <- function(df, undesired_cols) {
  for(j in intersect(undesired_cols, names(df))) data.table::set(df, j=j, value = NULL)
  invisible(df)
}

#' Coalesce two vector
#'
#' A simple wrapper around \code{\link[data.table:fcoalesce]{fcoalesce}}.
#'
#' @param new the newer vector to be coalesced into. Will take priority if both
#' columns have values at the same position.
#' @param old the older vector to be coalesced.
#'
#' @return a vector of the type of \code{new}
#' @keywords internal
#'
#' @examples
#' x = c(11L, NA, 13L, NA, 15L, NA)
#' y = c(NA, 12L, 5L, NA, NA, NA)
#' nsqipr:::coalesce(x, y)
#'
coalesce <- function(new, old) {
  data.table::fcoalesce(new, old)
}

#' Coalesce two columns
#'
#' @param df a data table with the desired columns to coalesce
#' @param coalesce_in_cols a character vector of column names representing the columns to be coalesced into and that take precedence
#' @param coalesce_out_cols a character vector of column names representing the columns to be coalesced out of
#'
#' @details This function \bold{modifies by reference}.
#' The \code{coalesce_in_cols} columns will take precedence over the values in the \code{coalesce_out_cols} columns if both
#' columns contain data. The data is coalesced into the \code{coalesce_in_cols} so that the data in the \code{coalesce_out_cols} is
#' not altered. The \code{coalesce_out_cols} columns are not removed.
#'
#' If \code{df} does not contain any \code{coalesce_out_cols} columns, no changes to \code{df} are made. If \code{df}
#' contains \code{coalesce_out_cols} columns but no \code{coalesce_in_cols}, the \code{coalesce_out_cols} columns are simply
#' renamed to their counterpart \code{coalesce_in_cols} columns. For this reason, \code{coalesce_in_cols} and
#' \code{coalesce_out_cols} must be the same length and matching indices between the two vectors represent respective columns
#' to be coalesced into and out of.
#'
#' @return a data table
#' @keywords internal
#'
#' @examples
#' x <- data.table::data.table(a = c(NA, TRUE, FALSE), b = c(NA, TRUE, FALSE),
#' c = c(NA, TRUE, FALSE), d = c(NA, TRUE, FALSE),
#' A = c(TRUE, NA, TRUE), B = c(TRUE, NA, TRUE))
#' coalesce_in_cols <- c("A","B","C","D")
#' coalesce_out_cols <- c("a","b","c","d")
#' nsqipr:::coalesce_cols(x, coalesce_in_cols, coalesce_out_cols)
#' x
#'
coalesce_cols <- function(df, coalesce_in_cols, coalesce_out_cols) {
  stopifnot("coalesce_in_cols and coalesce_out_cols must be the same length" = length(coalesce_in_cols) == length(coalesce_out_cols))
  for(j in intersect(coalesce_out_cols, names(df))) {
    in_name <- coalesce_in_cols[which(coalesce_out_cols == j)]
    if(in_name %in% names(df)) {
      data.table::set(df, j = in_name, value = coalesce(df[[in_name]], df[[j]]))
    } else {
      data.table::setnames(df, j, in_name, skip_absent = TRUE)
    }
  }
  invisible(df)
}
