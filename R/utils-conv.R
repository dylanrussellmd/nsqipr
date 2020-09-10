#' Convert column names to lower case
#'
#' @param df a data frame to convert names to lower case
#' @return a data frame
#' @keywords internal
#' @examples
#'
#' x <- data.frame(X = NA, Y = NA, Z = NA)
#' setlowernames(x)
#' names(x)
#'
setlowernames <- function(df) {
  data.table::setnames(df, stringi::stri_trans_tolower(names(df)))
}

#' Convert all variables to lower case
#'
#' @param df a data frame to convert to lower case
#' @return a data frame
#' @keywords internal
#' @examples
#'
#' x <- data.frame(x = rep("APPLE", 10), y = rep("BANANA", 10), z = rep("CHERRY", 10))
#' setlower(x)
#' x
#'
setlower <- function(df) {
  for(j in seq_along(df)){
    data.table::set(df, j=j, value=stringi::stri_trans_tolower(df[[j]]))
  }
  invisible(df)
}

#' Convert all strings matching vector to NA
#'
#' @param df a data frame to convert values to NA
#' @param val a character vector of values to set to NA
#' @return a data frame
#' @keywords internal
#' @examples
#'
#' xx <- data.table::data.table(x = rep("unknown", 10), y = rep("unknown/not reported", 10), z = rep("null", 10),
#' xx = rep("not documented", 10), yy = rep("none/not documented", 10), zz = rep("not entered", 10),
#' xxx = rep("-99", 10), yyy = rep("test", 10))
#'
#' setna(x, c("unknown", "unknown/not reported", "null", "n/a", "not documented", "none/not documented", "not entered","-99"))
#' x
#'
setna <- function(df, val) {
  for(j in seq_along(df)){
    data.table::set(df, i=which(df[[j]] %in% val), j=j, value=NA)
  }
  invisible(df)
}

conv_ <- function(df, cols, f) {
  for(j in intersect(cols, names(df))) data.table::set(df, j = j, value = f(df[[j]]))
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
#' lapply(x , conv_yesno)
#'
conv_yesno <- function(vec) {
  stringi::stri_detect_fixed(vec, "yes", opts_fixed = list(case_insensitive = TRUE))
}

#' Convert "no" columns to logicals
#'
#' @param vec a character vector to convert "no" columns
#' @return a logical vector
#'
#' @details Matches the case-insensitive fixed string \code{"no"} and negates the result.
#' Any string that does not match will return TRUE. NA will return NA.
#'
#' @keywords internal
#' @examples
#'
#' x <- data.frame(x = rep("no", 10), y = rep("NO", 10), z = rep("yes", 10),
#' xx = rep("", 10), yy = rep(NA, 10))
#' lapply(x , conv_notno)
#'
conv_notno <- function(vec) {
  !stringi::stri_detect_fixed(vec, "no", opts_fixed = list(case_insensitive = TRUE))
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
#' x <- data.frame(x = rep("no complication", 10), y = rep("NO COMPLICATION", 10), z = rep("complication", 10),
#' xx = rep("", 10), yy = rep(NA, 10))
#' lapply(x , conv_complication)
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
#' lapply(x , conv_numscale)
#'
conv_numscale <- function(vec) {
  as.integer(stringi::stri_extract_first_regex(vec,"^.*?\\d"))
}


#' Converts a single year to a date
#'
#' @param vec a character vector containing dates in the format "yyyy"
#' @return a Date vector
#'
#' @details Formats dates via \code{as.Date(vec, "%Y")}
#'
#' @keywords internal
#' @examples
#'
#' x <- c("2000", "1900", "2020", "1950")
#' conv_date(x)
#'
conv_date <- function(vec) {
  as.Date(vec,"%Y")
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
#' c("apple","cherry","pork") %^% list(fruit = c("apple", "banana", "cherry"), meat = c("steak", "chicken", "pork"))
#'
`%^%` <- function(lhs, rhs) {
  `levels<-`(factor(stringi::stri_replace_all_fixed(lhs,"  ", " ")), rhs)
}

#' Convert factor columns
#'
#' @param df a data frame in which to convert factor columns
#' @return a data frame
#'
#' @details This function checks for which columns to factor by looking for a master character vector called \code{factor_cols}.
#' The function then fetches a second variable with the same name as the column being converted to a factor.
#' This variable should reference a named list specifying how to rename the levels (see \code{\link[nsqipr:factorpipe]{\%^\%}})
#' for further details.
#'
#' @keywords internal
#' @examples
#'
#' x <- data.table::data.table(foods = c("apple","banana","cherry","steak","chicken","pork"),
#'                             drinks = c("milk","water","oj","beer","vodka","rum"))
#' foods <- list(fruit = c("apple", "banana", "cherry"), meat = c("steak","chicken","pork"))
#' drinks <- list(`non-alcoholic` = c("milk","water","oj"), alcoholic = c("beer","vodka","rum"))
#' factor_cols <- c("foods", "drinks")
#' conv_factor(x)
#' x
#'
conv_factor <- function(df) {
  for(j in intersect(factor_cols, names(x))) data.table::set(x, j = j, value = x[[j]] %^% get(j))
  invisible(df)
}

colorder <- function(df) {
  data.table::setcolorder(df, intersect(col_order, names(df)))
}

#' @importFrom data.table :=
remove_redundant <- function(df) {
  df[, intersect(redundant_cols, names(df)) := NULL]
}
