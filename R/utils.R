#' Quick Search
#'
#' Quick case-insensitive search of strings in a character vector
#'
#' @param str a character vector: the values to be matched
#' @param vec a character vector: the values to be matched against
#'
#' @details Utilizes \code{data.table::`%chin%`} to rapidly complete a case-insensitive search
#' through a character vector to return a logical vector of string detections.
#' Will always return TRUE or FALSE for each position of \code{str} regardless of NA missing values
#' in either provided vector. NA in \code{str} will never match an NA value in \code{vec}.
#'
#' @return a logical vector of length \code{length(str)}
#'
#' @importFrom data.table %chin%
#' @export
#'
#' @examples
#' x <- c("apple","banana","cherry",NA)
#' "apple" %qsin% x
#' c("APPLE","BANANA","coconut", NA) %qsin% x
#'
`%qsin%` <- function(str, vec) {
  tolower(str) %chin% na.omit(tolower(vec))
}

#' Check if a data table is full
#'
#' @param x a data.table
#'
#' @details Will return TRUE if the object supplied is a non-empty data.table.
#' Otherwise, returns FALSE.
#'
#' @examples
#' isFullDT(NULL)
#' isFullDT("test")
#' isFullDT(NA)
#' isFullDT(data.table::data.table())
#' isFullDT(data.table::data.table(test = numeric(), test2 = logical(), test3 = character()))
#' isFullDT(data.table::data.table(test = 1, test2 = TRUE, test3 = "character"))
#'
isFullDT<- function(x) all(data.table::is.data.table(x), (nrow(x) > 0))
