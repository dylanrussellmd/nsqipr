#' \code{nsqipr} package
#'
#' Interface with NSQIP data via R
#'
#' See the README on \href{https://github.com/dylanrussellmd/nsqipr}{Github}
#'
#'
#' @docType package
#' @name nsqipr
#' @importFrom magrittr %>%
#' @importFrom magrittr %T>%
NULL

## quiets concerns of R CMD check for global variables being used.
if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c(".", col_order))
}
