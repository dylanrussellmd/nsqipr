#' nsqipr: A cleaner NSQIP. \if{html}{\figure{hex.png}{options: align="right" alt='nsqipr logo' width=200}} \if{latex}{\figure{hex.png}{options: width=0.5in}}
#'
#' @author \href{https://www.dylanrussellmd.com}{Dylan Russell, M.D}.
#'
#' @description
#' The purpose of `nsqipr` is to streamline the data cleaning process.
#' This package is geared towards those surgical interns, residents, and attendings
#' who have limited experience with R, SQL, or "big data" analysis.
#' It is also designed to be a useful tool for that experienced researcher or
#' computer scientist making frequent use of ACS NSQIP PUFs.
#'
#' @seealso
#' \itemize{
#'  \item{\href{https://github.com/dylanrussellmd/nsqipr}{nsqipr Github}}
#'  \item{\href{https://www.dylanrussellmd/nsqipr_book}{nsqipr Design and Data Dictionary}}
#'  \item{\href{https://www.dylanrussellmd/nsqipr}{nsqipr documentation}}
#'  \item{\href{https://www.facs.org/quality-programs/acs-nsqip}{ACS NSQIP}}
#' }
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
