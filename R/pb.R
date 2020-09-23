#' Create a progress bar
#'
#' This function creates a progress bar for use with the \code{nsqip} function.
#'
#' @param csv character vector indicating if the user would like to write to a CSV.
#' @param rds logical vector indicating if the user would like to write an RDS.
#'
#' @keywords internal
#'
pb <- function(csv, rds) {
  csv <- if(is.na(csv)) {
    0
  } else {
    (csv == "indiv" | csv == "both") + (csv == "append" | csv == "both")
  }
  totalticks <- 16 + csv + rds
  progress::progress_bar$new(
        format = "(:spin)  :preface :what [:bar] :current/:total", total = totalticks, show_after = 0)
}

#' Increment a progress bar
#'
#' Increments a progress bar by a specified number of ticks. Meant to be used in a pipe.
#'
#' @param pb a progress bar object
#' @param preface the desired preface text to a progress bar
#' @param file the file being processed
#' @param len number of ticks
#'
#' @keywords internal
#'
tick <- function(pb, preface, file, len = 1) {
  pb$tick(len, tokens = list(what = file, preface = preface))
}

