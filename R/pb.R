#' Create a progress bar
#'
#' This function creates a progress bar for use with the \code{nsqip} function.
#'
#' @param write_to_csv boolean value indicating if the user would like to write to a CSV.
#'
#' @keywords internal
#'
pb <- function(write_to_csv) {
  progress::progress_bar$new(
    format = "(:spin)  :preface :what [:bar] :current/:total", total = 5 + write_to_csv, show_after = 0)
}

#' Increment a progress bar
#'
#' Increments a progress bar by a specified number of ticks. Meant to be used in a pipe.
#'
#' @param df accepts a dummy variable that allows it to be used in a pipe.
#' @param pb a progress bar object
#' @param preface the desired preface text to a progress bar
#' @param file the file being processed
#' @param len number of ticks
#'
#' @keywords internal
#'
tick <- function(df = NULL, pb, preface, file, len = 1) {
  pb$tick(len, tokens = list(what = file, preface = preface))
}

