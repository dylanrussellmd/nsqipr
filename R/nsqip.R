#' Clean NSQIP data
#'
#' Converts a single NSQIP \code{.txt} or a directory of NSQIP \code{.txt} files into a standardized \code{data.table}.
#'
#' @param path a relative or absolute path to a file or directory.
#' @param csv a character vector. write the resulting data table to a \code{.csv} file. See details for options.
#' @param rds a logical vector. Write the resulting data table to \code{.rds} file. Produces individual files for each \code{.txt} input.
#'
#' @details
#'
#' \code{csv} may be set to "\bold{indiv}", "\bold{append}", "\bold{both}", or \bold{NA}. If "\bold{indiv}",
#' a CSV file will be output for each input file. If "\bold{append}", a single CSV file will be created appending all input files into a single output file.
#' "\bold{both}" will enact the effects of both "\bold{indiv}" and "\bold{append}". \bold{NA} will produce no CSV files.
#'
#' @return NULL
#'
#' @export
#'
nsqip <- function(path, csv = "both", rds = TRUE) {
  usethis::ui_info("Relax! Don't worry, its working...")
  files <- get_files_or_dirs(path) # returns a character vector of matching file(s)
  dirs <- parse_files(files) # Creates directories and moves files into them
  df <- lapply(dirs, nsqip_dir, csv, rds) # Runs nsqip_dir over each directory
  usethis::ui_done("All files cleaned and converted!")
  usethis::ui_todo("If you had any problems, please open an issue at {usethis::ui_value('github.com/dylanrussellmd/nsqipr')}!")
}

