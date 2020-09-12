#' Clean NSQIP data
#'
#' Converts a single NSQIP \code{.txt} or a directory of NSQIP \code{.txt} files into a standardized NSQIP data table.
#'
#' @param path path to file or directory.
#' @param csv write the resulting data frame to a \code{.csv} file. Produces individual files for each \code{.txt} input
#' @param rds write the resulting data frame to \code{.rds} file. Produces individual files for each \code{.txt} input
#' @param datatable return a data table?
#'
#' @return if \code{datatable} is TRUE, will return a data.table
#'
#' @export
#'
nsqip <- function(path, csv = TRUE, rds = TRUE, datatable = FALSE) {

  usethis::ui_info("This could take a while (10-15 minutes depending on how many files)! Don't worry, its working...")
  files <- get_file_or_dir(path) # returns a character vector of matching file(s)
  dirs <- parse_files(files) # Creates directories and moves files into them
  lapply(dirs, nsqip_dir, csv, rds, datatable) # Runs nsqip_dir over each directory
  usethis::ui_done("All files in {usethis::ui_path(path)} cleaned and converted!")
  usethis::ui_todo("If you had any problems, please open an issue at {usethis::ui_value('github.com/dylanrussellmd/nsqipr')}!")
}

