#' Clean NSQIP data
#'
#' Converts a single NSQIP \code{.txt} or a directory of NSQIP \code{.txt} files into a standardized NSQIP data table.
#'
#' @param path path to file or directory.
#' @param csv write the resulting data frame to a \code{.csv} file. See details for options.
#' @param rds a logical value. Write the resulting data frame to \code{.rds} file. Produces individual files for each \code{.txt} input.
#'
#' @details \code{csv} may be set to "indiv", "append", "both", or NA. If "indiv", a CSV file will be output for each input file.
#' If "append", a single CSV file will be created appending all input files into a single output file. "Both" will enact the
#' effects of both "indiv" and "append". NA will produce no CSV files.
#'
#' @return NULL
#'
#' @export
#'
nsqip <- function(path, csv = "both", rds = TRUE) {

  usethis::ui_info("This could take a while (10-15 minutes depending on how many files)! Don't worry, its working...")
  files <- get_file_or_dir(path) # returns a character vector of matching file(s)
  dirs <- parse_files(files) # Creates directories and moves files into them
  df <- lapply(dirs, nsqip_dir, csv, rds) # Runs nsqip_dir over each directory
  usethis::ui_done("All files in {usethis::ui_path(path)} cleaned and converted!")
  usethis::ui_todo("If you had any problems, please open an issue at {usethis::ui_value('github.com/dylanrussellmd/nsqipr')}!")
}

