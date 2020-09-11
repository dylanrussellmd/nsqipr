#' Clean NSQIP data
#'
#' Converts a single NSQIP \code{.txt} or a directory of NSQIP \code{.txt} files into a standardized NSQIP data table.
#'
#' @param path path to file or directory.
#' @param rds write the resulting data frame to \code{.rds} file. Produces individual files for each \code{.txt} input
#' @param csv write the resulting data frame to a \code{.csv} file. Produces individual files for each \code{.txt} input
#' @param datatable return a data table?
#'
#' @return if \code{datatable} is TRUE, will return a data.table
#'
#' @export
#'
nsqip <- function(path, rds = TRUE, csv = TRUE, datatable = FALSE) {

  usethis::ui_info("This could take a while (10-15 minutes depending on how many files)! Don't worry, its working...")
  files <- get_file_or_dir(path) # returns a character vector of matching file(s)
  dirs <- parse_files(files) # Creates directories and moves files into them
  dirs %>% lapply(nsqip_dir, rds, csv, datatable) # Runs nsqip_dir over each directory
  usethis::ui_done('All files in {usethis::ui_path(path)} converted to {usethis::ui_field("nsqipr")} .Rdata files!')
}

