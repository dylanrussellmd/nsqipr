#' Clean NSQIP data
#'
#' Converts a single NSQIP \code{.txt} or a directory of NSQIP \code{.txt} files into a standardized NSQIP data frame.
#'
#' @return a data frame of class \code{tibble}.
#'
#' @param path path to file or directory.
#' @param return_df return a data frame.
#' @param write_to_csv write the resulting data frame to a \code{.csv} file. Produces individual files for each \code{.txt} input
#' @param append writes the resulting data frames all to a single \code{.csv} file
#' @param headers whether to include column headers in the generated \code{.csv} file
#'
#' @export
#'
nsqip <- function(path, return_df = TRUE, write_to_csv = FALSE, append = FALSE, headers = TRUE) {

  usethis::ui_info("This could take a while (10-15 minutes depending on how many files)! Don't worry, its working...")
  files <- get_file_or_dir(path) # returns a character vector of matching file(s)
  dirs <- parse_files(files) # Creates directories and moves files into them
  dataframes <- dirs %>% purrr::map(nsqip_dir, return_df, write_to_csv, append, headers) # Runs nsqip_dir over each directory
  usethis::ui_done('All files in {usethis::ui_path(path)} converted to {usethis::ui_field("nsqipr")} dataframes!')
  return(dataframes)
}





