#' Converts a single NSQIP \code{.txt} or a directory of NSQIP \code{.txt} files into a standardized NSQIP data frame.
#'
#' @return a data frame of class \code{tibble}.
#'
#' @param path path to file or directory.
#' @param write_to_csv write the resulting data frame to a \code{.csv} file. Produces individual files for each \code{.txt} input
#' @param append writes the resulting data frames all to a single \code{.csv} file called \code{nsqip_clean.csv}
#'
#' @export
#' @importFrom "utils" "file_test"
#'

# TODO Need to add in the functionality to create all the directories then parse through directories.

nsqip <- function(path, return_df = TRUE, write_to_csv = FALSE, append = FALSE) {

  files <- get_file_or_dir(path)
  parse_files(files)
  dirs <- unique(sapply(path, list.dirs, recursive = FALSE))
  lapply(dirs,
         nsqip_dir,
         return_df = return_df, write_to_csv = write_to_csv, append = append)
}

nsqip_dir <- function(dir, return_df, write_to_csv, append) {
  files <- open_dir(dir)
  col_names <- create_na_template(files)
  lapply(files,
         conv_to_standard,
         return_df = return_df, write_to_csv = write_to_csv, append = append, col_names = col_names)
}




