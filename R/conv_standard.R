#' Converts a single NSQIP \code{.txt} or a directory of NSQIP \code{.txt} files into a standardized NSQIP data frame.
#'
#' @return a data frame of class \code{tibble}.
#'
#' @param path path to file or directory.
#' @param write_to_csv write the resulting data frame to a \code{.csv} file. Produces individual files for each \code{.txt} input
#' @param append writes the resulting data frames all to a single \code{.csv} file called \code{nsqip_clean.csv}
#' @param return_df return the resulting data frame
#'
#' @export
#' @importFrom "utils" "file_test"
#'

# TODO Need to add in the functionality to create all the directories then parse through directories.

nsqip <- function(path, write_to_csv = FALSE, append = FALSE) {

  col_names <- create_na_template(path)

  lapply(get_file_or_dir(path),
         conv_to_standard,
         write_to_csv = write_to_csv, append = append, col_names = col_names)
}



