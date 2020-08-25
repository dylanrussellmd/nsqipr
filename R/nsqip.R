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
#' @importFrom "utils" "file_test"
#'
# TODO If only one type of PUF (i.e. just ACS, just targeted, etc.), then skip making directories.
nsqip <- function(path, return_df = TRUE, write_to_csv = FALSE, append = FALSE, headers = TRUE) {

  files <- get_file_or_dir(path)
  parse_files(files)
  dirs <- list.dirs(unique(dirname(files)), recursive = FALSE) # This allows this function to work whether the user passes a single file or a directory of files.

  lapply(dirs,
         nsqip_dir,
         return_df = return_df, write_to_csv = write_to_csv, append = append, headers = headers)

  usethis::ui_done('Successfullly cleaned all files in {usethis::ui_path(path)}.')
}

#' Runs the \code{nsqip} function in a single directory.
#'
#' Utilized by \code{nsqip} after all files are parsed and respective directories are created.
#'
#' @param dir directory to which the \code{nsqip} function is being applied.
#' @inheritParams nsqip
#'
#' @keywords internal
#'
nsqip_dir <- function(dir, return_df, write_to_csv, append, headers) {
  files <- open_dir(dir)
  col_names <- create_na_template(files)
  lapply(files,
         conv_to_standard,
         return_df = return_df, write_to_csv = write_to_csv, append = append, headers = headers, col_names = col_names)
}




