#' Check if a path is a directory or a file
#'
#' Checks if a provided path is a directory or a file. This allows a user to pass either a directory
#' of files matching a provided \code{pattern} or a single file matching a provided \code{pattern}.
#'
#' @inheritParams base::list.files
#'
#' @return either a list of file paths or a single file path.
#'
#' @keywords internal
#'
get_file_or_dir <- function(path, pattern = "*.txt"){
  if (file_test("-d", path)) {
    result <- list.files(path = path, pattern = pattern,
                         full.names = TRUE, recursive = FALSE)
  } else if (file_test("-f", path)) {
    result <- path
  } else {
    usethis::ui_stop("{usethis::ui_path(path)} is an invalid file or directory path.")
  }
  return(result)
}

#' Parses a directory of files and creates the requisite directories. Moves files into their respective directories.
#'
#' @param files a character vector of file names.
#'
#' @keywords internal
#'
parse_files <- function(files) {

  dirnames <- files %>%
    sapply(parse_filename)

  base <- unique(sapply(files, dirname))
  stopifnot("Are you sure the files are in the specified folder?" = length(base) == 1)

  suppressMessages(create_dirs(dirnames, base)) # Suppress the messages from the filesstrings::create_dir function.

  dirnames %>% move_file(base = base)
}

#' Parses a file name and returns a lower-case string of the extracted regular expression match.
#'
#' @param file a file name
#'
#' @return string
#'
#' @keywords internal
#'
parse_filename <- function(file) {
  pattern <- stringr::regex("acs_nsqip_puf|puf_tar_[a-z]{1,4}",
                            ignore_case = TRUE)
  stopifnot(stringr::str_detect(file, pattern))
  stringr::str_extract(file, pattern) %>% stringr::str_to_lower()
}

#' Creates directories from a vector of desired directory names. Only creates unique directories.
#'
#' @param dirnames a character vector of desired directory names
#' @param base the path of the containing directory
#'
#' @keywords internal
#'
create_dirs <- function(dirnames, base) {
  dirnames %>%
    unique() %>%
    sapply(., create_path, base = base) %>%
    sapply(filesstrings::create_dir)
}

#' Moves files from a parent directory to newly created subdirectories.
#'
#' @inheritParams create_dirs
#'
#' @keywords internal
#'
move_file <- function(dirnames, base) {
  paste(dirnames, base)
  suppressMessages(filesstrings::move_files(names(dirnames), file.path(base, dirnames))) # Suppress messages from the filesstrings::move_files function.
}

#' Creates a path to a directory.
#'
#' @param dir_name desired name of directory
#' @inheritParams create_dirs
#'
#' @keywords internal
#'
create_path <- function(dir_name, base) {
  if (file_test("-d", base)) {
    return(file.path(check_separator(base), dir_name))
  } else {
    return(file.path(dirname(base), dir_name))
  }
}

#' Checks a path name for a final back- or forward slash.
#'
#' @param path a directory path
#'
#' @keywords internal
#'
check_separator <- function(path) {
  stringr::str_remove_all(path, "[:punct:]+$")
}

#' Opens a directory and lists all \code{.txt} files with full names.
#'
#' @inheritParams check_separator
#'
#' @keywords internal
open_dir <- function(path) {
  list.files(path, pattern = "*.txt", full.names = TRUE, recursive = FALSE)
}
