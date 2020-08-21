#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

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

parse_files <- function(files) {

  dirnames <- files %>%
    sapply(parse_filename)

  base <- unique(sapply(files, dirname))
  stopifnot("Are you sure the files are in the specified folder?" = length(base) == 1)

  create_dirs(dirnames, base)

  dirnames %>% move_file(base = base)
}

parse_filename <- function(file) {
  pattern <- stringr::regex("acs_nsqip_puf|puf_tar_[a-z]{1,4}",
                            ignore_case = TRUE)
  stopifnot(stringr::str_detect(file, pattern))
  stringr::str_extract(file, pattern) %>% stringr::str_to_lower()
}

create_dirs <- function(dirnames, base) {
  dirnames %>%
    unique() %>%
    sapply(., create_path, base = base) %>%
    sapply(filesstrings::create_dir)
}

move_file <- function(dirnames, base) {
  paste(dirnames, base)
  filesstrings::move_files(names(dirnames), file.path(base, dirnames))
}

create_path <- function(dir_name, base) {
  if (file_test("-d", base)) {
    return(file.path(check_separator(base), dir_name))
  } else {
    return(file.path(dirname(base), dir_name))
  }
}

check_separator <- function(path) {
  stringr::str_remove_all(path, "[:punct:]+$")
}

open_dir <- function(path) {
  list.files(path, pattern = "*.txt", full.names = TRUE, recursive = FALSE)
}
