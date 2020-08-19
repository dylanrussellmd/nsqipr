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

parse_files <- function(path) {

  files <- get_file_or_dir(path) %>%
    sapply(parse_filename)

  create_dirs(files, path)

  files %>% move_file(path = path)
}

create_dirs <- function(files, path) {
  files %>%
    unique() %>%
    create_path(path) %>%
    sapply(filesstrings::create_dir)
}

move_file <- function(file, path) {
  filesstrings::move_files(names(file), file.path(check_separator(path), file))
}

create_path <- function(dir_name, path) {
  if (file_test("-d", path)) {
    return(file.path(check_separator(path), dir_name))
  } else {
    return(file.path(dirname(path), dir_name))
  }
}

check_separator <- function(path) {
  stringr::str_remove_all(path, "[:punct:]+$")
}

parse_filename <- function(file) {
  pattern <- stringr::regex("acs_nsqip_puf|puf_tar_[a-z]{1,4}",
                            ignore_case = TRUE)
  stopifnot(stringr::str_detect(file, pattern))
  stringr::str_extract(file, pattern) %>% stringr::str_to_lower()
}


