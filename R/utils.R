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
