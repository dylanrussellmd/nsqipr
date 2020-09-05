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
get_file_or_dir <- function(path, pattern = "^(acs_nsqip_puf|puf_tar_[a-z]{1,4})(?:\\d{2})?(?:.*)?\\.txt$"){
  pattern <- stringr::regex(pattern, ignore_case = TRUE)
  if (fs::is_dir(path)) {
    result <- fs::dir_ls(path = path, pattern = pattern,
                         type = "file", recurse = FALSE)
  } else if (fs::is_file(path)) {
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

  new_dirnames <- purrr::map_chr(files, parse_filename) # iterates over a list of file paths and parses out the base of the name i.e. acs_nsqip_puf, puf_tar_col, etc.
  stopifnot("Are you sure the files are in the specified folder?" = length(new_dirnames) > 0)
  base <- unique(purrr::map_chr(files, dirname)) # retrieves the original path by finding the unique base name
  stopifnot("Something strange is happening. Move all the files you want cleaned to their own folder with nothing else in it and try again." = length(base) == 1)
  dirs <- create_dirs(new_dirnames, base)
  files %>% move_files() # Move files into the correct directory
  return(dirs)
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
  pattern <- stringr::regex("acs_nsqip_puf|puf_tar_[a-z]{1,4}", ignore_case = TRUE)
  stopifnot(stringr::str_detect(file, pattern)) # Double check that the file matches the pattern provided.
  stringr::str_extract(file, pattern) %>% stringr::str_to_lower() # Extract the file and convert it to lower case.
}

#' Creates directories from a vector of desired directory names. Only creates unique directories.
#'
#' @param dirnames a character vector of desired directory names
#' @param base the path of the containing directory
#'
#' @keywords internal
#'
create_dirs <- function(dirnames, base) {
  dirs <- dirnames %>%
    unique() %>%
    purrr::map_chr(~fs::path(base, .)) %T>% # Create list of paths for new directories
    fs::dir_create()  # Create each of these directories
}

#' Moves files from a parent directory to newly created subdirectories.
#'
#' @param files character vector of files to move
#'
#' @keywords internal
#'
move_files <- function(files) {
  files %>% purrr::walk(function(x) {
    to <- fs::path(fs::path_dir(x), parse_filename(x))
    fs::file_move(x, to) # Move files into the correct directories
  })
}
