
#' Write data from ACS NSQIP files into database
#'
#' Accepts a directory containing ACS NSQIP files as downloaded from the ACS NSQIP website and imports to a database.
#' Will automatically format all strings in sentence case and correctly set the variable types.
#' Table names will be either \code{acs_nsqip_puf} for all ACS NSQIP PUF files or
#' \code{puf_tar_x} where \code{x} is a three- or four-letter abbreviation for the targeted procedure.
#'
#' @param dir A directory containing only \code{.exe} files downloaded directly from ACS NSQIP
#' @param conn An S4 object of class PostgreSQL
#'
#' @details
#' # Environmental variables
#' The \code{.Renviron} file should contain the following variables:
#' \itemize{
#' \item NSQIP_DB - the name of the database
#' \item NSQIP_DB_USER - the database user name
#' \item NSQIP_DB_PW - the database password
#' \item NSQIP_DB_DRIVER - the database driver (see \code{\link[odbc]{odbc}})
#' \item NSQIP_DB_PORT - the database port
#' \item NSQIP_DB_HOST - the host IP address
#' }
#'
#' If using \href{https://www.github.com}{GitHub}, be sure \strong{\code{.Renviron}
#' is listed in your \code{.gitignore}}!
#'
#' # Warning
#' \itemize{
#' \item The directory should include \strong{only} those ".exe" executable files downloaded directly from ACS NSQIP.
#' \item Do \strong{not} change the names of the downloaded files.
#' \item Currently only functioning with a PostgreSQL 12 database server.
#' \item Requires \code{7z} and \code{psql} installed on the host OS. Be sure \code{psql} is in the PATH.
#' \item \code{dir} will currently will not accept a directory with spaces in the file path.
#' }
#'
#' @examples
#' \dontrun{
#' nsqip(dir, conn)
#' DBI::dbGetQuery(conn, "select count(*) as n from acs_nsqip_puf")
#' }
#'
#' @source <https://www.facs.org/quality-programs/acs-nsqip>
#'
#' @export
nsqip <- function(dir, conn) {
  tmpdir <- build_tmp_dir()
  extract_exe_folder(dir, tmpdir)
  write_to_database(conn, tmpdir)
  unlink(tmpdir, recursive = TRUE, force = TRUE)
  usethis::ui_done("Temporary directory at {usethis::ui_path(tmpdir)} removed.")
  usethis::ui_todo("All data succesfully imported to database {usethis::ui_value(Sys.getenv('NSQIP_DB'))}!")
}

#' Create a temporary directory
#'
#' References a temporary directory where .exe files can be unzipped and .csv files can be created.
#' Creates the directory if it does not already exist.
#'
#' @return a path to a temporary directory
#'
#' @keywords internal
#'
build_tmp_dir <- function() {
  tmpdir <- file.path(tempdir(), stringi::stri_rand_strings(1,10))
  if(!dir.exists(tmpdir)) {
    dir.create(tmpdir)
  } else {
    usethis::ui_warn("{usethis::ui_path(tmpdir)} already exists. Using existing folder. This may overwrite data.")
  }
  usethis::ui_done("Temporary directory at {usethis::ui_path(tmpdir)} created.")
  return(tmpdir)
}

#' Extract \code{.exe} files from a directory
#'
#' Accepts a directory and applys the function \code{\link{extract_exe_file}} to each \code{.exe} file.
#'
#' @details The \code{tmpdir} parameter may be produced by the function \code{\link{build_tmp_dir}}.
#'
#' @param tmpdir a temporary directory
#' @inheritParams nsqip
#'
#' @keywords internal
#'
extract_exe_folder <- function(dir, tmpdir) {
  usethis::ui_line("Unarchiving all files at {usethis::ui_path(dir)}")
  lapply(get_file_or_dir(dir, pattern = "*.exe$"),
         extract_exe_file, tmpdir = tmpdir)
}

#' Check if a path is a directory or a file
#'
#' Checks if a provided path is a directory or a file. This allows a user to pass either a directory
#' of files matching a provided \code{pattern} or a single file matching a provided \code{pattern}.
#'
#' @details \code{\link{get_file_or_dir}} is particularly necessary for \code{\link{extract_exe_folder}} because
#' \code{\link[base]{list.files}} will not accept a single file path and thus \code{\link{get_file_or_dir}}
#' is necssary to pass either a list of file paths or a single file path.
#'
#' @inheritParams base::list.files
#'
#' @return either a list of file paths or a single file path.
#'
#' @keywords internal
#'
get_file_or_dir <- function(path, pattern){
  if (file_test("-d", path)) {
    result <- list.files(path = path, pattern = pattern,
                         full.names = TRUE, recursive = FALSE)
  } else if (file_test("-f", dir)) {
    result <- path
  } else {
    usethis::ui_stop("{usethis::ui_path(dir)} is an invalid file or directory path.")
  }
  return(result)
}

#' Extract a file uzing \code{7z}
#'
#' Unzips a file using the \code{7z} (\strong{7-zip}) command line interface.
#'
#' @param file a file to be unzipped
#' @inheritParams extract_exe_folder
#'
#' @details \code{7z} is a file archiver with a high compression ratio format.
#' The command \code{e} "extracts files from archive (without using directory names).
#' The switch \code{-o} will "set output directory".
#' See \code{man 7z} for further details.
#'
#' @source <https://www.7-zip.org>
#'
#' @keywords internal
#'
extract_exe_file <- function(file, tmpdir) {
  system(
    sprintf("7z e %s -o%s -y", file, tmpdir),
    ignore.stdout = TRUE
  )
  usethis::ui_done("{usethis::ui_path(basename(file))} successfully unarchived.")
}

#' Write a delimited text file to a database
#'
#' Accepts a directory containing \code{.txt} files to be written to a database
#' and applies the function \code{\link{write_from_file}} to each file.
#'
#' @inheritParams nsqip
#' @inheritParams extract_exe_folder
#'
#' @keywords internal
#'
write_to_database <- function(conn, tmpdir) {
  usethis::ui_line("Writing to database {usethis::ui_value(Sys.getenv('NSQIP_DB'))}.")
  files <- list.files(path = tmpdir, pattern = "*.txt$",
                      full.names = TRUE, recursive = FALSE)
  lapply(files, write_from_file, conn = conn)
}

#' Enact all necessary steps to upload delimited text file to a database
#'
#' Determines to which table to write (\code{\link{parse_filename}} and \code{\link{check_table}}),
#' creates a data frame from \code{file} (\code{\link{create_df}}), and writes data to the specified
#' database table (\code{\link{write_to_table}}.
#'
#' @inheritParams nsqip
#' @param file a delimited text file to be written to a database
#'
#' @keywords internal
#'
write_from_file <- function(file, conn) {
  tablename <- parse_filename(file)
  df <- create_df(file)
  check_table(conn, tablename, df)
  write_to_table(tablename, df)
  usethis::ui_done("{usethis::ui_path(basename(file))} copied to table {usethis::ui_value(tablename)}.")
}

#' @describeIn write_from_file parses a file name with the regular expression
#' "\code{acs_nsqip_puf|puf_tar_[a-z]{1,4}}". Returns a lower case table name -
#' either \code{acs_nsqip_puf} for all ACS NSQIP PUF files or \code{puf_tar_x}
#' where \code{x} is a three- or four-letter abbreviation for the targeted procedure.
#'
#' @inheritParams write_from_file
#'
#' @keywords internal
#'
parse_filename <- function(file) {
  pattern <- stringr::regex("acs_nsqip_puf|puf_tar_[a-z]{1,4}",
                            ignore_case = TRUE)
  stopifnot(stringr::str_detect(file, pattern))
  tablename <- stringr::str_extract(file, pattern) %>% stringr::str_to_lower()
  }

#' @describeIn write_from_file creates a dataframe from the delimited text file passed by \code{file}.
#' Will eventually be separated into a separate \code{.R} file with multiple functions to provide
#' more sophisticated formatting.
#'
#' @inheritParams write_from_file
#'
#' @keywords internal
#'
create_df <- function(file) {
  nas <- c("", "NA", "-99","NULL")
  df <- readr::read_tsv(file, na = nas, guess_max = 30000,
                        trim_ws = TRUE, col_types = readr::cols()) %>% # cols() just to suppress messages.
    dplyr::rename_all(tolower) %>%
    dplyr::mutate(dplyr::across(where(is.character), stringr::str_to_sentence))
}

#' @describeIn write_from_file checks if the database contains a table equal to \code{tablename}.
#' If it does not, it creates the table using the variable names specified in the provided dataframe.
#'
#' @param df a dataframe
#' @param tablename the name of the table
#' @inheritParams nsqip
#'
#' @keywords internal
#'
check_table <- function(conn, tablename, df) {
  if (!DBI::dbExistsTable(conn, tablename)) {
    DBI::dbCreateTable(conn, tablename, df)
    usethis::ui_info("{usethis::ui_value(tablename)} does not exist as a table.")
    usethis::ui_done("Created table {usethis::ui_value(tablename)}.")
  }
}

#' @describeIn write_from_file creates a temporary \code{.csv} file from a dataframe and writes the data
#' to a PostgreSQL 12 database via a system command.
#'
#' @details
#' The temporary \code{.csv} that is created is immediately destroyed after completion of the function in
#' order to excessive storage requirements.
#'
#' A \strong{URI} is created with the following format in order to access the database via a system command:
#' \code{postgresql://NSQIP_DB_USER:NSQIP_DB_PW@NSQIP_DB_HOST:NSQIP_DB_PORT/NSQIP_DB}. This requires these
#' variables to be declared in the local \code{.Renviron} file.
#'
#' The system command to write to the database requires that \code{psql} is installed on the host system
#' and available in the PATH.
#'
#' @inheritParams check_table
#'
#' @keywords internal
#'
write_to_table <- function(tablename, df) {
  tmp <- tempfile(fileext = ".csv")
  data.table::fwrite(df, tmp)
  URI <- sprintf("postgresql://%s:%s@%s:%s/%s", Sys.getenv('NSQIP_DB_USER'), Sys.getenv('NSQIP_DB_PW'),
                 Sys.getenv('NSQIP_DB_HOST'), Sys.getenv('NSQIP_DB_PORT'), Sys.getenv('NSQIP_DB'))
  system(
    sprintf("psql -c \"\\copy %s (%s) from %s (FORMAT CSV, HEADER)\" %s",
            tablename, paste(colnames(df), collapse = ","),
            tmp, URI),
    show.output.on.console = FALSE
  )
  unlink(tmp)
}
