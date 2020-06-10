
#' Write data from ACS NSQIP files into database.
#'
#' Accepts a directory containing ACS NSQIP files as downloaded from the ACS NSQIP website and uploads to a database.
#' Will automatically format all strings in sentence case and correctly set the variable types.
#' Table names will be either \code{acs_nsqip_puf} for all ACS NSQIP PUF files or
#' \code{puf_tar_x} where \code{x} is a three- or four-letter abbreviation for the targeted procedure.
#'
#' @param dir A directory containing only \code{.exe} files downloaded directly from ACS NSQIP.
#' @param conn An S4 object of class PostgreSQL
#'
#' @details # Warning
#' \itemize{
#' \item The directory should include \strong{only} those ".exe" executable files downloaded directly from ACS NSQIP.
#' \item Do \strong{not} change the names of the downloaded files.
#' \item Currently only functioning with a PostgreSQL 12 database server.
#' \item Requires \code{7z} and \code{psql} installed on the host OS. Be sure \code{psql} is in the PATH.
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
nsqip <- function(dir, conn) {
  tmpdir <- build_tmp_dir()
  extract_exe_folder(dir, tmpdir)
  write_to_database(conn, tmpdir)
  unlink(tmpdir, recursive = TRUE, force = TRUE)
}

build_tmp_dir <- function() {
  tmpdir <- file.path(tempdir(),'nsqip-data')
  message("Building temporary directory at ", tmpdir, ".")
  if(!dir.exists(tmpdir)) {
    dir.create(tmpdir)
  } else {
    message(tmpdir, " already exists. Using existing folder. This may overwrite data.")
  }
  return(tmpdir)
}

extract_exe_folder <- function(dir, tmpdir) {
  lapply(get_file_or_dir(dir, pattern = "*.exe$"),
         extract_exe_file, tmpdir = tmpdir)
}

get_file_or_dir <- function(path, pattern){
  if (file_test("-d", path)) {
    result <- list.files(path = path, pattern = pattern,
                         full.names = TRUE, recursive = FALSE)
  } else if (file_test("-f", dir)) {
    result <- path
  } else {
    stop(paste(dir,'is an invalid file or directory path.', sep = " "))
  }
  return(result)
}

extract_exe_file <- function(file, tmpdir) {
  system(
    sprintf("7z e %s -o%s", file, tmpdir),
    show.output.on.console = FALSE
  )
}

write_to_database <- function(conn, tmpdir) {
    files <- list.files(path = tmpdir, pattern = "*.txt$",
                        full.names = TRUE, recursive = FALSE)
    lapply(files, write_from_file, conn = conn)
}

write_from_file <- function(conn, file) {
  tablename <- parse_filename(file)
  df <- create_df(file)
  check_table(conn, tablename, df)
  write_to_table(tablename, df)
}

parse_filename <- function(file) {
  pattern <- stringr::regex("acs_nsqip_puf|puf_tar_[a-z]{1,4}",
                            ignore_case = TRUE)
  stopifnot(stringr::str_detect(file, pattern))
  tablename <- stringr::str_extract(file, pattern)
  stringr::str_to_lower(tablename)
}

create_df <- function(file) {
  nas <- c("", "NA", "-99","NULL")
  df <- readr::read_tsv(file, na = nas, guess_max = 30000,
                        trim_ws = TRUE) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::mutate(dplyr::across(where(is.character), stringr::str_to_sentence))
}

check_table <- function(conn, tablename, df) {
  if (!DBI::dbExistsTable(conn, tablename)) {
    DBI::dbCreateTable(conn, tablename, df)
  }
}

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
