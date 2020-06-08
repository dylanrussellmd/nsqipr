#conn <- DBI::dbConnect(odbc::odbc(),
#                      Driver   = Sys.getenv("NSQIP_DB_DRIVER"),
#                      Server   = Sys.getenv("NSQIP_DB_HOST"),
#                     Database = Sys.getenv("NSQIP_DB"),
#                      UID      = Sys.getenv("NSQIP_DB_USER"),
#                      PWD      = Sys.getenv("NSQIP_DB_PW"),
#                      Port     = Sys.getenv("NSQIP_DB_PORT")
#)

import_data_dir <- function(conn, dir) {
  if (file_test("-d", dir)) {
    files <- list.files(path = dir, pattern = "*.txt$",
                        full.names = TRUE, recursive = FALSE)
    lapply(files, write_data, conn = conn)
  } else if (file_test("-f", dir)) {
    write_data(conn, dir)
  } else {
    stop(paste(dir,'is an invalid file or directory path.', sep = " "))
  }
}

write_data <- function(conn, file) {
  tablename <- parse_filename(file)
  df <- create_df(file)
  check_table(conn, tablename, df)
  write_to_table(tablename, df)
}

check_table <- function(conn, tablename, df) {
  if (!DBI::dbExistsTable(conn, tablename)) {
    DBI::dbCreateTable(conn, tablename, df)
  }
}

write_to_table <- function(tablename, df) {
  tmp <- tempfile(fileext = '.csv')
  data.table::fwrite(df, tmp)
  URI <- sprintf("postgresql://%s:%s@%s:%s/%s", Sys.getenv('NSQIP_DB_USER'), Sys.getenv('NSQIP_DB_PW'),
                 Sys.getenv('NSQIP_DB_HOST'), Sys.getenv('NSQIP_DB_PORT'), Sys.getenv('NSQIP_DB'))
  system(
    sprintf("psql -c \"\\copy %s (%s) from %s (FORMAT CSV, HEADER)\" %s",
            tablename, paste(colnames(df), collapse = ","),
            tmp, URI),
  )
  unlink(tmp)
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
