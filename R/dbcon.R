#conn <- DBI::dbConnect(odbc::odbc(),
#                      Driver   = nsqip_db_driver,
#                      Server   = nsqip_db_ip,
#                      Database = nsqip_db,
#                      UID      = nsqip_db_user,
#                      PWD      = nsqip_db_pw,
#                      Port     = nsqip_db_port
#)

#conn <- function(driver, ip, db, port, user, pw) {
#  DBI::dbConnect(odbc::odbc(),
#                 Driver = driver,
#                 Server = ip,
#                 Databse = db,
#                 Port = port,
#                 UID = user,
#                 PWD = pw
#                 )
#}

import_data_dir <- function(conn, dir) {
  if (file_test("-d", dir)) {
    files <- list.files(path = dir, pattern = "*.txt",
                        full.names = TRUE, recursive = FALSE)
    lapply(files, write_data, conn)
  } else if (file_test("-f", dir)) {
    write_data(conn, dir)
  } else {
    stop(paste(dir,'is an invalid file or directory path.', sep = " "))
  }
}

write_data <- function(conn, file) {
  df <- create_df(file)
  tablename <- parse_filename(file)
  if (!DBI::dbExistsTable(conn, tablename)) {
    DBI::dbCreateTable(conn, tablename, df)
  }
  DBI::dbAppendTable(conn, tablename, df)
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
