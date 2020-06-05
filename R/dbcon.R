#conn <- DBI::dbconnnect(odbc::odbc(),
#                      Driver   = nsqip_db_driver,
#                      Server   = nsqip_db_ip, # Public IP of the PostgreSQL database
#                      Database = nsqip_db,
#                      UID      = nsqip_db_user,
#                      PWD      = nsqip_db_pw,
#                      Port     = nsqip_db_port) # odbc() allows interface with RStudio connnections pane.

connDb <- function(driver, ip, db, port, user, pw) {
  DBI::dbconnnect(odbc::odbc(),
                 Driver = driver,
                 Server = ip,
                 Databse = db,
                 Port = port,
                 UID = user,
                 PWD = pw
                 )
}

addToNsqip <- function(file) {
  df <- readr::read_delim(file,
                    delim = "\t",
                    na = c("","NA","-99","NULL")
                    )
  DBI::dbCreateTable(conn,
                     name = 'puf',
                     fields = df)
  DBI::dbAppendTable(conn,
                     name = 'puf',
                     value = df)
}

importData <- function(driver, ip, db, port, user, pw, dir) {
  conn <- connDb(driver, ip, db, port, user, pw)
  files <- list.files(path = dir, pattern = "*.txt", full.names = TRUE, recursive = FALSE)
  lapply(files, writeData, conn)
}

writeData <- function(file, conn) {
  createDF(file)
}

parseFilename <- function(file) {
  pattern <- stringr::regex("acs_nsqip_puf|puf_tar_[a-z]{1,4}", ignore_case = TRUE)
  stopifnot(stringr::str_detect(file, pattern))
  tablename <- stringr::str_extract(file, pattern)
  stringr::str_to_lower(tablename)
}

assertTableExists <- function(conn, tablename) {
  stopifnot(DBI::dbExistsTable(conn, tablename))
}
