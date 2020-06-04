con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = nsqip_db,
                      host = nsqip_db_ip, # Public IP of the PostgresSQL database
                      port = nsqip_db_port,
                      user = nsqip_db_user,
                      password = nsqip_db_pw)

con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = nsqip_db_driver,
                      Server   = nsqip_db_ip, # Public IP of the PostgresSQL database
                      Database = nsqip_db,
                      UID      = nsqip_db_user,
                      PWD      = nsqip_db_pw,
                      Port     = nsqip_db_port) # odbc() allows interface with RStudio connections pane.

addToNsqip <- function(file) {
  df <- readr::read_delim(file,
                    delim = "\t",
                    na = c("","NA","-99","NULL"),
                    )
  DBI::dbCreateTable(con,
                     name = 'puf',
                     fields = df)
  DBI::dbAppendTable(con,
                     name = 'puf',
                     value = df)
}
