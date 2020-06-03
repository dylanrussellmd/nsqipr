con <- DBI::dbConnect(RPostgres::Postgres(),dbname = 'postgres',
                 host = '34.94.46.211', # Public IP of the PostgresSQL database
                 port = 5432,
                 user = rstudioapi::askForPassword("Database user"),
                 password = rstudioapi::askForPassword("Database password"))
