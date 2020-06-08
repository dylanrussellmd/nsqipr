df <- create_df('nsqip-data/acs_nsqip_puf12.txt')
DBI::dbCreateTable(conn, 'acs_nsqip_puf',df)
data.table::fwrite(df, 'df.csv')

system.time({
  URI <- sprintf("postgresql://%s:%s@%s:%s/%s", nsqip_db_user, nsqip_db_pw, nsqip_db_ip, nsqip_db_port, "nsqip")
  shell(
    sprintf("psql -c \"\\copy %s (%s) from %s (FORMAT CSV, HEADER)\" %s",
            "acs_nsqip_puf", paste(colnames(df), collapse = ","),
            "df.csv", URI),
  )
})

DBI::dbGetQuery(conn, "select count(*) as n from acs_nsqip_puf")
