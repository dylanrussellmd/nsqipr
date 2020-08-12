get_bq_def <- function(col) {
  switch(class(col),
         "Date" = "DATE",
         "character" = "STRING",
         "logical" = "BOOLEAN",
         "integer" = "INTEGER",
         "numeric" = "NUMERIC"
  )
}

#' @export
print_bq_schema <- function(df) {
  bq <- unname(purrr::imap_chr(purrr::map_chr(df, get_bq_def), ~paste0(.y,":",.x)))
  stringr::str_c(bq, collapse = ",")
}
