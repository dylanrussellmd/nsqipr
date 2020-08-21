dev_col_names <- function(path) {
  collect_column_names(list.files(path, full.names = TRUE, recursive = TRUE))
}

dev_print_col_names <- function(cols) {
  cat(cols, sep = '", "')
}

rebuild_data <- function() {
  usethis::use_data(test_df1,test_df2,test_df3,test_df4,aaa_cols,acs_cols,col_cols,col_order,complication_cols,date_cols,integer_cols,numeric_cols,numscale_cols,reason_cols,redundant_cols,test_col_names,yes_no_cols, overwrite = TRUE, internal = TRUE)
}
