dev_col_names <- function(path) {
  collect_column_names(list.files(path, full.names = TRUE, recursive = TRUE))
}

dev_print_col_names <- function(cols) {
  cat(cols, sep = '", "')
}
