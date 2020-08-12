get_file_sizes <- function(dir) {
  list.files(dir, full.names = TRUE) %>%
    sapply(file.size) %>%
    magrittr::divide_by(1e6) %>%
    round(1)
}
