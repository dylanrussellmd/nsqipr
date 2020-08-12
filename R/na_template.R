# These functions are used strictly in development to create the 'col_names' variable that is stored in sysdata.rda.
# 'col_names' is necessary for the standard conversion to occur

create_na_template <- function(dir) {
  collect_column_names(dir) %>%
    na_template()
}

collect_column_names <- function(dir) {
  list.files(path = dir, pattern = "*.txt$", full.names = TRUE, recursive = FALSE) %>%
    lapply(., get_headers) %>%
    unlist() %>%
    unique()
}

get_headers <- function(file) {
  file %>% readr::read_lines(n_max = 1) %>%
    stringr::str_split(pattern = stringr::boundary("word")) %>%
    unlist() %>%
    stringr::str_to_lower()
}

na_template <- function(vec) {
  vec %>%
    magrittr::set_names(., .) %>%
    replace(., c(1:length(.)), NA)
}
