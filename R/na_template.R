create_na_template <- function(files) {
  collect_column_names(files) %>%
    na_template()
}

collect_column_names <- function(files) {
  files %>%
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
