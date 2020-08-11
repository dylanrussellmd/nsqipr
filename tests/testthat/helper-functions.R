get_unittest_df <- function(dir, maxlen = 30L) {
  list.files(path = dir, pattern = "*.txt$", full.names = TRUE, recursive = FALSE) %>%
    purrr::map(~get_unique_list(file = ., maxlen = maxlen)) %>%
    purrr::reduce(., cat_lists) %>%
    lapply(., unique) %>%
    lapply(., pad_vec, maxlen = maxlen) %>%
    tibble::as_tibble()
}

get_unique_list <- function(file, maxlen) {
  readr::read_tsv(file, col_types = readr::cols(.default = "c")) %>%
    dplyr::rename_with(., tolower) %>%
    purrr::map(~get_unique_cols(., maxlen))
}

get_unique_cols <- function(column, maxlen) {
  vec <- unname(unlist(head(unique(column), n = maxlen, keepnums = TRUE)))
  length(vec) <- maxlen
  return(vec)
}

cat_lists <- function(list1, list2) {
  keys <- unique(c(names(list1), names(list2)))
  purrr::map2(list1[keys], list2[keys], c) %>%
    purrr::set_names(keys)
}

pad_vec <- function(vec, maxlen) {
  length(vec) <- maxlen
  return(vec)
}
