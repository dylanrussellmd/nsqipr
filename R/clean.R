clean_data <- function(d) {
  nas <- c("", "NA", "-99","NULL")
  d %>%
    dplyr::mutate(dplyr::across(.fns = stringr::str_to_lower)) %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), ~dplyr::na_if(x = dplyr::cur_column(), y = "-99")))
}

