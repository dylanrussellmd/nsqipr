conv_to_standard <- function(file, return_df, write_to_csv, append, col_names) {

  df <- readr::read_tsv(file, col_types = readr::cols(.default = "c")) %>%
    set_up_df(col_names) %>%
    conv_type_cols() %>%
    conv_special_cols(file) %>%
    dplyr::select(!dplyr::any_of(redundant_cols)) %>%
    dplyr::select(dplyr::any_of(col_order))

  if(write_to_csv & !append) readr::write_csv(df, path = paste(tools::file_path_sans_ext(file), "_clean.csv", sep = ""), na = "", col_names = FALSE)
  if(write_to_csv & append) readr::write_csv(df, path = file.path(dirname(file),paste(parse_filename(file),"full_clean.csv", sep = "_")), na = "", col_names = FALSE, append = TRUE)
  if(return_df) df
}

set_up_df <- function(df, col_names) {
  df %>%
    dplyr::rename_with(., tolower) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), tolower)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), dplyr::na_if, "unknown")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), dplyr::na_if, "null")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), dplyr::na_if, "n/a")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), dplyr::na_if, "not documented")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), dplyr::na_if, "none/not documented")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), dplyr::na_if, "-99")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), dplyr::na_if, -99)) %>%
    tibble::add_column(., !!!col_names[setdiff(names(col_names), names(.))])
}

conv_type_cols <- function(df) {
  df %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(date_cols), ~ lubridate::ymd(.x, truncated = 2))) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(complication_cols), conv_complication)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(numscale_cols), conv_numscale)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(yes_no_cols), conv_yesno)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(integer_cols), as.integer)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(numeric_cols), conv_numeric)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(reason_cols), conv_reasons))
}

conv_special_cols <- function(df, file) {
  fn <- switch(parse_filename(file),
               "acs_nsqip_puf" = `conv_acs_cols`,
               "puf_tar_col" = `conv_col_cols`,
               "puf_tar_aaa" = `conv_aaa_cols`,
               "puf_tar_aie" = `conv_aie_cols`)
  fn(df)
}


