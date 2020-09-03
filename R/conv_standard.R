# TODO: Figure out how to append CSVs while keeping first row as headers.
conv_to_standard <- function(file, return_df, write_to_csv, append, headers, col_names) {

  progbar <- pb(write_to_csv)
  tick(NULL, progbar, "reading", file, 0)

  df <- readr::read_tsv(file, col_types = readr::cols(.default = "c"), progress = FALSE) %T>% tick(progbar, "setting up", file) %>%
    set_up_df(col_names) %T>% tick(progbar, "converting generic columns of", file) %>%
    conv_type_cols() %T>% tick(progbar, "converting unique columns of", file) %>%
    conv_special_cols(file) %T>% tick(progbar, "removing redundant columns of", file) %>%
    dplyr::select(!dplyr::any_of(redundant_cols)) %T>% tick(progbar, "ordering columns of", file) %>%
    dplyr::select(dplyr::any_of(col_order)) %T>% {if(write_to_csv) tick(NULL, progbar, "writing CSV for", file) else .}

  if(write_to_csv & !append) readr::write_csv(df, path = paste(tools::file_path_sans_ext(file), "_clean.csv", sep = ""), na = "", col_names = headers)
  if(write_to_csv & append) readr::write_csv(df, path = file.path(dirname(file),paste(parse_filename(file),"full_clean.csv", sep = "_")), na = "", col_names = FALSE, append = TRUE)
  tick(NULL, progbar, "completed", file)
  if(return_df) return(df) else return(NULL)

# Need to work in (or at least make it possible to) outputting tidy long data. In addition, should probably split into two tables (one patient, one procedures, one by case id)
#  df1 %>% rename(othercpt0 = cpt, otherproc0 = prncptx, wrvu0 = workrvu) %>% pivot_longer(cols = c(starts_with("othercpt"), starts_with("otherproc"), starts_with("otherwrvu")), names_to = c(".value", "procedure"), names_pattern = "^other([a-z]*)(\\d)$", names_repair = "unique", values_drop_na = TRUE, names_transform = list(procedure = as.integer)) %>% mutate(procedure = procedure + 1) %>% select(caseid,procedure, cpt, proc, wrvu) %>% View()

}

set_up_df <- function(df, col_names) {
  df %>%
    dplyr::rename_with(., tolower) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), tolower)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), furniture::washer, "unknown", "null", "n/a", "not documented", "none/not documented", "not entered","-99", -99)) %>%
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


