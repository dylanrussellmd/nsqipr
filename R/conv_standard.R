#' Runs the \code{nsqip} function in a single directory.
#'
#' Utilized by \code{nsqip} after all files are parsed and respective directories are created.
#'
#' @param dir directory to which the \code{nsqip} function is being applied.
#' @inheritParams nsqip
#'
#' @keywords internal
#'
nsqip_dir <- function(dir, rds, csv, dataframe) {
  fs::dir_ls(dir) %>%
   lapply(function(file) {
      progbar <- pb(rds, csv, dataframe)
      tick(progbar, "reading", file, 0)
      data.table::fread(file, sep = "\t", colClasses = character(), data.table = FALSE, showProgress = FALSE) %>%
        conv_to_standard(file, rds, csv, dataframe, progbar)
  })
  usethis::ui_done('Successfully cleaned all files in {usethis::ui_path(dir)}.')
}

# TODO: Figure out how to append CSVs while keeping first row as headers.
conv_to_standard <- function(df, file, rds, csv, dataframe, progbar) {

  df %>%
    set_up_df(file, progbar) %>%
    conv_type_cols(file, progbar) %>%
    conv_special_cols(file, progbar) %>%
    conv_order_cols(file, progbar) %>%
    output(file, rds, csv, dataframe, progbar)

  tick(progbar, "completed", file)
  usethis::ui_done('Successfully cleaned {usethis::ui_path(file)}.')

# Need to work in (or at least make it possible to) outputting tidy long data. In addition, should probably split into two tables (one patient, one procedures, one by case id)
#  df1 %>% rename(othercpt0 = cpt, otherproc0 = prncptx, wrvu0 = workrvu) %>% pivot_longer(cols = c(starts_with("othercpt"), starts_with("otherproc"), starts_with("otherwrvu")), names_to = c(".value", "procedure"), names_pattern = "^other([a-z]*)(\\d)$", names_repair = "unique", values_drop_na = TRUE, names_transform = list(procedure = as.integer)) %>% mutate(procedure = procedure + 1) %>% select(caseid,procedure, cpt, proc, wrvu) %>% View()

}

set_up_df <- function(df, file, progbar) {
  tick(progbar, "setting up", file)
  data.table::setnames(df, stringi::stri_trans_tolower(names(x)))
  setlower(df)
  setna(df, c("unknown", "unknown/not reported", "null", "n/a", "not documented", "none/not documented", "not entered","-99"))
}

setna <- function(df, val) {
  for(j in seq_along(df)){
    data.table::set(df, i=which(df[[j]] %in% val), j=j, value=NA)
  }
}

setlower <- function(df) {
  for(j in seq_along(df)){
    data.table::set(df, j=j, value=stringi::stri_trans_tolower(df[[j]]))
  }
}

conv_type_cols <- function(df, file, progbar) {
  tick(progbar, "converting generic columns of", file)
  df %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(date_cols), ~ lubridate::ymd(.x, truncated = 2))) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(complication_cols), conv_complication)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(numscale_cols), conv_numscale)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(yes_no_cols), conv_yesno)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(integer_cols), as.integer)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(numeric_cols), conv_numeric)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(reason_cols), conv_reasons))
}

conv_special_cols <- function(df, file, progbar) {
  tick(progbar, "converting unique columns of", file)
  fn <- switch(parse_filename(file),
               "acs_nsqip_puf" = `conv_acs_cols`,
               "puf_tar_col" = `conv_col_cols`,
               "puf_tar_aaa" = `conv_aaa_cols`,
               "puf_tar_aie" = `conv_aie_cols`,
               "puf_tar_pan" = `conv_pan_cols`)
  fn(df)
}

conv_order_cols <- function(df, file, progbar) {
  tick(progbar, "ordering columns of", file)
  #Think about using set order
  df %>%
    dplyr::select(!dplyr::any_of(redundant_cols)) %>%
    dplyr::select(dplyr::any_of(col_order))
}


