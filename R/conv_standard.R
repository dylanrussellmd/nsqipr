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
     conv_to_standard(file, rds, csv, dataframe, progbar)
  })
  usethis::ui_done('Successfully cleaned all files in {usethis::ui_path(dir)}.')
}

# TODO: Figure out how to append CSVs while keeping first row as headers.
conv_to_standard <- function(file, rds, csv, dataframe, progbar) {
  progbar <- pb(rds, csv, dataframe)
  filename <- fs::path_file(file)
  tick(progbar, "reading", filename, 0)

  df <- data.table::fread(file, sep = "\t", colClasses = character(), showProgress = FALSE)
  setup(df, filename, progbar)
  conv_type_cols(df, filename, progbar)
  df <- conv_special_cols(df, filename, progbar)
  conv_order_cols(df, filename, progbar)
  output(df, file, rds, csv, dataframe, progbar)

  tick(progbar, "completed", file)
  usethis::ui_done('Successfully cleaned {usethis::ui_path(file)}.')

# Need to work in (or at least make it possible to) outputting tidy long data. In addition, should probably split into two tables (one patient, one procedures, one by case id)
#  df1 %>% rename(othercpt0 = cpt, otherproc0 = prncptx, wrvu0 = workrvu) %>% pivot_longer(cols = c(starts_with("othercpt"), starts_with("otherproc"), starts_with("otherwrvu")), names_to = c(".value", "procedure"), names_pattern = "^other([a-z]*)(\\d)$", names_repair = "unique", values_drop_na = TRUE, names_transform = list(procedure = as.integer)) %>% mutate(procedure = procedure + 1) %>% select(caseid,procedure, cpt, proc, wrvu) %>% View()

}

setup <- function(df, file, progbar) {
  tick(progbar, "converting names to lower case in", file)
  setlowernames(df)
  tick(progbar, "converting values to lower case in", file)
  setlower(df)
  tick(progbar, "setting NA values in", file)
  setna(df, c("unknown", "unknown/not reported", "null", "n/a", "not documented", "none/not documented", "not entered","-99"))
}

setupdev <- function(df) {
  setlowernames(df)
  setlower(df)
  setna(df, c("unknown", "unknown/not reported", "null", "n/a", "not documented", "none/not documented", "not entered","-99"))
}

conv_type_cols <- function(df, file, progbar) {
  tick(progbar, "converting integer columns of", file)
  conv_(df, integer_cols, as.integer)
  tick(progbar, "converting numeric columns of", file)
  conv_(df, numeric_cols, as.numeric)
  tick(progbar, "converting complication columns of", file)
  conv_(df, complication_cols, conv_complication)
  tick(progbar, "converting number scale columns of", file)
  conv_(df, numscale_cols, conv_numscale)
  tick(progbar, "converting yes/no columns of", file)
  conv_(df, yes_no_cols, conv_yesno)
  tick(progbar, "converting reason for readmission columns of", file)
  conv_(df, reason_cols, conv_reasons)
  tick(progbar, "converting date columns of", file)
  conv_(df, date_cols, conv_date)
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
  colorder(df)
  tick(progbar, "removing redundant columns from", file)
  remove_redundant(df)
}


