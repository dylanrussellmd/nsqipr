#' Runs the \code{nsqip} function in a single directory.
#'
#' Utilized by \code{nsqip} after all files are parsed and respective directories are created.
#'
#' @param dir directory to which the \code{nsqip} function is being applied.
#' @inheritParams nsqip
#'
#' @keywords internal
#'
nsqip_dir <- function(dir, csv, rds, datatable) {
  df <- lapply(fs::dir_ls(dir), function(file) {
     conv_to_standard(file, csv, rds, datatable, progbar)
  })
  usethis::ui_done('Successfully cleaned all files in {usethis::ui_path(dir)}.')
  return(df)
}

# TODO: Figure out how to append CSVs while keeping first row as headers.
conv_to_standard <- function(file, csv, rds, datatable, progbar) {
  progbar <- pb(csv, rds, datatable)
  filename <- fs::path_file(file)
  tick(progbar, "reading", filename, 0)

  df <- data.table::fread(file, sep = "\t", colClasses = "character", showProgress = FALSE)
  setup(df, filename, progbar)
  conv_type_cols(df, filename, progbar)
  conv_special_cols(df, filename, progbar)
  conv_factor_cols(df, filename, progbar)
  conv_order_cols(df, filename, progbar)
  output(df, file, csv, rds, datatable, progbar)

  tick(progbar, "completed", filename)
  usethis::ui_done('Successfully cleaned {usethis::ui_path(filename)}.')

  return(df)
}

setup <- function(df, filename, progbar) {
  tick(progbar, "converting names to lower case in", filename)
  setlowernames(df)
  tick(progbar, "setting NA values in", filename)
  setna(df, c("unknown", "unknown/not reported", "null", "n/a", "not documented", "none/not documented", "not entered","-99"))
}

conv_type_cols <- function(df, filename, progbar) {
  tick(progbar, "converting integer columns of", filename)
  conv_(df, integer_cols, as.integer)
  tick(progbar, "converting numeric columns of", filename)
  conv_(df, numeric_cols, as.numeric)
  tick(progbar, "converting complication columns of", filename)
  conv_(df, complication_cols, conv_complication)
  tick(progbar, "converting number scale columns of", filename)
  conv_(df, numscale_cols, conv_numscale)
  tick(progbar, "converting yes/no columns of", filename)
  conv_(df, yes_no_cols, conv_yesno)
  tick(progbar, "converting date columns of", filename)
  conv_(df, date_cols, conv_date)
}


conv_special_cols <- function(df, filename, progbar) {
  tick(progbar, "converting unique columns of", filename)
  fn <- switch(parse_filename(filename),
               "acs_nsqip_puf" = `conv_acs_cols`,
               "puf_tar_col" = `conv_col_cols`,
               "puf_tar_aaa" = `conv_aaa_cols`,
               "puf_tar_aie" = `conv_aie_cols`,
               "puf_tar_pan" = `conv_pan_cols`)
  fn(df, filename)
}

conv_factor_cols <- function(df, filename, progbar) {
  tick(progbar, "converting factor columns of", filename)
  conv_factor(df, factor_cols)
}

conv_order_cols <- function(df, filename, progbar) {
  tick(progbar, "coalescing old and new columns", filename)
  coalesce_cols(df, coalesce_in_cols, coalesce_out_cols)
  tick(progbar, "ordering columns of", filename)
  colorder(df, col_order)
  tick(progbar, "removing redundant columns from", filename)
  remove_undesired(df, redundant_cols)
}


