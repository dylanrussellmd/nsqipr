#' Runs the \code{nsqip} function in a single directory.
#'
#' Utilized by \code{nsqip} after all files are parsed and respective directories are created.
#'
#' @param dir directory to which the \code{nsqip} function is being applied.
#' @inheritParams nsqip
#'
#' @keywords internal
#'
nsqip_dir <- function(dir, csv, rds) {
  files <- fs::dir_ls(dir, type = "file", glob = "*.txt") # Create list of files in directory
  cols <- collect_column_names(files) # Create unique vector of column names in directory
  df <- lapply(files, function(file) {
     conv_to_standard(file, cols, csv, rds) # Clean each file in the directory
  })
  usethis::ui_done('Successfully cleaned all files in {usethis::ui_path(dir)}.')
  invisible(NULL)
}

#' The top level data cleaning function
#'
#' Calls all cleaning functions in the correct order and outputs a clean file.
#'
#' @param file a text file containing data to be cleaned
#' @param cols a character vector of column names
#' @inheritParams nsqip_dir
#'
#' @keywords internal
#'
conv_to_standard <- function(file, cols, csv, rds) {
  progbar <- pb(csv, rds) # Creates a progress bar
  filename <- fs::path_file(file) # Extracts the file name portion of the file path
  tick(progbar, "reading", filename, 0)

  # Call all cleaning functions
  df <- data.table::fread(file, sep = "\t", colClasses = "character", showProgress = FALSE, na.strings = na_strings) # This is where the NA strings are converted.
  setup(df, filename, progbar, cols)
  conv_type_cols(df, filename, progbar)
  conv_special_cols(df, filename, progbar)
  conv_factor_cols(df, filename, progbar)
  longtables <- conv_long_tables(df, filename, progbar)
  conv_order_cols(df, filename, progbar)
  output(df, file, filename, csv, rds, longtables, progbar)

  tick(progbar, "completed", filename)
  usethis::ui_done('Successfully cleaned {usethis::ui_path(filename)}.')

  invisible(NULL)
}

setup <- function(df, filename, progbar, cols) {
  tick(progbar, "converting names to lower case in", filename)
  setlowernames(df)
  tick(progbar, "adding missing columns to", filename)
  addmissingcolumns(df, cols)
  tick(progbar, "coalescing old and new columns", filename)
  coalesce_cols(df, coalesce_in_cols, coalesce_out_cols)
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
               "acs_nsqip_puf" = `conv_puf_cols`,
               "puf_tar_col" = `conv_col_cols`,
               "puf_tar_aaa" = `conv_aaa_cols`,
               "puf_tar_aie" = `conv_aie_cols`,
               "puf_tar_aio" = `conv_aio_cols`,
               "puf_tar_lee" = `conv_lee_cols`,
               "puf_tar_leo" = `conv_leo_cols`,
               "puf_tar_app" = `conv_app_cols`,
               "puf_tar_pan" = `conv_pan_cols`,
               "puf_tar_hep" = `conv_hep_cols`)
  fn(df, filename)
}

conv_factor_cols <- function(df, filename, progbar) {
  tick(progbar, "converting factor columns of", filename)
  conv_factor(df, factor_cols)
}

conv_long_tables <- function(df, filename, progbar) {
  tick(progbar, "creating long tables for", filename)
  long_funcs <- c(make_cpt_long, make_reop_long, make_readm_long, make_anesthes_other_long, make_pan_percdrainage_long, make_amylase_long, make_hep_neotherapy_long, make_hep_con_ablation_long,
                  make_hep_invasive_type_long)
  lapply(long_funcs, function(f) f(df))
}

conv_order_cols <- function(df, filename, progbar) {
  tick(progbar, "ordering columns of", filename)
  colorder(df, col_order)
  tick(progbar, "removing redundant columns from", filename)
  remove_undesired(df, redundant_cols)
}
