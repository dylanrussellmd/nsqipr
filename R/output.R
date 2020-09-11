output <- function(df, file, rds, csv, datatable, progbar) {
  output_rds(df, file, rds, progbar)
  output_csv(df, file, csv, progbar)
  output_datatable(df, file, datatable, progbar)
}

output_rds <- function(df, file, rds, progbar) {
  if(rds) {
    tick(progbar, "writing .rds for", file)
    saveRDS(df, file = paste(tools::file_path_sans_ext(file), "_clean.rds", sep = ""))
  }
}

output_csv <- function(df, file, csv, progbar) {
  if(csv) {
    tick(progbar, "writing .csv for", file)
    data.table::fwrite(df, file = paste(tools::file_path_sans_ext(file), "_clean.csv", sep = ""), showProgress = FALSE)
  }
}

output_datatable <- function(df, file, datatable, progbar) {

}

#if(write_to_csv & !append) vroom::vroom_write(df, path = paste(tools::file_path_sans_ext(file), "_clean.csv", sep = ""), delim = ",", na = "", col_names = headers)
#if(write_to_csv & append) vroom::vroom_write(df, path = file.path(dirname(file),paste(parse_filename(file),"full_clean.csv", sep = "_")), delim = ",", na = "", col_names = FALSE, append = TRUE)
