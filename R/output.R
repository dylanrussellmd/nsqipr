output <- function(df, file, filename, csv, rds, longtables, progbar) {
  output_rds(df, file, filename, rds, progbar)
  output_csv(df, file, filename, csv, progbar)
  output_longtables(longtables, file, filename, csv, rds, progbar)
}

output_rds <- function(df, file, filename, rds, progbar) {
  if(rds) {
    tick(progbar, "writing main .rds for", filename)
    saveRDS(df, file = paste(tools::file_path_sans_ext(file), "_clean.rds", sep = ""))
  }
}

output_csv <- function(df, file, filename, csv, progbar) {
  if(!is.na(csv)) {
    if(csv == "indiv" | csv == "both") {
      tick(progbar, "writing main .csv for", filename)
      data.table::fwrite(df, file = paste(tools::file_path_sans_ext(file), "_clean.csv", sep = ""), showProgress = FALSE)
    }
    if(csv == "append" | csv == "both") {
      tick(progbar, "appending to full main .csv:", filename)
      data.table::fwrite(df, file = fs::path(fs::path_dir(file), paste(basename(dirname(file)), "_full_clean.csv", sep = "")), showProgress = FALSE, append = TRUE)
    }
  }
}

output_longtables <- function(longtables, file, filename, csv, rds, progbar) {
  tick(progbar, "writing supplementary tables for", filename)
  lapply(longtables, function(x) {
    if(isFullDT(x)) {
      name <- names(x)[[3]]
      if(!is.na(csv)) {
        if(csv == "indiv" | csv == "both") {
          data.table::fwrite(x, file = paste(tools::file_path_sans_ext(file),"_", name ,".csv", sep = ""), showProgress = FALSE)
        }
        if(csv == "append" | csv == "both") {
          data.table::fwrite(x, file = fs::path(fs::path_dir(file), paste(basename(dirname(file)),"_full_", name ,".csv", sep = "")), showProgress = FALSE, append = TRUE)
        }
      }
      if(rds) {
        saveRDS(x, file = paste(tools::file_path_sans_ext(file),"_", name ,".rds", sep = ""))
      }
    }
  })
}
