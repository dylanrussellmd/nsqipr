pb <- function(write_to_csv) {
  progress::progress_bar$new(
    format = "(:spin)  :preface :what [:bar] :percent", total = 6 + write_to_csv, clear = FALSE, show_after = 0)
}

tick <- function(df = NULL, pb, preface, file, len = 1) {
  pb$tick(len, tokens = list(what = basename(file), preface = preface))
}

