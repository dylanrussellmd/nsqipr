pb <- function() {
  progress_bar$new(
    format = "(:spin)  :preface :what [:bar] :percent", total = 120, clear = FALSE, show_after = 0)
}

tick <- function(pb, preface, file, len = 20) {
  pb$tick(len, tokens = list(what = basename(file), preface = preface))
}

