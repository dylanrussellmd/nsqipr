combine <- function(dir) {
  fs::dir_ls(dir, glob = "*.rds") %>%
    purrr::map_dfr(function(x) {
      readRDS(x)
    })
}
