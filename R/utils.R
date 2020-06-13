parse_filename <- function(file) {
  pattern <- stringr::regex("acs_nsqip_puf|puf_tar_[a-z]{1,4}",
                            ignore_case = TRUE)
  stopifnot(stringr::str_detect(file, pattern))
  tablename <- stringr::str_extract(file, pattern) %>% stringr::str_to_lower()
}

writerow <- function(file) {
  line <- readLines(con = file, n = 1)
  cat(paste(basename(file),line,"\n",sep="\t"), file = "../nsqipr-txt/allvars.txt",
      append = TRUE)
}
