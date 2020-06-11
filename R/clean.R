lapply(list.files(path = "../nsqipr-txt",
                  pattern = "*.txt$",
                  full.names = TRUE),
       writerow)

writerow <- function(file) {
  line <- readLines(con = file, n = 1)
  cat(paste(basename(file),line,"\n",sep="\t"), file = "../nsqipr-txt/allvars.txt",
      append = TRUE)
}
