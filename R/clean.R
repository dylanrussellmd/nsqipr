lapply(list.files(path = "../nsqipr-txt",
                  pattern = "*.txt$",
                  full.names = TRUE),
       writerow)

writerow <- function(file) {
  line <- readLines(con = file, n = 1)
  cat(paste(basename(file),line,"\n",sep="\t"), file = "../nsqipr-txt/allvars.txt",
      append = TRUE)
}

df <- readr::read_csv("../nsqipr-misc/allvars.csv", # This function will show me which columns have non-matching rows.
                      na = "",
                      col_types = readr::cols(.default = "c")) %>%
  dplyr::mutate(dplyr::across(.fns = stringr::str_to_lower)) %>%
  dplyr::rename_with(stringr::str_to_lower) %>%
  dplyr::mutate(dplyr::across(2:dplyr::last_col(), ~.x==dplyr::cur_column())) %>%
  dplyr::mutate(dplyr::across(2:dplyr::last_col(), ~!is.na(.x))) %>%
  dplyr::mutate(dplyr::across(2:dplyr::last_col(), ~dplyr::if_else(TRUE, "U+2713", "U+274C"))) %>%
  knitr::kable()

  formattable::formattable(df, align = rep("c",ncol(df)),
                           list(area(col = 2:ncol(df)) ~ formatter("span",
                                            style = function(x) style(color = ifelse(x==TRUE, "green", "black")),
                                            function(x) icontext(ifelse(x==TRUE, "ok","")))
                             ))




