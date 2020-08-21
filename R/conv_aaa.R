conv_aaa_cols <- function(df) {
  df %>%
    dplyr::mutate(
      pufyear = tryCatch(conv_pufyear(caseid), error = function(e) return(NULL)),
      aaa_surgind = tryCatch(conv_aaa_surgind(aaa_surgind), error = function(e) return(NULL)),
      aaa_surgap= tryCatch(conv_aaa_surgap(aaa_surgap), error = function(e) return(NULL)),
      aaa_pcl= tryCatch(conv_aaa_pcl(aaa_pcl), error = function(e) return(NULL)),
      aaa_pae= tryCatch(conv_aaa_pae(aaa_pae), error = function(e) return(NULL)),
      aaa_distext= tryCatch(conv_aaa_distext(aaa_distext), error = function(e) return(NULL)),
      aaa_mima= tryCatch(conv_aaa_mima(aaa_mima), error = function(e) return(NULL)),
      aaa_colitis_treat= tryCatch(conv_aaa_colitis_treat(aaa_colitis_treat), error = function(e) return(NULL)),
      aaa_iculos= tryCatch(conv_aaa_iculos(aaa_iculos), error = function(e) return(NULL))
    )
}

conv_aaa_surgind <- function(vec) {
  val <- c("diameter" = 1L,
           "dissection" = 2L,
           "embolization" = 3L,
           "non-ruptured symptomatic" = 4L,
           "other indication for surgery" = 5L,
           "prior endovascular intervention w/ unsatisfactory result" = 6L,
           "rupture w/ hypotension or use of pressors" = 7L,
           "rupture w/out hypotension" = 8L,
           "thrombosis" = 9L,
           "prior open intervention w/ unsatisfactory result" = 10L,
           "not documented" = NA)
  unname(val[vec])
}

conv_aaa_surgap <- function(vec) {
  val <- c("retroperitoneal" = 1L,
           "transperitoneal-midline" = 2L,
           "transperitoneal-transverse" = 3L,
           "not documented" = NA)
  unname(val[vec])
}

conv_aaa_pcl <- function(vec) {
  val <- c("above one renal" = 1L,
           "between sma & renals" = 2L,
           "between sma chr(38) renals" = 2L,
           "infrarenal" = 3L,
           "supraceliac" = 4L,
           "not documented" = NA)
  unname(val[vec])
}

conv_aaa_pae <- function(vec) {
  val <- c("infrarenal" = 1L,
           "juxtarenal" = 2L,
           "pararenal" = 3L,
           "supra-renal" = 4L,
           "type iv thoracoabdominal aneurysm" = 5L,
           "not documented" = NA)
  unname(val[vec])
}

conv_aaa_distext <- function(vec) {
  val <- c("aortic" = 1L,
           "common iliac" = 2L,
           "external iliac" = 3L,
           "internal iliac" = 4L,
           "not documented" = NA)
  unname(val[vec])
}

conv_aaa_mima <- function(vec) {
  val <- c("chronically occluded" = 1L,
           "implanted" = 2L,
           "ligated" = 3L,
           "not documented" = NA)
  unname(val[vec])
}

conv_aaa_colitis_treat <- function(vec) {
  val <- c("medical treatment" = 1L,
           "surgical treatment" = 2L,
           "not documented" = NA)
  unname(val[vec])
}

conv_age <- function(vec) {
  as.integer(ifelse(stringr::str_detect(vec, "30 or more"), "30", vec))
}
