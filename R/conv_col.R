conv_col_cols <- function(df) {
  df %>%
    dplyr::mutate(
      pufyear = tryCatch(conv_pufyear(caseid), error = function(e) return(NULL)),
      col_indication = tryCatch(conv_col_indication(col_indication), error = function(e) return(NULL)),
      col_emergent = tryCatch(conv_col_emergent(col_emergent), error = function(e) return(NULL)),
      col_open_assist = tryCatch(conv_col_open_assist(col_approach), error = function(e) return(NULL)),
      col_unplanned_conversion = tryCatch(conv_col_unplanned_conversion(col_approach), error = function(e) return(NULL)),
      col_approach = tryCatch(conv_col_approach(col_approach), error = function(e) return(NULL)),
      col_malignancyt = tryCatch(conv_col_malignancyt(col_malignancyt), error = function(e) return(NULL)),
      col_malignancyn = tryCatch(conv_col_malignancyn(col_malignancyn), error = function(e) return(NULL)),
      col_malignancym = tryCatch(conv_col_malignancym(col_malignancym), error = function(e) return(NULL)),
      col_leak_treatment = tryCatch(conv_col_leak_treatment(col_anastomotic), error = function(e) return(NULL)),
      col_anastomotic = tryCatch(conv_col_anastomotic(col_anastomotic), error = function(e) return(NULL))
    )
}

conv_col_anastomotic <- function(vec) {
  stringr::str_detect(vec, "yes-|leak, ")
}

conv_col_leak_treatment <- function(vec) {
  val <- c("no" = NA,
           "yes-reoperation" = 1L,
           "yes-percutaneous intervention" = 2L,
           "yes-no intervention required" = 3L,
           "unknown" = NA,
           "no definitive diagnosis of leak/leak related abscess" = NA,
           "leak, treated w/ reoperation" = 1L,
           "leak, treated w/ interventional means" = 2L,
           "leak, no treatment intervention documented" = 3L,
           "leak, treated w/ non-interventional/non-operative means" = 4L)
  unname(val[vec])
}

conv_col_malignancym <- function(vec) {
  val <- c("m0/mx" = 1L,
           "m1" = 2L,
           "m1a" = 3L,
           "m1b" = 4L,
           "unknown" = NA,
           "n/a" = NA
  )
  unname(val[vec])
}

conv_col_malignancyt <- function(vec) {
  val <- c("t0" = 1L,
           "t1" = 2L,
           "t2" = 3L,
           "t3" = 4L,
           "t4" = 5L,
           "t4a" = 6L,
           "t4b" = 7L,
           "tis" = 8L,
           "tx" = 9L,
           "unknown" = NA,
           "n/a" = NA
           )
  unname(val[vec])
}

conv_col_malignancyn <- function(vec) {
  val <- c("n0" = 1L,
           "n1" = 2L,
           "n1a" = 3L,
           "n1b" = 4L,
           "n1c" = 5L,
           "n2" = 6L,
           "n2a" = 7L,
           "n2b" = 8L,
           "nx" = 9L,
           "unknown" = NA,
           "n/a" = NA
           )
  unname(val[vec])
}

conv_col_open_assist <- function(vec) {
  stringr::str_detect(vec, "w/ open assist|hand assisted")
}

conv_col_unplanned_conversion <- function(vec) {
  stringr::str_detect(vec, "w/ unplanned conversion to open")
}

conv_col_approach <- function(vec) {
  val <- c("endoscopic" = 1L,
           "endoscopic w/ open assist" = 1L,
           "endoscopic w/ unplanned conversion to open" = 1L,
           "hybrid" = 2L,
           "hybrid w/ open assist" = 2L,
           "hybrid w/ unplanned conversion to open" = 2L,
           "laparoscopic" = 3L,
           "laparoscopic w/ open assist" = 3L,
           "laparoscopic w/ unplanned conversion to open" = 3L,
           "laparoscopic hand assisted" = 3L,
           "notes" = 4L,
           "notes w/ open assist" = 4L,
           "notes w/ unplanned conversion to open" = 4L,
           "open" = 5L,
           "open (planned)" = 5L,
           "other"= 6L,
           "other mis approach" = 7L,
           "other mis approach w/ open assist" = 7L,
           "other mis approach w/ unplanned conversion to open" = 7L,
           "robotic" = 8L,
           "robotic w/ open assist" = 8L,
           "robotic w/ unplanned conversion to open" = 8L,
           "sils" = 9L,
           "sils w/ open assist" = 9L,
           "sils w/ unplanned conversion to open" = 9L,
           "unknown" = NA
           )
  unname(val[vec])
}

conv_col_emergent <- function(vec) {
  val <- c("bleeding" = 1L,
           "obstruction" = 2L,
           "perforation" = 3L,
           "toxic colitis (toxic megacolon, c. diff w/out perforation, ischemic colitis)" = 4L,
           "other (enter icd-9 code)" = 5L,
           "other (enter icd-10 code)" = 6L,
           "unknown" = NA
           )
  unname(val[vec])
}

conv_col_indication <- function(vec) {
  val <- c("acute diverticulitis" = 1L,
           "bleeding" = 2L,
           "chronic diverticular disease" = 3L,
           "colon cancer" = 4L,
           "colon cancer w/ obstruction" = 5L,
           "crohn's disease" = 6L,
           "enterocolitis (e.g. c. difficile)" = 7L,
           "non-malignant polyp" = 8L,
           "other-enter icd-9 for diagnosis" = 9L,
           "other-enter icd-10 for diagnosis" = 10L,
           "ulcerative colitis" = 11L,
           "volvulus" = 12L,
           "unknown" = NA
           )
  unname(val[vec])
}
