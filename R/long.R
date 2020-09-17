make_reop_long <- function(df) {
  reoperations <- paste("reoperation", 1:3, sep = "")
  retorpodays <- c("retorpodays","retor2podays", "retor3podays")
  reoporcpt1 <- c("reoporcpt1","reopor2cpt1","reopor3cpt1")
  retorrelated <- c("retorrelated","retor2related","retor3related")
  reoporicd91 <- c("reoporicd91","reopor2icd91","reopor3icd91")
  reoporicd10 <- c("reopor1icd101", "reopor2icd101", "reopor3icd101")

  reop_cols <- c(reoperations, retorpodays, reoporcpt1, retorrelated, reoporicd91, reoporicd10)

  if(length(intersect(reop_cols, names(df))) > 0) {
    for(j in setdiff(reop_cols, names(df))) data.table::set(df, j = j, value = NA)
    melted <- suppressWarnings(data.table::melt(df, id.vars = "caseid", measure.vars = list(reoperations, retorpodays, reoporcpt1, retorrelated, reoporicd91, reoporicd10),
                                                variable.name = "nreoperation", value.name = c("reoperation", "retorpodays", "reoporcpt", "retorrelated", "reoporicd9", "reoporicd10")))
    na.omit(melted, "reoperation")
  }
}

make_readm_long <- function(df) {
  readmission <- paste("readmission", 1:5, sep = "")
  readmpodays <- paste("readmpodays", 1:5, sep = "")
  readmrelated <- paste("readmrelated", 1:5, sep = "")
  readmsuspreason <- paste("readmsuspreason", 1:5, sep = "")
  readmrelicd9 <- paste("readmrelicd9", 1:5, sep = "")
  readmrelicd10 <- paste("readmrelicd10", 1:5, sep = "")
  unplannedreadmission <- paste("unplannedreadmission", 1:5, sep = "")
  readmunrelsusp <- paste("readmunrelsusp", 1:5, sep = "")
  readmunrelicd9 <- paste("readmunrelicd9", 1:5, sep = "")
  readmunrelicd10 <- paste("readmunrelicd10", 1:5, sep = "")

  readm_cols <- c(readmission, readmpodays, readmrelated, readmsuspreason, readmrelicd9, readmrelicd10, unplannedreadmission, readmunrelsusp, readmunrelicd9, readmunrelicd10)

  if(length(intersect(readm_cols, names(df))) > 0) {
    for(j in setdiff(readm_cols, names(df))) data.table::set(df, j = j, value = NA)
    melted <- data.table::melt(x, id.vars = "caseid",
                               measure.vars = list(readmission, readmpodays, readmrelated, readmsuspreason, readmrelicd9,
                                                   readmrelicd10, unplannedreadmission, readmunrelsusp, readmunrelicd9, readmunrelicd10),
                               variable.name = "nreadmission",
                               value.name = c("readmission","readmpodays","readmrelated","readmsuspreason","readmrelicd9","readmrelicd10",
                                              "unplannedreadmission","readmunrelsusp","readmunrelicd9",'readmunrelicd10'))
    na.omit(melted, cols = "readmission")
  }
}



make_anesthes_other_long <- function(df) {
  if("anesthes_other" %qsin% names(df)) {
    anesthes_other = paste("anesthes_other", 1:8, sep = "")
    long <- data.table::as.data.table(cbind(df[["caseid"]],stringi::stri_split_fixed(df[["anesthes_other"]], ",", simplify = NA, n = 8, omit_empty = TRUE)))
    data.table::setnames(long, names(long), c("caseid", anesthes_other))
    long <- data.table::melt(long, id.vars = "caseid",
                             measure.vars = list(anesthes_other),
                             variable.name = "nanesthes_other",
                             value.name = "anesthes_other",
                             na.rm = TRUE)
    data.table::set(long, j = "nanesthes_other", value = stringi::stri_extract_all_regex(long[["nanesthes_other"]], "\\d"))
    return(long)
  }
}

