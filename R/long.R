#' Convert reoperation columns from wide to long format
#'
#' @param df a data.table
#' @param removeFALSE a logical indicating whether to remove rows where \code{reoperation} is FALSE
#'
#' @details The data from the data table is then melted into a long format with \code{caseid} as the ID variable to allow
#' rejoining to the main table. After melting, rows with missing values are omitted to reduce the size of the table.
#' Rows where \code{reoperation} are false may also be removed with \code{removeFALSE} to reduce table size if
#' desired, but note this results in an inability to a clarify a known FALSE ("did not have a reoperation") from
#' a missing value ("do not know if there was a reoperation").
#'
#' @return a data.table
#'
#' @keywords internal
#' @examples
#' x <- data.table::data.table(caseid = c(1,2,3,4),
#' reoperation1 = c(TRUE, TRUE, FALSE, NA),
#' retorpodays = c(10, 7, NA, NA),
#' reoporcpt1 = c("44005", "37211", NA, NA),
#' retorrelated = c(TRUE, TRUE, NA, NA),
#' reoporicd91 = c("K56.69","T82.868A", NA, NA),
#' reopor1icd101 = c("K56.59", "T82.868A", NA, NA),
#' reoperation2 = c(TRUE, TRUE, FALSE, NA),
#' retor2podays = c(10, 7, NA, NA),
#' reopor2cpt1 = c("44005", "37211", NA, NA),
#' retor2related = c(TRUE, TRUE, NA, NA),
#' reopor2icd91 = c("K56.69","T82.868A", NA, NA),
#' reopor2icd101 = c("K56.59", "T82.868A", NA, NA),
#' reoperation3 = c(TRUE, TRUE, FALSE, NA),
#' retor3podays = c(10, 7, NA, NA),
#' reopor3cpt1 = c("44005", "37211", NA, NA),
#' retor3related = c(TRUE, TRUE, NA, NA),
#' reopor3icd91 = c("K56.69","T82.868A", NA, NA),
#' reopor3icd101 = c("K56.59", "T82.868A", NA, NA))
#'
#' nsqipr:::make_reop_long(x)
#' nsqipr:::make_reop_long(x, TRUE)
#'
make_reop_long <- function(df, removeFALSE = FALSE) {
  if(length(intersect(reop_cols, names(df))) > 0) {
    melted <- suppressWarnings(data.table::melt(df, id.vars = "caseid", measure.vars = list(reoperations, retorpodays, reoporcpt1, retorrelated, reoporicd91, reoporicd10),
                                                variable.name = "nreoperation", value.name = c("reoperation", "retorpodays", "reoporcpt", "retorrelated", "reoporicd9", "reoporicd10"),
                                                variable.factor = FALSE,
                                                value.factor = TRUE))
    melted <- na.omit(melted, "reoperation")
    if(removeFALSE) {
      melted <- melted[melted[["reoperation"]], ]
    }
    data.table::set(melted, j = "nreoperation", value = as.integer(melted[["nreoperation"]]))
    data.table::setorder(melted, caseid)
    return(melted)
  }
}

#' Convert readmission columns from wide to long format
#'
#' @param df a data.table
#' @param removeFALSE a logical indicating whether to remove rows where \code{reoperation} is FALSE
#'
#' @details The data from the data table is melted into a long format with \code{caseid} as the ID variable to allow
#' rejoining to the main table. After melting, rows with missing values are omitted to reduce the size of the table.
#' Rows where \code{readmission} are false may also be removed with \code{removeFALSE} to reduce table size if
#' desired, but note this results in an inability to a clarify a known FALSE ("did not have a readmission") from
#' a missing value ("do not know if there was a readmission").
#'
#' @return a data.table
#'
#' @keywords internal
#' @examples
#' x <- data.table::data.table(caseid = c(1,2,3,4),
#' readmission1 = c(TRUE, TRUE, FALSE, NA),
#' readmpodays1 = c(10, 7, NA, NA),
#' readmrelated1 = c(TRUE, FALSE, NA, NA),
#' readmsuspreason1 = c("Reason", "Reason", NA, NA),
#' readmrelicd91 = c("111", "222", NA, NA),
#' readmrelicd101 = c("1111","2222", NA, NA),
#' unplannedreadmission1 = c(TRUE, FALSE, NA, NA),
#' readmunrelsusp1 = c("Reason", NA, NA, NA),
#' readmunrelicd91 = c("111", NA, NA, NA),
#' readmunrelicd101 = c("1111",NA,NA,NA),
#' readmission2 = c(TRUE, TRUE, FALSE, NA),
#' readmpodays2 = c(10, 7, NA, NA),
#' readmrelated2 = c(TRUE, FALSE, NA, NA),
#' readmsuspreason2 = c("Reason", "Reason", NA, NA),
#' readmrelicd92 = c("111", "222", NA, NA),
#' readmrelicd102 = c("1111","2222", NA, NA),
#' unplannedreadmission2 = c(TRUE, FALSE, NA, NA),
#' readmunrelsusp2 = c("Reason", NA, NA, NA),
#' readmunrelicd92 = c("111", NA, NA, NA),
#' readmunrelicd102 = c("1111",NA,NA,NA),
#' readmission3 = c(TRUE, TRUE, FALSE, NA),
#' readmpodays3 = c(10, 7, NA, NA),
#' readmrelated3 = c(TRUE, FALSE, NA, NA),
#' readmsuspreason3 = c("Reason", "Reason", NA, NA),
#' readmrelicd93 = c("111", "222", NA, NA),
#' readmrelicd103 = c("1111","2222", NA, NA),
#' unplannedreadmission3 = c(TRUE, FALSE, NA, NA),
#' readmunrelsusp3 = c("Reason", NA, NA, NA),
#' readmunrelicd93 = c("111", NA, NA, NA),
#' readmunrelicd103 = c("1111",NA,NA,NA),
#' readmission4 = c(TRUE, TRUE, FALSE, NA),
#' readmpodays4 = c(10, 7, NA, NA),
#' readmrelated4 = c(TRUE, FALSE, NA, NA),
#' readmsuspreason4 = c("Reason", "Reason", NA, NA),
#' readmrelicd94 = c("111", "222", NA, NA),
#' readmrelicd104 = c("1111","2222", NA, NA),
#' unplannedreadmission4 = c(TRUE, FALSE, NA, NA),
#' readmunrelsusp4 = c("Reason", NA, NA, NA),
#' readmunrelicd94 = c("111", NA, NA, NA),
#' readmunrelicd104 = c("1111",NA,NA,NA),
#' readmission5 = c(TRUE, TRUE, FALSE, NA),
#' readmpodays5 = c(10, 7, NA, NA),
#' readmrelated5 = c(TRUE, FALSE, NA, NA),
#' readmsuspreason5 = c("Reason", "Reason", NA, NA),
#' readmrelicd95 = c("111", "222", NA, NA),
#' readmrelicd105 = c("1111","2222", NA, NA),
#' unplannedreadmission5 = c(TRUE, FALSE, NA, NA),
#' readmunrelsusp5 = c("Reason", NA, NA, NA),
#' readmunrelicd95 = c("111", NA, NA, NA),
#' readmunrelicd105 = c("1111",NA,NA,NA))
#'
#' nsqipr:::make_readm_long(x)
#' nsqipr:::make_reop_long(x, TRUE)
#'
make_readm_long <- function(df, removeFALSE = FALSE) {
  if(length(intersect(readm_cols, names(df))) > 0) {
    melted <- suppressWarnings(data.table::melt(df, id.vars = "caseid",
                               measure.vars = list(readmission, readmpodays, readmrelated, readmsuspreason, readmrelicd9,
                                                   readmrelicd10, unplannedreadmission, readmunrelsusp, readmunrelicd9, readmunrelicd10),
                               variable.name = "nreadmission",
                               value.name = c("readmission","readmpodays","readmrelated","readmsuspreason","readmrelicd9","readmrelicd10",
                                              "unplannedreadmission","readmunrelsusp","readmunrelicd9",'readmunrelicd10'),
                               variable.factor = FALSE,
                               value.factor = TRUE))
    melted <- na.omit(melted, cols = "readmission")
    if(removeFALSE) {
      melted <- melted[melted[["readmission"]], ]
    }
    data.table::set(melted, j = "nreadmission", value = as.integer(melted[["nreadmission"]]))
    data.table::setorder(melted, caseid)
    return(melted)
  }
}

#' Convert anesthes_other column from wide to long format
#'
#' @param df a data.table
#'
#' @details The data contained in the 8 "anesthes_other" columns created by \code{make_anesthes_other_cols}
#' are converted into a long format.
#'
#' If "anesthes_other" is a column in \code{df}, it will be broken into a long format with
#' \code{caseid} as the ID variable for joining back to the main table. This is because many of the NSQIP
#' PUF datasets input multiple values into a single "anesthes_other" column. For example,
#' "General, Spinal, MAC/IV Sedation" may be an entry in the raw data set. This makes
#' parsing for patients that received "Spinal" anesthesia at any point, for example, very difficult.
#'
#' Note that this does not alter the "anesthes" column or include the anesthetic technique stored in the
#' "anesthes" column in the resulting "anesthes_other" data table.
#'
#' @return a data.table
#'
#' @keywords internal
#' @examples
#' x <- data.table::data.table(caseid = c(1,2,3,4),
#' anesthes_other = c("General","General, Spinal", "General, Spinal, MAC/IV Sedation", NA),
#' anesthes_other1 = c("General", "General", "General", NA),
#' anesthes_other2 = c(NA, "Spinal", "Spinal", NA),
#' anesthes_other3 = c(NA, NA, "MAC/IV Sedation", NA),
#' anesthes_other4 = c(NA, NA, NA, NA),
#' anesthes_other5 = c(NA, NA, NA, NA),
#' anesthes_other6 = c(NA, NA, NA, NA),
#' anesthes_other7 = c(NA, NA, NA, NA),
#' anesthes_other8 = c(NA, NA, NA, NA))
#'
#' nsqipr:::make_anesthes_other_long(x)
#'
make_anesthes_other_long <- function(df) {
  if("anesthes_other" %qsin% names(df)) {
    long <- suppressWarnings(data.table::melt(df, id.vars = "caseid",
                             measure.vars = anesthes_other_cols,
                             variable.name = "nanesthes_other",
                             value.name = "anesthes_other",
                             na.rm = TRUE,
                             variable.factor = FALSE,
                             value.factor = TRUE))
    data.table::set(long, j = "nanesthes_other", value = stringi::stri_extract_all_regex(long[["nanesthes_other"]], "\\d", simplify = TRUE))
    data.table::set(long, j = "nanesthes_other", value = as.integer(long[["nanesthes_other"]]))
    data.table::setorder(long, caseid)
    return(long)
  }
}

#' Create a CPT, Procedure Name, and WRVU Long Table
#'
#' @param df a data.table
#'
#' @details If all of the requisite columns are present in a data.table, this function
#' will create a long format data table that contains the CPT, procedure name, and WRVU
#' of each procedure each patient underwent. \code{caseid} is retained in order to allow joining
#' back to a main table. Each procedure is numbered sequentially beginning at 1. This is stored
#' in \code{nproc}. The only number that holds significance is "1", as this is the "primary procedure"
#' as entered into the original data set.
#'
#' In order to distinguish between an "other" procedure and a "concurrent" procedure, utilize the
#' \code{primarysurg} variable: TRUE is equivalent to an "other" procedure and FALSE is equivalent
#' to a "concurrent" procedure. This variable is so named because the only difference between an
#' "other" procedure and a "concurrent" procedure is that the latter is a procedure not performed
#' by the primary surgical team.
#'
#' @return a data.table
#'
#' @keywords internal
#'
make_cpt_long <- function(df) {
  if(all(cpt_cols %qsin% names(df))) {
    melted <- suppressWarnings(data.table::melt(df, id.vars = "caseid",
                                                measure.vars = list(cpt, proc, wrvu),
                                                variable.name = "nproc",
                                                value.name = c("cpt","proc","wrvu"),
                                                variable.factor = FALSE))
    melted <- na.omit(melted, cols = "cpt")
    data.table::set(melted, j = "nproc", value = as.integer(melted[["nproc"]]))
    data.table::set(melted, j = "primarysurg", value = melted[["nproc"]] <= 11)
    data.table::set(melted, j = "nproc", value = data.table::rowid(melted[["caseid"]]))
    data.table::setorder(melted, caseid)
    return(melted)
  }
}

#' Convert pan_percdrainage column from wide to long format
#'
#' @param df a data.table
#'
#' @details The data contained in the 4 "pan_percdrainage" columns created by \code{make_pan_percdrainage_cols}
#' are converted into a long format.
#'
#' If "pan_percdrainage" is a column in \code{df}, it will be broken into a long format with
#' \code{caseid} as the ID variable for joining back to the main table. This is because the targeted pancreatectomy
#' datasets input multiple values into a single "pan_percdrainage" column. For example,
#' "Yes-other, Yes-bile, Yes-pus" may be an entry in the raw data set. This makes
#' parsing for patients that had purulent percutaneous drainage at any point, for example, very difficult.
#'
#' Note that this does not alter the "pan_percdrainage" column.
#'
#' @return a data.table
#'
#' @keywords internal
#' @examples
#' x <- data.table::data.table(caseid = 1:15,
#' pan_percdrainage = c("Yes-other", "Yes-bile", "Yes-pus", "Yes-amylase-rich fluid",
#' "Yes-amylase-rich fluid,Yes-pus", "Yes-bile,Yes-other", "Yes-amylase-rich fluid,Yes-pus,Yes-other",
#' "Yes-amylase-rich fluid,Yes-bile,Yes-other", "Yes-pus,Yes-bile", "Yes-pus,Yes-other",
#' "Yes-amylase-rich fluid,Yes-pus,Yes-bile", "Yes-amylase-rich fluid,Yes-other",
#' "Yes-amylase-rich fluid,Yes-pus,Yes-bile,Yes-other", "Yes-amylase-rich fluid,Yes-bile",
#' "Yes-pus,Yes-bile,Yes-other"))
#'
#' nsqipr:::make_pan_percdrainage_cols(x)
#' nsqipr:::make_pan_percdrainage_long(x)
#'
make_pan_percdrainage_long <- function(df) {
  if("pan_percdrainage" %qsin% names(df)) {
    long <- suppressWarnings(data.table::melt(df, id.vars = "caseid",
                                              measure.vars = pan_percdrainage_cols,
                                              variable.name = "npercdrainage",
                                              value.name = "percdrainage",
                                              na.rm = TRUE,
                                              variable.factor = FALSE,
                                              value.factor = TRUE))
    data.table::set(long, j = "npercdrainage", value = stringi::stri_extract_all_regex(long[["npercdrainage"]], "\\d", simplify = TRUE))
    data.table::set(long, j = "npercdrainage", value = as.integer(long[["npercdrainage"]]))
    data.table::setorder(long, caseid)
    return(long)
  }
}

#' Convert amylase columns from wide to long format
#'
#' @param df a data.table
#'
#' @details The data contained in the 3 amylase columns of the targeted pancreatectomy
#' dataset ("pan_amylase_pod1", "pan_amylase_pod230", and "damylase") are converted into a
#' long format. This reduces the total number of columns from 3 to 2 and makes the data
#' more intuitive.
#'
#' If both "pan_amylase_pod1" and "pan_amylase_pod230" are columns in \code{df},
#' it will be broken into a long format with \code{caseid} as the ID variable
#' for joining back to the main table. The amylase values from both "pan_amylase_pod1"
#' and "pan_amylase_pod230" are placed in the "amylase" column". The POD on which
#' this value was acquired (either 1 if from "pan_amylase_pod1" column or set equal
#' to the value of the "damylase" column) is placed in the "pod" column.
#'
#' Note that this does not alter the original columns.
#'
#' @return a data.table
#'
#' @keywords internal
#' @examples
#' x <- data.table::data.table(caseid = 1:30,
#' pan_amylase_pod1 = c(NA, sample(1:10000, 29)),
#' pan_amylase_pod230 = c(NA, sample(1000:10000, 29)),
#' damylase = c(NA, sample(2:30, 29)))
#'
#' nsqipr:::make_amylase_long(x)
#'
make_amylase_long <- function(df) {
  if(all(pan_amylase_cols %qsin% names(df))) {
    melted <- suppressWarnings(data.table::melt(df, id.vars = c("caseid", "damylase"),
                                                measure.vars = pan_amylase_cols,
                                                variable.name = "pod",
                                                value.name = "amylase",
                                                variable.factor = FALSE,
                                                value.factor = FALSE,
                                                na.rm = TRUE))
    data.table::set(melted, j = c("damylase","pod"), value = list(
                        ifelse(melted[["pod"]] == "pan_amylase_pod1", 1, melted[["damylase"]]),
                        NULL))
    data.table::setnames(melted, "damylase","pod")
    data.table::setorder(melted, caseid)
    return(melted)
  }
}
