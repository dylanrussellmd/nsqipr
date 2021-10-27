#' Convert ACS NSQIP PUF columns
#'
#' @param df a data table to be cleaned
#' @param filename the name of the file from which the data table has been read in
#'
#' @details If the file being processed is an ACS NSQIP PUF (i.e., not a targeted data set),
#' it will be processed by this function. This function determines how data cleaning steps specific
#' to ACS NSQIP PUF files should proceed.
#'
#' @keywords internal
#'
conv_acs_cols <- function(df, filename) {
  get_pufyear(df, filename)
  conv_hispanic(df)
  conv_(df, "race", conv_race)
  conv_(df, "age", conv_age)
  conv_(df, "inout", conv_inout)
  conv_(df, "diabetes", insulin, newcol = "insulin")
  conv_(df, "diabetes", conv_notno)
  conv_(df, "dyspnea", when_dyspnea, newcol = "when_dyspnea")
  conv_(df, "dyspnea", conv_notno)
  conv_(df, "prsepis", type_prsepis, newcol = "type_prsepis")
  conv_(df, "prsepis", conv_notno)
  check_comaneurograft(df)
}

#### ---- FACTOR LISTS (THESE DEFINE THE FACTOR LEVELS FOR VARIOUS COLUMNS) ---- ####
sex <- list(Male = "male",
            Female = "female",
            `Non-binary` = "non-binary")
fnstatus1 <- list(Independent = "Independent",
                 `Partially dependent` = "Partially Dependent",
                 `Totally dependent` = "Totally Dependent")
fnstatus2 <- fnstatus1
typeintoc <- list(`Cardiac arrest requiring CPR` = "Cardiac Arrest Requiring CPR",
                  `Myocardial infarction` = "Myocardial Infarction",
                  `Unplanned intubation` = "Unplanned Intubation")
airtra <- list(None = "None",
               `Lip laceration or hematoma` = "Lip laceration or hematoma",
               `Tooth chipped, loosened, or lost` = "Tooth chipped, loosened or lost",
               `Tongue laceration or hematoma` = "Tongue laceration or hematoma",
               `Pharyngeal laceration` = "Pharyngeal laceration",
               `Laryngeal laceration` = "Laryngeal laceration",
               `Failure to intubate` = "Failure to intubate")
opnote <- list(Attending = "Attending",
               Resident = "Resident")
attend <- list(`Attending alone` = "Attending Alone",
               `Attending and resident in OR` = c("Attending in OR","Attending & Resident in OR"),
               `Attending in OR suite` = "Attending in OR Suite",
               `Attending not present, but available` = "Attending Not Present, but Available")
wound_closure <- list(`All layers of incision (deep and superficial) fully closed` = "All layers of incision (deep and superficial) fully closed",
                      `Only deep layers closed; superficial left open` = "Only deep layers closed; superficial left open",
                      `No layers of incision are surgically closed` = "No layers of incision are surgically closed")
transt <- list(`Acute care hospital` = c("From acute care hospital inpatient","Acute Care Hospital","VA Acute Care Hospital"),
               `Admitted from home` = c("Not transferred (admitted from home)","Admitted directly from home"),
               `Chronic care facility` = c("Nursing home - Chronic care - Intermediate care","Chronic Care Facility","VA Chronic Care Facility"),
               `Outside emergency department` = "Outside emergency department",
               Other = c("Transfer from other","Other"))
readmsuspreason1 <- list(`Superficial incisional SSI` = "Superficial Incisional SSI",
                         `Deep incisional SSI` = "Deep Incisional SSI",
                         `Organ-space SSI` = "Organ/Space SSI",
                         `Wound disruption` = "Wound Disruption",
                         Pneumonia = "Pneumonia",
                         `Unplanned intubation` = "Unplanned Intubation",
                         `Pulmonary embolism` = "Pulmonary Embolism",
                         `On ventilator > 48 hours` = "On Ventilator > 48 hours",
                         `Progressive renal insufficiency` = "Progressive Renal Insufficiency",
                         `Acute renal failure` = "Acute Renal Failure",
                         `Urinary tract infection` = "Urinary Tract Infection",
                         `Cerebrovascular accident` = "CVA",
                         `Cardiac arrest requiring CPR` = "Cardiac Arrest Requiring CPR",
                         `Myocardial infarction` = "Myocardial Infarction",
                         `Bleeding requiring transfusion (within 72 hours of surgery start time)` = "Bleeding Requiring Transfusion (72h of surgery start time)",
                         `Vein thrombosis requiring therapy` = c("Vein Thrombosis Requiring Therapy","DVT Requiring Therapy"),
                         Sepsis = "Sepsis",
                         `Septic shock` = "Septic Shock",
                         Other = c("Other (list ICD 9 code)","Other (list ICD 10 code)", "Other (list ICD9 code)", "Other (list ICD10 code)"),
                         `C. difficile` = "C. diff",
                         `Graft/prosthesis/flap failure` = "Graft/Prosthesis/Flap Failure",
                         `Peripheral nerve injury` = "Peripheral Nerve Injury")
readmunrelsusp1 <- readmsuspreason1
readmsuspreason2 <- readmsuspreason1
readmunrelsusp2 <- readmsuspreason1
readmsuspreason3 <- readmsuspreason1
readmunrelsusp3 <- readmsuspreason1
readmsuspreason4 <- readmsuspreason1
readmunrelsusp4 <- readmsuspreason1
readmsuspreason5 <- readmsuspreason1
readmunrelsusp5 <- readmsuspreason1
dischdest <- list(`Skilled care, not home` = "Skilled Care, Not Home",
                  `Unskilled facility, not home` = "Unskilled Facility Not Home",
                  `Facility which was home` = "Facility Which was Home",
                  Home = "Home",
                  `Separate acute care` = "Separate Acute Care",
                  Rehab = "Rehab",
                  Expired = "Expired",
                  `Against medical advice (AMA)` = "Against Medical Advice (AMA)",
                  `Multi-level senior community` = "Multi-level Senior Community",
                  Hospice = "Hospice")
anesthes <- list(`Epidural` = "Epidural",
                 `General` = "General",
                 `Local` = "Local",
                 `Monitored anesthesia care` = c("MAC/IV Sedation","Monitored Anesthesia Care",
                                                 "Monitored Anesthesia Care/IV Sedation", "Monitored anesthesia care/IV sedation"),
                 `None` = "None",
                 `Other` = "Other",
                 `Regional` = "Regional",
                 `Spinal` = "Spinal")
surgspec <- list(`Cardiac surgery` = "Cardiac Surgery",
                 `General surgery` = "General Surgery",
                 `Gynecology` = "Gynecology",
                 `Neurosurgery` = "Neurosurgery",
                 `Orthopedics` = "Orthopedics",
                 `Otolaryngology (ENT)` = "Otolaryngology (ENT)",
                 `Plastics` = "Plastics",
                 `Thoracic` = "Thoracic",
                 `Urology` = "Urology",
                 `Vascular` = "Vascular",
                 `Interventional radiologist` = "Interventional Radiologist",
                 `Ophthalmology` = "Ophthalmology",
                 `Podiatry` = "Podiatry",
                 `Oral surgery` = "Oral Surgery",
                 `Obstetrics` = "Obstetrics",
                 `Other` = "Other")

#### ---- LONG COLUMNS ---- ####
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

reoperation <- paste("reoperation", 1:3, sep = "")
retorpodays <- c("retorpodays","retor2podays", "retor3podays")
reoporcpt <- c("reoporcpt1","reopor2cpt1","reopor3cpt1")
retorrelated <- c("retorrelated","retor2related","retor3related")
reoporicd9 <- c("reoporicd91","reopor2icd91","reopor3icd91")
reoporicd10 <- c("reopor1icd101", "reopor2icd101", "reopor3icd101")

anesthes_other <- paste("anesthes_other", 1:8, sep = "")

proc <- c("prncptx", paste("otherproc", 1:10, sep = ""), paste("concurr", 1:10, sep = ""))
cpt <- c("cpt", paste("othercpt", 1:10, sep = ""), paste("concpt", 1:10, sep = ""))
workrvu <- c("workrvu", paste("otherwrvu", 1:10, sep = ""), paste("conwrvu", 1:10, sep = ""))

#### ---- FUNCTIONS ---- ####

#' Convert readmission columns from wide to long format
#'
#' @param df a data.table
#' @param removeFALSE a logical vector indicating whether or not to remove rows with a FALSE value.
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
#' readmission4 = c(TRUE, TRUE, NA, NA),
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
#' nsqipr:::make_readm_long(x, TRUE)
#'
make_readm_long <- function(df, removeFALSE = FALSE) {
  removeFALSE <- ifelse(removeFALSE, "readmission", rlang::missing_arg()) # Allows removeFALSE to be used as a logical switch
  make_cols_long(df, readmission, readmpodays, unplannedreadmission, readmrelated, readmsuspreason, readmunrelsusp, readmrelicd9, readmrelicd10,  readmunrelicd9, readmunrelicd10,
                 na.cols = "readmission",
                 removeFALSE = removeFALSE,
                 reorder = TRUE)
}

#' Convert reoperation columns from wide to long format
#'
#' @param df a data.table
#' @param removeFALSE a logical vector indicating whether or not to remove rows with a FALSE value.
#'
#' @details The data from the data table is then melted into a long format with \code{caseid} as the ID variable to allow
#' rejoining to the main table. After melting, rows with missing values are omitted to reduce the size of the table.
#' Rows where \code{reoperation} are false may also be removed with \code{removeFALSE} to reduce table size if
#' desired, but note this results in an inability to a clarify a known FALSE ("did not have a reoperation") from
#' a missing value ("do not know if there was a reoperation").
#'
#' Note that this function does not reorder \code{nreoperation} after converting to long and removing records with
#' NA, FALSE, or both. This is because the third reoperation (\code{reoperation3}) has significance as representing
#' 3 or more reoperations.
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
  removeFALSE <- ifelse(removeFALSE, "reoperation", rlang::missing_arg()) # Allows removeFALSE to be used as a logical switch
  make_cols_long(df, reoperation, retorpodays, reoporcpt, retorrelated, reoporicd9, reoporicd10,
                 na.cols = "reoperation",
                 removeFALSE = removeFALSE)
}

#' Convert anesthes_other column from wide to long format
#'
#' @param df a data.table
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
#' @examples
#' x <- data.table::data.table(
#' anesthes_other = c("General","General, Spinal", "General, Spinal, MAC/IV Sedation", NA)
#' )
#' nsqipr:::make_anesthes_other_cols(x)
#'
make_anesthes_other_long <- function(df) {
  make_commas_long(df, anesthes_other, levels = anesthes)
}

#' Convert CPT, Procedure Name, and WRVU from wide to long format
#'
#' @param df a data.table
#'
#' @details If all of the requisite columns are present in a data.table, this function
#' will create a long format data table that contains the CPT, procedure name, and WRVU
#' of each procedure each patient underwent. \code{caseid} is retained in order to allow joining
#' back to a main table. Each procedure is numbered sequentially beginning at 1. This is stored
#' in \code{nproc}. The only number that holds significance is "1", as this is the "primary procedure"
#' as entered into the original data set. If any CPT codes are "NA", these records are removed
#' and the "nproc" column is renumbered appropriately.
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
  make_cols_long(df, cpt, proc, workrvu, variable.name = "nproc", na.cols = "cpt", reorder = TRUE, fn = function(x) {
    data.table::set(x, j = "primarysurg", value = as.integer(x[["nproc"]]) <= 11)
  })
}

#' Remove coma, neuro deficit, and graft columns after 2010
#'
#' @param df a data.table from which to remove the coma, neuro deficit, and graft outcome columns
#'
#' @details According to NSQIP, Graft failure, Coma, and Peripheral Nerve Injury should not be
#' considered accurate for any PUF after 2010 (see the \link[https://www.facs.org/quality-programs/acs-nsqip/participant-use]{NSQIP} website).
#'
#' @return a data.table
#' @keywords internal
#'
#' @examples
#' x <- data.table::data.table(
#' cnscoma = c(TRUE, TRUE, FALSE),
#' ncnscoma = c(1,2,3),
#' dcnscoma = c(1,2,3),
#' neurodef = c(TRUE, TRUE, FALSE),
#' nneurodef = c(1,2,3),
#' dneurodef = c(1,2,3),
#' othgrafl = c(TRUE, TRUE, FALSE),
#' nothgrafl = c(1,2,3),
#' dothgrafl = c(1,2,3),
#' distraction = c("Test","test","test")
#' )
#'
#' nsqipr:::get_pufyear(x, "acs_nsqip_puf10.txt")
#' nsqipr:::check_comaneurograft(x)
#' nsqipr:::get_pufyear(x, "acs_nsqip_puf12.txt")
#' nsqipr:::check_comaneurograft(x)
#'
check_comaneurograft <- function(df) {
  if(unique(df[["pufyear"]]) > 2010) {
    cols <- c("cnscoma","ncnscoma","dcnscoma","neurodef","nneurodef","dneurodef","othgrafl","nothgrafl","dothgrafl")
    for(j in intersect(cols, names(df))) data.table::set(df, j = j, value = NA)
  }
  invisible(df)
}

#' Add or update Hispanic ethnicity column
#'
#' @param df a data.table to add to or update with an \code{ethnicity_hispanic} column
#'
#' @details \code{ethnicity_hispanic} was not added until the 2008 NSQIP PUF when \code{race} was revised to
#' \code{race_new}. Data regarding hispanic ethnicity was hard coded directly into the old \code{race} variable
#' (such as "Hispanic, White"). In order to marry early and later datasets, this information must be extracted
#' from \code{race} and a new \code{ethnicity_hispanic} column created.
#'
#' If the data provided already has a \code{ethnicity_hispanic} column present, this column is simply converted
#' into a logical vector.
#'
#' @return a data table
#' @keywords internal
#'
#' @examples
#' x <- data.table::data.table(
#' race = c("Hispanic, White", "White, Not of Hispanic Origin","Hispanic, Black",
#' "Black, Not of Hispanic Origin", "Hispanic, Color Unknown", "White", "Black or African American",
#' "American Indian or Alaska Native", "Asian", "Native Hawaiian or Pacific Islander",
#' "Asian or Pacific Islander", NA),
#' ethnicity_hispanic = c(NA, NA, NA, NA, NA, "Yes", "No", "Yes", "No", NA, NA, "Yes")
#' )
#'
#' nsqipr:::conv_hispanic(x)
#' x
#'
conv_hispanic <- function(df) {
  if("ethnicity_hispanic" %chin% names(df)) {
    vec <- ifelse(!is.na(df[["ethnicity_hispanic"]]),
                  conv_yesno(df[["ethnicity_hispanic"]]),
                  conv_hispanic_helper(df))
  } else {
    vec <- ifelse(stringi::stri_detect_regex(df[["race"]], "hispanic", opts_regex = list(case_insensitive = TRUE)),
                  stringi::stri_detect_regex(df[["race"]], "^hispanic,", opts_regex = list(case_insensitive = TRUE)),
                  NA)
  }
  data.table::set(df, j = "ethnicity_hispanic", value = vec)
}

#' @describeIn conv_hispanic A helper function for updating the \code{ethnicity_hispanic} column
conv_hispanic_helper <- function(df) {
  ifelse(stringi::stri_detect_regex(df[["race"]], "hispanic", opts_regex = list(case_insensitive = TRUE)),
         stringi::stri_detect_regex(df[["race"]], "^hispanic,", opts_regex = list(case_insensitive = TRUE)),
         NA)
}

#' Convert race to factor
#'
#' @param vec a character vector of races to be converted to a factor
#' @param pacific whether to consider "Asian or Pacific Islander" as part of the "Native Hawaiian or Pacific islander"
#' level or part of the "Asian" level.
#'
#' @details 2005-2007 NSQIP PUFs included a race called "Asian or Pacific islander". Later PUFs split these into "Asian" and
#' "Native Hawaiian or Pacific islander". In order to combine PUFs, a decision must be made as to which group to assign
#' "Asian or Pacific islander" to. To assign them to "Asian", \code{pacific} should be set to "asian". To assign them to
#' "Native Hawaiian or Pacific islander", \code{pacific} should be set to "hawaiian".
#'
#' @return a factor vector
#'
#' @keywords internal
#' @examples
#' x <- c("White","Black or African American","Asian or Pacific Islander")
#' nsqipr:::conv_race(x)
#' x
#'
#' x <- c("White","Black or African American","Asian or Pacific Islander")
#' nsqipr:::conv_race(x, pacific = "hawaiian")
#' x
conv_race <- function(vec, pacific = "asian") {
  asian <- list(White = c("Hispanic, White", "White, Not of Hispanic Origin","White"),
                Black = c("Hispanic, Black","Black, Not of Hispanic Origin", "Black or African American"),
                `American Indian or Alaska native` = "American Indian or Alaska Native",
                `Asian` = c("Asian", "Asian or Pacific Islander"),
                `Native Hawaiian or Pacific islander` = "Native Hawaiian or Pacific Islander")
  hawaiian <- list(White = c("Hispanic, White", "White, Not of Hispanic Origin","White"),
                   Black = c("Hispanic, Black","Black, Not of Hispanic Origin", "Black or African American"),
                   `American Indian or Alaska native` = "American Indian or Alaska Native",
                   `Asian` = "Asian",
                   `Native Hawaiian or Pacific islander` = c("Native Hawaiian or Pacific Islander","Asian or Pacific Islander"))

  levels <- switch(pacific,
                    "asian" = asian,
                    "hawaiian" = hawaiian)

  vec %^% levels
}

#' Convert inout to logical
#'
#' @param vec a character vector of values to convert
#'
#' @details If "Inpatient", will result in true. If given NA, will return NA.
#'
#' @return an integer vector
#' @keywords internal
#'
#' @examples
#'  nsqipr:::conv_inout(c("Inpatient", "Outpatient", NA))
#'
conv_inout <- function(vec) {
  stringi::stri_detect_fixed(vec, "Inpatient", opts_fixed = list(case_insensitive = TRUE))
}

#' Convert age to integer
#'
#' @param vec a character vector of values to convert
#'
#' @details NSQIP encodes anyone over the age of 90 as "90+". This converts all "90+" to 90.
#' If given NA, will return NA.
#'
#' @return an integer vector
#' @keywords internal
#'
#' @examples
#' nsqipr:::conv_age(c("18","45","90+",NA))
#'
conv_age <- function(vec) {
  as.integer(ifelse(stringi::stri_detect_fixed(vec, "90+", opts_fixed = list(case_insensitive = TRUE)), "90", vec))
}

#' Parse a column for insulin usage
#'
#' @param vec a character vector of values to convert
#'
#' @details NSQIP encodes the \code{diabetes} column as either "no", "non-insulin", "oral", or "insulin".
#' This function checks that the value is both \bold{not} "no" and also equal to "insulin". Returns
#' FALSE if either "oral" or "non-insulin". Returns NA if \code{diabetes} is either "no" or NA.
#'
#' @return a logical vector
#' @keywords internal
#'
#' @examples
#'  nsqipr:::insulin(c("no","non-insulin","oral","insulin",NA))
#'
insulin <- function(vec) {
  ifelse(stringi::stri_detect_regex(vec, "^no$", opts_regex = list(case_insensitive = TRUE)), NA,
                                    conv_notno(vec) & stringi::stri_detect_regex(vec, "^insulin$", opts_regex = list(case_insensitive = TRUE)))
}

#' Parse a column for type of dyspnea
#'
#' @param vec a character vector of values to convert
#'
#' @details NSQIP encodes the \code{dyspnea} column as either "no", "at rest", or "moderate exertion".
#' This function factors the vector for the levels "At rest" and "Moderate exertion".
#'
#' @return a factor vector
#' @keywords internal
#'
#' @examples
#'  nsqipr:::when_dyspnea(c("at rest","moderate exertion", NA))
#'
when_dyspnea <- function(vec) {
  vec %^% list(`At rest` = "AT REST", `Moderate exertion` = "MODERATE EXERTION")
}

#' Parse a column for type of sepsis
#'
#' @param vec a character vector of values to convert
#'
#' @details NSQIP encodes the \code{sepsis} column as either "sirs", "sepsis", "septic shock", or "none.
#' This function factors the vector for the levels "SIRS", "Sepsis", and "Septic shock".
#'
#' \bold{NOTE}: \code{prsepis} is spelled incorrectly (as it is originally spelled in the NSQIP database).
#' It is not spelled \code{prsepsis}.
#'
#' @return a factor vector
#' @keywords internal
#'
#' @examples
#'  nsqipr:::type_prsepis(c("sirs","sepsis", "septic shock", NA))
#'
type_prsepis <- function(vec) {
  vec %^% list(`SIRS` = "SIRS", `Sepsis` = "Sepsis", `Septic shock` = "Septic Shock")
}
