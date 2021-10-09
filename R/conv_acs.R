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
  make_readm_cols(df)
  make_reop_cols(df)
  make_anesthes_other_cols(df)
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
                         Other = c("Other (list ICD 9 code)","Other (list icd 10 code)"),
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
anesthes_other1 <- anesthes
anesthes_other2 <- anesthes
anesthes_other3 <- anesthes
anesthes_other4 <- anesthes
anesthes_other5 <- anesthes
anesthes_other6 <- anesthes
anesthes_other7 <- anesthes
anesthes_other8 <- anesthes
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
readm_cols <- c(readmission, readmpodays, readmrelated, readmsuspreason, readmrelicd9, readmrelicd10, unplannedreadmission, readmunrelsusp, readmunrelicd9, readmunrelicd10)

reoperations <- paste("reoperation", 1:3, sep = "")
retorpodays <- c("retorpodays","retor2podays", "retor3podays")
reoporcpt1 <- c("reoporcpt1","reopor2cpt1","reopor3cpt1")
retorrelated <- c("retorrelated","retor2related","retor3related")
reoporicd91 <- c("reoporicd91","reopor2icd91","reopor3icd91")
reoporicd10 <- c("reopor1icd101", "reopor2icd101", "reopor3icd101")
reop_cols <- c(reoperations, retorpodays, reoporcpt1, retorrelated, reoporicd91, reoporicd10)

anesthes_other_cols = paste("anesthes_other", 1:8, sep = "")

proc <- c("prncptx", paste("otherproc", 1:10, sep = ""), paste("concurr", 1:10, sep = ""))
cpt <- c("cpt", paste("othercpt", 1:10, sep = ""), paste("concpt", 1:10, sep = ""))
wrvu <- c("workrvu", paste("otherwrvu", 1:10, sep = ""), paste("conwrvu", 1:10, sep = ""))
cpt_cols <- c(proc, cpt, wrvu)

#### ---- FUNCTIONS ---- ####

#' Create readmission columns for long conversion
#'
#' @param df a data table to add the columns to
#'
#' @details First checks if the data table contains any of the readmission columns.
#' If so, the rest are created as needed. New columns are set to a value of NA.
#'
#' @keywords internal
#'
#' @examples
#' x <- data.table::data.table(readmission1 = TRUE)
#' nsqipr:::make_readm_cols(x)
#' x
#'
make_readm_cols <- function(df) {
  if(length(intersect(readm_cols, names(df))) > 0) {
    for(j in setdiff(readm_cols, names(df))) data.table::set(df, j = j, value = NA)
  }
}

#' Create reoperation columns for long conversion
#'
#' @param df a data table to add the columns to
#'
#' @details First checks if the data table contains any of the reoperation columns.
#' If so, the rest are created as needed. New columns are set to a value of NA.
#'
#' @keywords internal
#'
#' @examples
#' x <- data.table::data.table(reoperation1 = TRUE)
#' nsqipr:::make_reop_cols(x)
#' x
#'
make_reop_cols <- function(df) {
  if(length(intersect(reop_cols, names(df))) > 0) {
    for(j in setdiff(reop_cols, names(df))) data.table::set(df, j = j, value = NA)
  }
}

#' Create reoperation columns for long conversion
#'
#' @param df a data table to add the columns to
#'
#' @details First checks if the data table contains an "anesthes_other" column.
#' If so, the column is split into 8 columns according to the regex pattern ",\\s?".
#'
#' @keywords internal
#'
#' @examples
#' x <- data.table::data.table(
#' anesthes_other = c("General","General, Spinal", "General, Spinal, MAC/IV Sedation", NA)
#' )
#' nsqipr:::make_anesthes_other_cols(x)
#' x
#'
make_anesthes_other_cols <- function(df) {
  if("anesthes_other" %qsin% names(df)) {
    mat <- stringi::stri_split_regex(df[["anesthes_other"]], ",\\s?", simplify = NA, n = 8, omit_empty = TRUE, opts_regex = list(case_insensitive = TRUE))
    for(j in seq_along(anesthes_other_cols)) data.table::set(df, j = anesthes_other_cols[[j]], value = mat[, j])
  }
}

#' Remove coma, neuro deficit, and graft columns after 2010
#'
#' @param df a data.table from which to remove the coma, neuro deficit, and graft columns
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

#' Add or update hispanic ethnicity column
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
