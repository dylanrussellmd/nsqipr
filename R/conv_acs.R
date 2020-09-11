# TODO Explore error function e and try to only capture the error for variable not found.
# conv_acs_cols <- function(df) {
#   df %>%
#     dplyr::rename(dplyr::any_of(c("race" = "race_new"))) %>%
#     dplyr::mutate(
#      *  ethnicity_hispanic = tryCatch(conv_hispanic(.), error = function(e) return(NULL)),
#       * race = tryCatch(conv_race(race), error = function(e) return(NULL)),
#       wound_closure = tryCatch(conv_wound_closure(wound_closure), error = function(e) return(NULL)),
#       *pnapatos = tryCatch(dplyr::coalesce(cpneumon, pnapatos), error = function(e) return(NULL)),
#       *readmission1 = tryCatch(dplyr::coalesce(readmission, readmission1), error = function(e) return(NULL)),
#       *unplannedreadmission1 = tryCatch(dplyr::coalesce(unplanreadmission, unplannedreadmission1), error = function(e) return(NULL)),
#       *reoperation1 = tryCatch(dplyr::coalesce(reoperation, reoperation1), error = function(e) return(NULL)),
#     )
# }

conv_acs_cols <- function(df, filename) {
  data.table::setnames(df, "race_new","race")
  get_pufyear(df, filename)
  conv_(df, "sex", conv_sex)
  conv_(df, "inout", conv_inout)
  conv_(df, "diabetes", insulin, newcol = "insulin")
  conv_(df, "diabetes", conv_notno)
  conv_(df, "dyspnea", when_dyspnea, newcol = "when_dyspnea")
  conv_(df, "dyspnea", conv_notno)
  conv_(df, "prsepis", type_prsepis, newcol = "type_prsepis")
  conv_(df, "prsepis", conv_notno)
}


fnstatus1 <- list(Independent = "independent",
                 `Partially dependent` = "partially dependent",
                 `Totally dependent` = "totally dependent")
fnstatus2 <- fnstatus1
typeintoc <- list(`Cardiac arrest requiring CPR` = "cardiac arrest requiring cpr",
                  `Myocardial infarction` = "myocardial infarction",
                  `Unplanned intubation` = "unplanned intubation")
airtra <- list(None = "none",
               `Lip laceration or hematoma` = "lip laceration or hematoma",
               `Tooth chipped, loosened, or lost` = "tooth chipped, loosened or lost",
               `Tongue laceration or hematoma` = "tongue laceration or hematoma",
               `Pharyngeal laceration` = "pharyngeal laceration",
               `Laryngeal laceration` = "laryngeal laceration",
               `Failure to intubate` = "failure to intubate")
opnote <- list(Attending = "attending",
               Resident = "resident")
attend <- list(`Attending alone` = "attending alone",
               `Attending and resident in OR` = c("attending in or","attending & resident in or"),
               `Attending in OR suite` = "attending in or suite",
               `Attending not present, but available` = "attending not present, but available")
wound_closure <- list(`All layers of incision (deep and superficial) fully closed` = "all layers of incision (deep and superficial) fully closed",
                      `Only deep layers closed; superficial left open` = "only deep layers closed; superficial left open",
                      `No layers of incision are surgically closed` = "no layers of incision are surgically closed")
transt <- list(`Acute care hospital` = c("from acute care hospital inpatient","acute care hospital","va acute care hospital"),
               `Admitted from home` = c("not transferred (admitted from home)","admitted directly from home"),
               `Chronic care facility` = c("nursing home - chronic care - intermediate care","chronic care facility","va chronic care facility"),
               `Outside emergency department` = "outside emergency department",
               Other = c("transfer from other","other"))
readmsuspreason1 <- list(`Superficial incisional SSI` = "superficial incisional ssi",
                         `Deep incisional SSI` = "deep incisional ssi",
                         `Organ-space SSI` = "organ/space ssi",
                         `Wound disruption` = "wound disruption",
                         Pneumonia = "pneumonia",
                         `Unplanned intubation` = "unplanned intubation",
                         `Pulmonary embolism` = "pulmonary embolism",
                         `On ventilator > 48 hours` = "on ventilator > 48 hours",
                         `Progressive renal insufficiency` = "progressive renal insufficiency",
                         `Acute renal failure` = "acute renal failure",
                         `Urinary tract infection` = "urinary tract infection",
                         `Cerebrovascular accident` = "cva",
                         `Cardiac arrest requiring CPR` = "cardiac arrest requiring cpr",
                         `Myocardial infarction` = "myocardial infarction",
                         `Bleeding requiring transfusion (within 72 hours of surgery start time)` = "bleeding requiring transfusion (72h of surgery start time)",
                         `Vein thrombosis requiring therapy` = c("vein thrombosis requiring therapy","dvt requiring therapy"),
                         Sepsis = "sepsis",
                         `Septic shock` = "septic shock",
                         Other = c("other (list icd 9 code)","other (list icd 10 code)"),
                         `C. difficile` = "c. diff",
                         `Graft/prosthesis/flap failure` = "graft/prosthesis/flap failure",
                         `Peripheral nerve injury` = "peripheral nerve injury")
readmunrelsusp1 <- readmsuspreason1
readmsuspreason2 <- readmsuspreason1
readmunrelsusp2 <- readmsuspreason1
readmsuspreason3 <- readmsuspreason1
readmunrelsusp3 <- readmsuspreason1
readmsuspreason4 <- readmsuspreason1
readmunrelsusp4 <- readmsuspreason1
readmsuspreason5 <- readmsuspreason1
readmunrelsusp5 <- readmsuspreason1
dischdest <- list(`Skilled care, not home` = "skilled care, not home",
                  `Unskilled facility, not home` = "unskilled facility not home",
                  `Facility which was home` = "facility which was home",
                  Home = "home",
                  `Separate acute care` = "separate acute care",
                  Rehab = "rehab",
                  Expired = "expired",
                  `Against medical advice (AMA)` = "against medical advice (ama)",
                  `Multi-level senior community` = "multi-level senior community",
                  Hospice = "hospice")
anesthes <- list(`Epidural` = "epidural",
                 `General` = "general",
                 `Local` = "local",
                 `Monitored anesthesia care` = c("mac/iv sedation","monitored anesthesia care"),
                 `None` = "none",
                 `Other` = "other",
                 `Regional` = "regional",
                 `Spinal` = "spinal")
anesthes_other <- anesthes
surgspec <- list(`Cardiac surgery` = "cardiac surgery",
                 `General surgery` = "general surgery",
                 `Gynecology` = "gynecology",
                 `Neurosurgery` = "neurosurgery",
                 `Orthopedics` = "orthopedics",
                 `Otolaryngology (ENT)` = "otolaryngology (ent)",
                 `Plastics` = "plastics",
                 `Thoracic` = "thoracic",
                 `Urology` = "urology",
                 `Vascular` = "vascular",
                 `Interventional radiologist` = "interventional radiologist",
                 `Ophthalmology` = "ophthalmology",
                 `Podiatry` = "podiatry",
                 `Oral surgery` = "oral surgery",
                 `Other` = "other")

#' Remove coma, neuro deficit, and graft columns after 2010
#'
#' @param df a data.table from which to remove the coma, neuro deficit, and graft columns
#'
#' @examples
#' x <- data.table::data.table(coma = c(TRUE, TRUE, FALSE), cnscoma = c(TRUE, TRUE, FALSE), ncnscoma = c(1,2,3), dcnscoma = c(1,2,3),
#'                             neurodef = c(TRUE, TRUE, FALSE), nneurodef = c(1,2,3), dneurodef = c(1,2,3),
#'                             othgrafl = c(TRUE, TRUE, FALSE), nothgrafl = c(1,2,3), dothgrafl = c(1,2,3),
#'                             distraction = c("Test","test","test"))
#' conv_pufyear(x, "acs_nsqip_puf12.txt")
#' check_comagraftpn(x)
#'
#' x <- data.table::data.table(coma = c(TRUE, TRUE, FALSE), cnscoma = c(TRUE, TRUE, FALSE), ncnscoma = c(1,2,3), dcnscoma = c(1,2,3),
#'                             neurodef = c(TRUE, TRUE, FALSE), nneurodef = c(1,2,3), dneurodef = c(1,2,3),
#'                             othgrafl = c(TRUE, TRUE, FALSE), nothgrafl = c(1,2,3), dothgrafl = c(1,2,3),
#'                             distraction = c("Test","test","test"))
#' conv_pufyear(x, "acs_nsqip_puf10.txt")
#' check_comagraftpn(x)
#'
check_comaneurograft <- function(df) {
  if(unique(df[["pufyear"]]) > 2010) {
    cols <- c("coma","cnscoma","ncnscoma","dcnscoma","neurodef","nneurodef","dneurodef","othgrafl","nothgrafl","dothgrafl")
    for(j in intersect(cols, names(df))) data.table::set(df, j = j, value = NA)
  }
}

conv_hispanic <- function(df) {
  if("ethnicity_hispanic" %in% names(df)) {
    conv_hispanic_helper(df[["race"]], df[["ethnicity_hispanic"]])
  } else {
    stringr::str_detect(df[["race"]], "^hispanic,")
  }
}

conv_hispanic_helper <- function(race, ethnicity_hispanic) {
  ifelse((race %in% c("white","black or african american") | is.na(race)), # only PUFs after RACE_NEW was introduced should have these possible races.
         conv_yesno(ethnicity_hispanic),
         ifelse(race %in% c("american indian or alaska native","asian","native hawaiian or pacific islander","asian or pacific islander"),
                FALSE,
                stringr::str_detect(race, "^hispanic,")))
}

conv_race <- function(vec, pacific = "asian") {
  pacific <- switch(pacific,
                    "asian" = "Asian",
                    "hawaiian" = "Native Hawaiian or Pacific islander")

  orig <- c("hispanic, white",
            "white, not of hispanic origin",
            "white",
            "hispanic, black",
            "black, not of hispanic origin",
            "black or african american",
            "american indian or alaska native",
            "asian",
            "native hawaiian or pacific islander",
            "asian or pacific islander")
  names <- c("White", "White", "White", "Black", "Black", "Black", "American Indian or Alaska native", "Asian", "Native Hawaiian or Pacific islander", pacific)

  setNames(orig, names) %>% fact(vec)
}

#' Convert sex to logical
#'
#' @param vec a character vector of values to convert
#'
#' @details if "male", will result in TRUE. If given NA, will return NA.
#'
#' @return a logical vector
#' @keywords internal
#'
#' @examples
#  conv_sex(c("male","MALE","female","FEMALE",NA))
#'
conv_sex <- function(vec) {
  stringi::stri_detect_regex(vec, "^male", opts_regex = list(case_insensitive = TRUE))
}

#' Convert inout to logical
#'
#' @param vec a character vector of values to convert
#'
#' @details If "inpatient", will result in true. If given NA, will return NA.
#'
#' @return an integer vector
#' @keywords internal
#'
#' @examples
#  conv_inout(c("inpatient", "outpatient", NA))
#'
conv_inout <- function(vec) {
  stringi::stri_detect_fixed(vec, "inpatient", opts_fixed = list(case_insensitive = TRUE))
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
#  conv_age(c("18","45","90+",NA))
#'
conv_age <- function(vec) {
  as.integer(ifelse(stringi::stri_detect_fixed(vec, "90+", opts_fixed = list(case_insensitive = TRUE)), "90", vec))
}

#' Parse a column for insulin usage
#'
#' @param vec a character vector of values to convert
#'
#' @details NSQIP encodes the \code{diabetes} column as either "no", "non-insulin", or "insulin".
#' This function checks that the value is both \bold{not} "no" and also equal to "insulin". Returns
#' FALSE if either "no" or "non-insulin". If given NA, will return NA.
#'
#' @return a logical vector
#' @keywords internal
#'
#' @examples
#  insulin(c("no","non-insulin","insulin",NA))
#'
insulin <- function(vec) {
  conv_notno(vec) & stringi::stri_detect_regex(vec, "^insulin$", opts_regex = list(case_insensitive = TRUE))
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
#  when_dyspnea(c("at rest","moderate exertion", NA))
#'
when_dyspnea <- function(vec) {
  vec %^% list(`At rest` = "at rest", `Moderate exertion` = "moderate exertion")
}

#' Parse a column for type of sepsis
#'
#' @param vec a character vector of values to convert
#'
#' @details NSQIP encodes the \code{sepsis} column as either "sirs", "sepsis", "septic shock", or "none.
#' This function factors the vector for the levels "SIRS", "Sepsis", and "Septic shock".
#'
#' \bold{NOTE}: \code{prsepis} is spelled illogically (as it is originally spelled in the NSQIP database).
#' It is not spelled \code{prsepsis}.
#'
#' @return a factor vector
#' @keywords internal
#'
#' @examples
#  type_prsepis(c("sirs","sepsis", "septic shock", NA))
#'
type_prsepis <- function(vec) {
  vec %^% list(`SIRS` = "sirs", `Sepsis` = "sepsis", `Septic shock` = "septic shock")
}
