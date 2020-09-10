# TODO Explore error function e and try to only capture the error for variable not found.
conv_acs_cols <- function(df) {
  df %>%
    dplyr::rename(dplyr::any_of(c("race" = "race_new"))) %>%
    dplyr::mutate(
      pufyear = tryCatch(conv_pufyear(caseid), error = function(e) return(NULL)),
      sex = tryCatch(conv_sex(sex), error = function(e) return(NULL)),
      ethnicity_hispanic = tryCatch(conv_hispanic(.), error = function(e) return(NULL)),
      race = tryCatch(conv_race(race), error = function(e) return(NULL)),
      inout = tryCatch(conv_inout(inout), error = function(e) return(NULL)),
      attend = tryCatch(conv_attend(attend), error = function(e) return(NULL)),
      transt = tryCatch(conv_transt(transt), error = function(e) return(NULL)),
      age = tryCatch(conv_age(age), error = function(e) return(NULL)),
      dischdest = tryCatch(conv_dischdest(dischdest), error = function(e) return(NULL)),
      anesthes = tryCatch(conv_anesthes(anesthes), error = function(e) return(NULL)),
      anesthes_other = tryCatch(conv_anesthes(anesthes_other), error = function(e) return(NULL)),
      surgspec = tryCatch(conv_surgspec(surgspec), error = function(e) return(NULL)),
      insulin = tryCatch(insulin(diabetes), error = function(e) return(NULL)),
      diabetes = tryCatch(conv_notno(diabetes), error = function(e) return(NULL)),
      when_dyspnea = tryCatch(when_dyspnea(dyspnea), error = function(e) return(NULL)),
      dyspnea = tryCatch(conv_notno(dyspnea), error = function(e) return(NULL)),
      fnstatus1 = tryCatch(conv_fnstatus(fnstatus1), error = function(e) return(NULL)),
      fnstatus2 = tryCatch(conv_fnstatus(fnstatus2), error = function(e) return(NULL)),
      type_prsepis = tryCatch(type_prsepis(prsepis), error = function(e) return(NULL)),
      prsepis = tryCatch(conv_prsepis(prsepis), error = function(e) return(NULL)),
      coma = tryCatch(conv_coma(coma, pufyear), error = function(e) return(NULL)),
      wound_closure = tryCatch(conv_wound_closure(wound_closure), error = function(e) return(NULL)),
      pnapatos = tryCatch(dplyr::coalesce(cpneumon, pnapatos), error = function(e) return(NULL)),
      readmission1 = tryCatch(dplyr::coalesce(readmission, readmission1), error = function(e) return(NULL)),
      unplannedreadmission1 = tryCatch(dplyr::coalesce(unplanreadmission, unplannedreadmission1), error = function(e) return(NULL)),
      reoperation1 = tryCatch(dplyr::coalesce(reoperation, reoperation1), error = function(e) return(NULL)),
      opnote = tryCatch(conv_opnote(opnote), error = function(e) return(NULL)),
      airtra = tryCatch(conv_airtra(airtra), error = function(e) return(NULL)),
      ncnscoma = tryCatch(conv_dn_comagraftpn(ncnscoma, pufyear), error = function(e) return(NULL)),
      cnscoma = tryCatch(conv_comagraftpn(cnscoma, pufyear), error = function(e) return(NULL)),
      dcnscoma = tryCatch(conv_dn_comagraftpn(dcnscoma, pufyear), error = function(e) return(NULL)),
      nneurodef = tryCatch(conv_dn_comagraftpn(nneurodef, pufyear), error = function(e) return(NULL)),
      neurodef = tryCatch(conv_comagraftpn(neurodef, pufyear), error = function(e) return(NULL)),
      dneurodef = tryCatch(conv_dn_comagraftpn(dneurodef, pufyear), error = function(e) return(NULL)),
      nothgrafl = tryCatch(conv_dn_comagraftpn(nothgrafl, pufyear), error = function(e) return(NULL)),
      othgrafl = tryCatch(conv_comagraftpn(othgrafl, pufyear), error = function(e) return(NULL)),
      dothgrafl = tryCatch(conv_dn_comagraftpn(dothgrafl, pufyear), error = function(e) return(NULL)),
      typeintoc = tryCatch(conv_typeintoc(typeintoc), error = function(e) return(NULL))
    )
}

# THIS CHANGES EVERYTHING
fnstatus1 <- list(Independent = "independent",
                 `Partially dependent` = "partially dependent",
                 `Totally dependent` = "totally dependent")
fnstatus2 <- fnstatus1
type_prsepsis <- list(SIRS = "sirs",
                      Sepsis = "sepsis",
                      `Septic shock` = "septic shock")
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

#' Converts readmission reasons to a factor
#'
#' @param vec a character vector containing readmission reasons
#' @return a factor vector
#'
#' @details Formats factors accordingly:
#'
#' | \bold{Original}| \bold{Level}  | \bold{Label}  |
#' | -------------  |:-------------:| -----:|
#' | "superficial incisional ssi"       | sssi | Superficial incisional SSI |
#'
#'
#' @md
#' @keywords internal
#' @examples
#'
#' x <- c("2000", "1900", "2020", "1950")
#' conv_date(x)
#'
conv_reasons <- function(vec) {
  c(`Superficial incisional SSI` = "superficial incisional ssi",
    `Deep incisional SSI` = "deep incisional ssi",
    `Organ-space SSI` = "organ/space ssi",
    `Wound disruption` = "wound disruption",
    `Pneumonia` = "pneumonia",
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
    `Vein thrombosis requiring therapy` = "vein thrombosis requiring therapy",
    `Vein thrombosis requiring therapy` = "dvt requiring therapy",
    `Sepsis` = "sepsis",
    `Septic shock` = "septic shock",
    `Other` = "other (list icd 9 code)",
    `Other` = "other (list icd 10 code)",
    `C. difficile` = "c. diff",
    `Graft/prosthesis/flap failure` = "graft/prosthesis/flap failure",
    `Peripheral nerve injury` = "peripheral nerve injury"
  ) %>% fact(vec)
}

conv_dn_comagraftpn <- function(vec, pufyear) {
  ifelse(assert_before_puf11(pufyear), as.integer(vec), NA)
}

conv_comagraftpn <- function(vec, pufyear) {
  ifelse(assert_before_puf11(pufyear), conv_complication(vec), NA)
}

conv_coma <- function(vec, pufyear) {
  ifelse(assert_before_puf11(pufyear), conv_yesno(vec), NA)
}

assert_before_puf11 <- function(pufyear) {
  pufyear <= 5
}

# conv_typeintoc <- function(vec) {
#   c(`Cardiac arrest requiring CPR` = "cardiac arrest requiring cpr",
#     `Myocardial infarction` = "myocardial infarction",
#     `Unplanned intubation` = "unplanned intubation"
#   ) %>% fact(vec)
# }

# conv_airtra <- function(vec) {
#   c(`None` = "none",
#     `Lip laceration or hematoma` = "lip laceration or hematoma",
#     `Tooth chipped, loosened, or lost` = "tooth chipped, loosened or lost",
#     `Tongue laceration or hematoma` = "tongue laceration or hematoma",
#     `Pharyngeal laceration` = "pharyngeal laceration",
#     `Laryngeal laceration` = "laryngeal laceration",
#     `Failure to intubate` = "failure to intubate") %>% fact(vec)
# }

# conv_opnote <- function(vec) {
#   c(`Attending` = "attending",
#     `Resident` = "resident"
#     ) %>% fact(vec)
# }

# conv_attend <- function(vec) {
#   c(`Attending alone` = "attending alone",
#     `Attending and resident in OR` = "attending in or",
#     `Attending and resident in OR` = "attending & resident in or",
#     `Attending in OR suite` = "attending in or suite",
#     `Attending not present, but available` = "attending not present, but available"
#     ) %>% fact(vec)
# }

#TODO confirm that these caseids are accurate for checking pufyear.
conv_pufyear <- function(caseid) {
  vec <- c(152491, 363898, 635266, 822831, 1520169, 1979085,
           2435678, 3113030, 3873435, 5232202, 6708628, 7842829, 9284384)
  findInterval(caseid, vec) + 1
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

# conv_wound_closure <- function(vec) {
#   c(`All layers of incision (deep and superficial) fully closed` = "all layers of incision (deep and superficial) fully closed",
#     `Only deep layers closed; superficial left open` = "only deep layers closed; superficial left open",
#     `No layers of incision are surgically closed` = "no layers of incision are surgically closed"
#     ) %>% fact(vec)
# }

conv_sex <- function(vec) {
  stringr::str_detect(vec, "^male$")
}

conv_inout <- function(vec) {
  stringr::str_detect(vec, "^inpatient$")
}

conv_age <- function(vec) {
  as.integer(ifelse(stringr::str_detect(vec, "90+"), "90", vec))
}

# conv_transt <- function(vec) {
#   c(`Acute care hospital` = "from acute care hospital inpatient",
#     `Acute care hospital` = "acute care hospital",
#     `Acute care hospital` = "va acute care hospital",
#     `Admitted from home` = "not transferred (admitted from home)",
#     `Admitted from home` = "admitted directly from home",
#     `Chronic care facility` = "nursing home - chronic care - intermediate care",
#     `Chronic care facility` = "chronic care facility",
#     `Chronic care facility` = "va chronic care facility",
#     `Outside emergency department` = "outside emergency department",
#     `Other` = "transfer from other",
#     `Other` = "other"
#     ) %>% fact(vec)
#
# }

conv_dischdest <- function(vec) {
  c(`Skilled care, not home` = "skilled care, not home",
    `Unskilled facility, not home` = "unskilled facility not home",
    `Facility which was home` = "facility which was home",
    `Home` = "home",
    `Separate acute care` = "separate acute care",
    `Rehab` = "rehab",
    `Expired` = "expired",
    `Against medical advice (AMA)` = "against medical advice (ama)",
    `Multi-level senior community` = "multi-level senior community",
    `Hospice` = "hospice"
    ) %>% fact(vec)
}

conv_anesthes <- function(vec) {
  c(`Epidural` = "epidural",
    `General` = "general",
    `Local` = "local",
    `Monitored anesthesia care` = "mac/iv sedation",
    `Monitored anesthesia care` = "monitored anesthesia care",
    `None` = "none",
    `Other` = "other",
    `Regional` = "regional",
    `Spinal` = "spinal"
    ) %>% fact(vec)
}

conv_surgspec <- function(vec) {
  c(`Cardiac surgery` = "cardiac surgery",
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
    `Other` = "other"
    ) %>% fact(vec)
}

insulin <- function(vec) {
  conv_notno(vec) & stringr::str_detect(vec, "^insulin$")
}

when_dyspnea <- function(vec) {
  c(`At rest` = "at rest",
    `Moderate exertion` = "moderate exertion"
    ) %>% fact(vec)
}

conv_fnstatus <- function(vec) {
  c(`Independent` = "independent",
    `Partially dependent` = "partially dependent",
    `Totally dependent` = "totally dependent"
    ) %>% fact(vec)
}

conv_prsepis <- function(vec) {
  vec != "none"
}

type_prsepis <- function(vec) {
  c(`SIRS` = "sirs",
    `Sepsis` = "sepsis",
    `Septic shock` = "septic shock"
    ) %>% fact(vec)
}
