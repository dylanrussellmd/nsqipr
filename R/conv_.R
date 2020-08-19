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

conv_dn_comagraftpn <- function(vec, pufyear) {
  ifelse(assert_before_puf11(pufyear), as.integer(vec), NA)
}

conv_comagraftpn <- function(vec, pufyear) {
  ifelse(assert_before_puf11(pufyear), conv_complication(vec), NA)
}

conv_typeintoc <- function(vec) {
  val <- c("cardiac arrest requiring cpr" = 1L,
           "myocardial infarction" = 2L,
           "unplanned intubation" = 3L)

  unname(val[vec])
}

conv_airtra <- function(vec) {
  val <- c("none" = 1L,
           "lip laceration or hematoma" = 2L,
           "tooth chipped, loosened or lost" = 3L,
           "tongue laceration or hematoma" = 4L,
           "pharyngeal laceration" = 5L,
           "laryngeal laceration" = 6L,
           "failure to intubate" = 7L)
  unname(val[vec])
}

conv_opnote <- function(vec) {
  val <- c("attending" = 1L,
           "resident" = 2L,
           "not available" = NA)

  unname(val[vec])
}

conv_coma <- function(vec, pufyear) {
  ifelse(assert_before_puf11(pufyear), conv_yesno(vec), NA)
}

assert_before_puf11 <- function(pufyear) {
  pufyear <= 5
}

conv_attend <- function(vec) {
  val <- c("attending alone" = 1L,
           "attending in or" = 2L,
           "attending & resident in or" = 2L,
           "attending in or suite" = 3L,
           "attending not present, but available" = 4L,
           "not entered" = NA)
  unname(val[vec])
}

conv_pufyear <- function(caseid) {
  vec <- c(152491, 363898, 635266, 822831, 1520169, 1979085,
           2435678, 3113030, 3873435, 5232202, 6708628, 7842829, 9090216)
  findInterval(caseid, vec) + 1
}

conv_hispanic <- function(vec, race) {
  ifelse(!is.na(race), stringr::str_detect(race, "^hispanic,"), conv_yesno(vec))
}

conv_race <- function(race, race_new, pacific = "asian") {

  pacific <- switch(pacific,
                    "asian" = 2L,
                    "hawaiian" = 4L,
                    "exclude" = NA)

  race_val <- c("hispanic, white" = 5L,
                "hispanic, black" = 2L,
                "hispanic, color unknown" = NA,
                "black, not of hispanic origin" = 2L,
                "white, not of hispanic origin" = 5L,
                "american indian or alaska native" = 1L,
                "asian or pacific islander" = pacific,
                "unknown" = NA)
  race_vec <- unname(race_val[race])

  race_new_val <- c("american indian or alaska native" = 1L,
                    "asian" = 2L,
                    "black or african american" = 3L,
                    "native hawaiian or pacific islander" = 4L,
                    "white" = 5L,
                    "unknown/not reported" = NA)
  race_new_vec <- unname(race_new_val[race_new])

  dplyr::coalesce(race_vec, race_new_vec)
}

conv_numeric <- function(vec) {
  round(as.numeric(vec), digits = 3)
}

conv_wound_closure <- function(vec) {
  val <- c("all layers of incision (deep and superficial) fully closed" = 1L,
           "only deep layers closed; superficial left open" = 2L,
           "no layers of incision are surgically closed" = 3L)
  unname(val[vec])
}

conv_reasons <- function(vec) {
  val <- c("superficial incisional ssi" = 1L,
           "deep incisional ssi" = 2L,
           "organ/space ssi" = 3L,
           "wound disruption" = 4L,
           "pneumonia" = 5L,
           "unplanned intubation" = 6L,
           "pulmonary embolism" = 7L,
           "on ventilator > 48 hours" = 8L,
           "progressive renal insufficiency" = 9L,
           "acute renal failure" = 10L,
           "urinary tract infection" = 11L,
           "cva" = 12L,
           "cardiac arrest requiring cpr" = 13L,
           "myocardial infarction" = 14L,
           "bleeding requiring transfusion (72h of surgery start time)" = 15L,
           "vein thrombosis requiring therapy" = 16L,
           "dvt requiring therapy" = 16L,
           "sepsis" = 17L,
           "septic shock" = 18L,
           "other (list icd 9 code)" =19L,
           "other (list icd 10 code)" = 20L,
           "c. diff" = 21L,
           "graft/prosthesis/flap failure" = 22L,
           "peripheral nerve injury" = 23L)
  unname(val[vec])
}

conv_sex <- function(vec) {
  stringr::str_detect(vec, "^male$")
}

conv_inout <- function(vec) {
  stringr::str_detect(vec, "^inpatient$")
}

conv_complication <- function(vec) {
  !stringr::str_detect(vec, "no complication")
}

conv_age <- function(vec) {
  as.integer(ifelse(stringr::str_detect(vec, "90+"), "90", vec))
}

conv_numscale <- function(vec) {
  as.integer(stringr::str_extract(vec,"^\\d"))
}

conv_transt <- function(vec) {
  val <- c("from acute care hospital inpatient" = 1L,
           "acute care hospital" = 1L,
           "va acute care hospital" = 1L,
           "not transferred (admitted from home)" = 2L,
           "admitted directly from home" = 2L,
           "nursing home - chronic care - intermediate care" = 3L,
           "chronic care facility" = 3L,
           "va chronic care facility" = 3L,
           "outside emergency department" = 4L,
           "transfer from other" = 5L,
           "other" = 5L,
           "unknown" = NA)
  unname(val[vec])
}

conv_dischdest <- function(vec) {
  val <- c("skilled care, not home" = 1L,
           "unskilled facility not home" = 2L,
           "facility which was home" = 3L,
           "home" = 4L,
           "separate acute care" = 5L,
           "rehab" = 6L,
           "expired" = 7L,
           "against medical advice (ama)" = 8L,
           "multi-level senior community" = 9L,
           "hospice" = 10L,
           "unknown" = NA)
  unname(val[vec])
}

conv_anesthes <- function(vec) {
  val <- c("epidural" = 1L,
           "general" = 2L,
           "local" = 3L,
           "mac/iv sedation" = 4L,
           "monitored anesthesia care" = 4L,
           "none" = 5L,
           "other" = 6L,
           "regional" = 7L,
           "spinal" = 8L,
           "unknown" = NA)
  unname(val[vec])
}

conv_surgspec <- function(vec) {
  val <- c("cardiac surgery" = 1L,
           "general surgery" = 2L,
           "gynecology" = 3L,
           "neurosurgery" = 4L,
           "orthopedics" = 5L,
           "otolaryngology (ent)" = 6L,
           "plastics" = 7L,
           "thoracic" = 8L,
           "urology" = 9L,
           "vascular" = 10L,
           "interventional radiologist" = 11L,
           "ophthalmology" = 12L,
           "podiatry" = 13L,
           "oral surgery" = 14L,
           "other" = 15L,
           "unknown" = NA)
  unname(val[vec])
}

conv_notno <- function(vec) {
  vec != "no"
}

insulin <- function(vec) {
  conv_notno(vec) & stringr::str_detect(vec, "^insulin$")
}

conv_yesno <- function(vec) {
  stringr::str_detect(vec, "^yes$")
}

when_dyspnea <- function(vec) {
  val <- c("at rest" = 1L,
           "moderate exertion" = 2L,
           "no" = NA)
  unname(val[vec])
}

conv_fnstatus <- function(vec) {
  val <- c("independent" = 1L,
           "partially dependent" = 2L,
           "totally dependent" = 3L,
           "unknown" = NA)
  unname(val[vec])
}

conv_prsepis <- function(vec) {
  vec != "none"
}

type_prsepis <- function(vec) {
  val <- c("sirs" = 1L,
           "sepsis" = 2L,
           "septic shock" = 3L,
           "none" = NA)
  unname(val[vec])
}
