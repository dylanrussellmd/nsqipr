fact <- function(levels, vec) {
  factor(stringr::str_squish(vec), levels) %>% forcats::fct_recode(!!!levels)
}

conv_yesno <- function(vec) {
  stringr::str_detect(vec, "^yes$")
}

conv_notno <- function(vec) {
  vec != "no"
}

conv_complication <- function(vec) {
  !stringr::str_detect(vec, "no complication")
}

conv_numscale <- function(vec) {
  as.integer(stringr::str_extract(vec,"^\\d"))
}

conv_numeric <- function(vec) {
  round(as.numeric(vec), digits = 3)
}

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

setlowernames <- function(df) {
  data.table::setnames(df, stringi::stri_trans_tolower(names(df)))
}

setlower <- function(df) {
  for(j in seq_along(df)){
    data.table::set(df, j=j, value=stringi::stri_trans_tolower(df[[j]]))
  }
}

setna <- function(df, val) {
  for(j in seq_along(df)){
    data.table::set(df, i=which(df[[j]] %in% val), j=j, value=NA)
  }
}

