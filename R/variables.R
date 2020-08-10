new_variable <- function(var = character()) {
  stopifnot(rlang::is_character(var))
  structure(var, class = "nsqiprVariable")
}

validate_variable <- function(x) {
  value <- unclass(x)
  if(rlang::is_null(match.arg(value, c("test1","test2")))) {
    stop(
      "Variable name is not valid.",
      call. = FALSE
    )
  }
}




collect_columns <- function(dir) {
  list.files(path = dir, pattern = "*.txt$", full.names = TRUE, recursive = FALSE) %>%
    lapply(., get_headers) %>%
    unlist() %>%
    unique() %>%
    magrittr::set_names(., .) %>%
    replace(., c(1:length(.)), NA)
}

collect_tablenames <- function(dir) {
  tablenames <- unique(unlist(lapply(files, parse_filename)))
}

get_five <- function(file) {
  column <- file %>% readr::read_lines(n_max = 1) %>%
    stringr::str_split(pattern = stringr::boundary("word")) %>%
    unlist() %>%
    stringr::str_to_lower()
  file %>% readr::read_lines(n_max = 1, skip = 1) %>%
    stringr::str_split(pattern = "\t") %>%
    unlist() %>%
    stringr::str_to_lower() %>% magrittr::set_names(., column)
}

yes_no_cols <- c("electsurg","smoke","ventilat","hxcopd","ascites","hxchf","hypermed",
                 "renafail","dialysis","discancr","wndinf","steroid","wtloss","bleeddis","transfus","emergncy",
                 "sssipatos","dssipatos","ossipatos","pnapatos","ventpatos","utipatos","sepsispatos",
                 "sepshockpatos","returnor","stillinhosp","reoperation1","retorrelated","reoperation2","retorrelated2","reoperation3",
                 "readmission1","unplannedreadmission1","readmrelated1","readmission2","unplannedreadmission2","readmrelated2",
                 "readmission3","unplannedreadmission3","readmrelated3", "readmission4","unplannedreadmission4","readmrelated4",
                 "readmission5","unplannedreadmission5","readmrelated5", "etoh", "dnr", "cpneumon", "esovar","hxmi","prvpci","prvpcs",
                 "hxangina","hxpvd","restpain","impsens", "hemi", "hxtia","cva", "cvano", "tumorcns","para","quad","chemo","radio","pregnancy",
                 "proper30", "readmission","unplanreadmission","reoperation")

numscale_cols <- c("wndclas","asaclas")

complication_cols <- c("supinfec","wndinfd","orgspcssi","dehis","oupneumo","reintub","pulembol","failwean","renainsf","oprenafl","urninfec",
                       "cnscva","cdarrest","cdmi","othbleed","othdvt","othsysep","othseshock","othcdiff")

date_cols <- c("admyr","operyr","yrdeath","hdisdt", "admsyr","sdisdt")

integer_cols <- c("height","weight","optime","tothlos", "admqtr","htooday", "dsupinfec","dwndinfd","dorgspcssi", "doupneumo","dreintub",
                  "dpulembol","dfailwean","drenainsf","doprenafl","durninfec","dcnscva","dcdarrest","dcdmi","dothbleed","dothdvt",
                  "dothsysep","dothseshock","dopertod","doptodis","dothcdiff", "retorpodays", "retor2podays", "readmpodays1", "readmpodays2",
                  "readmpodays3", "readmpodays4", "readmpodays5", "dprna", "dprbun", "dprcreat", "dpralbum", "dprbili", "dprsgot", "dpralkph",
                  "dprwbc", "dprhct", "dprplate", "dprptt", "dprpt", "dprinr", "nsupinfec", "nwndinfd", "norgspcssi", "ndehis", "noupneumo",
                  "nreintub", "npulembol", "nfailwean", "nrenainsf", "noprenafl", "nurninfec", "ncnscva", "ncdarrest", "ncdmi", "nothbleed",
                  "nothdvt", "nothsysep", "nothseshock", "nothcdiff", "packs", "pgy", "mallamp", "rbc", "anesurg","surgane","dpatrm","anetime",
                  "stooday","totslos", "dsdtohd")

numeric_cols <- c("prsodm","prbun","prcreat","pralbum","prbili","prsgot","pralkph","prwbc","prhct","prplate","prptt","prinr","prpt",
                  "mortprob","morbprob", "workrvu", "otherwrvu1", "otherwrvu2", "otherwrvu3", "otherwrvu4", "otherwrvu5", "otherwrvu6",
                  "otherwrvu7", "otherwrvu8", "otherwrvu9", "otherwrvu10", "conwrvu1", "conwrvu2")

reason_cols <- c("readmsuspreason1", "readmunrelsusp1", "readmsuspreason2", "readmunrelsusp2", "readmsuspreason3", "readmunrelsusp3",
                 "readmsuspreason4", "readmunrelsusp4", "readmsuspreason5")

redundant_cols <- c('race_new','readmission','unplanreadmission','reoperation')

set_up_df <- function(file) {
  readr::read_tsv(file) %>%
    dplyr::rename_with(., tolower) %>%
    dplyr::mutate(dplyr::across(everything(), tolower)) %>%
    dplyr::mutate(dplyr::across(everything(), dplyr::na_if, "unknown")) %>%
    dplyr::mutate(dplyr::across(everything(), dplyr::na_if, "null")) %>%
    dplyr::mutate(dplyr::across(everything(), dplyr::na_if, "-99")) %>%
    dplyr::mutate(dplyr::across(everything(), dplyr::na_if, -99)) %>%
    tibble::add_column(., !!!col_names[setdiff(names(col_names), names(.))])
}

conv_to_standard <- function(file) {
  set_up_df(file) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(date_cols), ~ lubridate::ymd(.x, truncated = 2))) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(complication_cols), conv_complication)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(numscale_cols), conv_numscale)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(yes_no_cols), conv_yesno)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(integer_cols), as.integer)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(numeric_cols), conv_numeric)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(reason_cols), conv_reasons)) %>%
    dplyr::mutate(
      pufyear = conv_pufyear(file),
      sex = conv_sex(sex),
      ethnicity_hispanic = conv_hispanic(ethnicity_hispanic, race),
      race = conv_race(race, race_new),
      inout = conv_inout(inout),
      attend = conv_attend(attend),
      transt = conv_transt(transt),
      age = conv_age(age),
      dischdest = conv_dischdest(dischdest),
      anesthes = conv_anesthes(anesthes),
      anesthes_other = conv_anesthes(anesthes_other),
      surgspec = conv_surgspec(surgspec),
      insulin = insulin(diabetes),
      diabetes = conv_notno(diabetes),
      when_dyspnea = when_dyspnea(dyspnea),
      dyspnea = conv_notno(dyspnea),
      fnstatus1 = conv_fnstatus(fnstatus1),
      fnstatus2 = conv_fnstatus(fnstatus2),
      type_prsepis = type_prsepis(prsepis),
      prsepis = conv_prsepis(prsepis),
      coma = conv_coma(coma, pufyear),
      wound_closure = conv_wound_closure(wound_closure),
      pnapatos = dplyr::coalesce(cpneumon, pnapatos),
      readmission1 = dplyr::coalesce(readmission, readmission1),
      unplannedreadmission1 = dplyr::coalesce(unplanreadmission, unplannedreadmission1),
      reoperation1 = dplyr::coalesce(reoperation, reoperation1),
      opnote = conv_opnote(opnote),
      airtra = conv_airtra(airtra),
      ncnscoma = conv_dn_comagraftpn(ncnscoma, pufyear),
      cnscoma = conv_comagraftpn(cnscoma, pufyear),
      dcnscoma = conv_dn_comagraftpn(dcnscoma, pufyear),
      nneurodef = conv_dn_comagraftpn(nneurodef, pufyear),
      neurodef = conv_comagraftpn(neurodef, pufyear),
      dneurodef = conv_dn_comagraftpn(dneurodef, pufyear),
      nothgrafl = conv_dn_comagraftpn(nothgrafl, pufyear),
      othgrafl = conv_comagraftpn(othgrafl, pufyear),
      dothgrafl = conv_dn_comagraftpn(dothgrafl, pufyear)
    ) %>%
    dplyr::select(., !dplyr::any_of(redundant_cols))
}

###################  Graft failure, Coma, and Peripheral Nerve Injury should not be considered accurate for any PUF after 2010

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
  pufyear %in% c('05_06','07','08','09','10')
}

conv_attend <- function(vec) {
  val <- c("attending alone" = 1L,
           "attending in or" = 2L,
           "attending in or suite" = 3L,
           "attending not present, but available" = 4L,
           "not entered" = NA)
  unname(val[vec])
}

conv_pufyear <- function(file) {
  stringr::str_extract(file, stringr::regex("(?<=puf_?)(\\d{1,2}_\\d{1,2}|\\d{1,2})(?=.*\\.txt)", ignore_case = TRUE))
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
           "sepsis" = 17L,
           "septic shock" = 18L,
           "other (list icd 9 code)" =19L,
           "other (list icd 10 code)" = 20L,
           "c. diff" = 21L)
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
           "not transferred (admitted from home)" = 2L,
           "nursing home - chronic care - intermediate care" = 3L,
           "outside emergency department" = 4L,
           "transfer from other" = 5L,
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
           "interventional radiologist" = 11L)
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

df <- conv_to_standard("../nsqipr-txt/nsqip-puf/acs_nsqip_puf18.txt")
readr::write_csv(df, path = "../nsqipr-txt/puf18.csv", na = "", col_names = FALSE)

conv_dir_to_standard <- function(dir) {
  list.files(path = dir, pattern = "*.txt$", full.names = TRUE, recursive = FALSE) %>%
    lapply(., conv_and_write)
}

conv_and_write <- function(file) {
  conv_to_standard(file) %>% readr::write_csv(df, path = paste("../nsqipr-txt/", conv_pufyear(file), ".csv", sep = ""), na = "", col_names = FALSE)
}

get_bq_def <- function(col) {
  switch(class(col),
         "Date" = "DATE",
         "character" = "STRING",
         "logical" = "BOOLEAN",
         "integer" = "INTEGER",
         "numeric" = "NUMERIC"
         )
}

print_bq_schema <- function(df) {
  bq <- unname(purrr::imap_chr(purrr::map_chr(df, get_bq_def), ~paste0(.y,":",.x)))
  stringr::str_c(bq, collapse = ",")
}



