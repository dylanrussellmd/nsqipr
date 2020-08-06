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
    unique(unlist(lapply(., get_headers))) %>%
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

conv_to_standard <- function(file) {
  readr::read_tsv(file, n_max = 500) %>%
    dplyr::rename_with(., tolower) %>%
    dplyr::mutate(dplyr::across(everything(), tolower)) %>%
    dplyr::mutate(dplyr::across(everything(), dplyr::na_if, "unknown")) %>%
    dplyr::mutate(dplyr::across(everything(), dplyr::na_if, "null")) %>%
    dplyr::mutate(dplyr::across(everything(), dplyr::na_if, "-99")) %>%
    dplyr::mutate(dplyr::across(everything(), dplyr::na_if, -99)) %>%
    tibble::add_column(., !!!col_names[setdiff(names(col_names), names(.))]) %>%
    dplyr::mutate(dplyr::across(c("pufyear","admyr","operyr"), ~ lubridate::ymd(.x, truncated = 2))) %>%
    dplyr::mutate(
      sex = stringr::str_detect(sex, "^male$"),
      race_new = conv_race_new(race_new),
      ethnicity_hispanic = conv_yesno(ethnicity_hispanic),
      workrvu = as.numeric(workrvu),
      inout = stringr::str_detect(inout, "^inpatient$"),
      transt = conv_transt(transt),
      age = as.integer(ifelse(stringr::str_detect(age, "90+"), "90", age)),
      dischdest = conv_dischdest(dischdest),
      anesthes = conv_anesthes(anesthes),
      surgspec = conv_surgspec(surgspec),
      electsurg = conv_yesno(electsurg),
      height = as.integer(height),
      weight = as.integer(weight),
      insulin = insulin(diabetes),
      diabetes = diabetes(diabetes),
      smoke = conv_yesno(smoke),
      when_dyspnea = when_dyspnea(dyspnea),
      dyspnea = dyspnea(dyspnea),
      fnstatus2 = conv_fnstatus2(fnstatus2),
      ventilat = conv_yesno(ventilat),
      hxcopd = conv_yesno(hxcopd),
      ascites = conv_yesno(ascites),
      hxchf = conv_yesno(hxchf),
      hypermed = conv_yesno(hypermed),
      renafail = conv_yesno(renafail),
      dialysis = conv_yesno(dialysis),
      discancr = conv_yesno(discancr),
      wndinf = conv_yesno(wndinf),
      steroid = conv_yesno(steroid),
      wtloss = conv_yesno(wtloss),
      bleeddis = conv_yesno(bleeddis),
      transfus = conv_yesno(transfus),
      type_prsepis = type_prsepis(prsepis),
      prsepis = prsepis(prsepis)
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("dpr"), as.integer)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c("prsodm","prbun","prcreat","pralbum","prbili","prsgot","pralkph","prwbc","prhct","prplate","prptt","prinr","prpt")), as.numeric))
}

conv_race_new <- function(vec) {
  val <- c("american indian or alaska native" = 1L,
           "asian" = 2L,
           "black or african american" = 3L,
           "native hawaiian or pacific islander" = 4L,
           "white" = 5L,
           "unknown/not reported" = NA)
  unname(val[vec])
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

diabetes <- function(vec) {
  vec != "no"
}

insulin <- function(vec) {
  diabetes(vec) & stringr::str_detect(vec, "^insulin$")
}

conv_yesno <- function(vec) {
  stringr::str_detect(vec, "^yes$")
}

dyspnea <- function(vec) {
  vec != "no"
}

when_dyspnea <- function(vec) {
  val <- c("at rest" = 1L,
           "moderate exertion" = 2L,
           "no" = NA)
  unname(val[vec])
}

conv_fnstatus2 <- function(vec) {
  val <- c("independent" = 1L,
           "partially dependent" = 2L,
           "totally dependent" = 3L,
           "unknown" = NA)
  unname(val[vec])
}

prsepis <- function(vec) {
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
