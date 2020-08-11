#' @export
conv_to_standard <- function(file, write_to_csv = FALSE) {
  readr::read_tsv(file, col_types = readr::cols(.default = "c")) %>%
    set_up_df() %>%
    conv_type_cols() %>%
    conv_special_cols() %>%
    dplyr::select(., !dplyr::any_of(redundant_cols)) %>%
    ifelse(write_to_csv,
           readr::write_csv(., path = paste("./", file, "_clean.csv", sep = ""), na = "", col_names = FALSE),
           return(.))
}

#' @export
conv_dir_to_standard <- function(dir) {
  list.files(path = dir, pattern = "*.txt$", full.names = TRUE, recursive = FALSE) %>%
    lapply(., conv_to_standard, write_to_csv = TRUE)
}

########################################################

set_up_df <- function(df) {
  df %>%
    dplyr::rename_with(., tolower) %>%
    dplyr::mutate(dplyr::across(everything(), tolower)) %>%
    dplyr::mutate(dplyr::across(everything(), dplyr::na_if, "unknown")) %>%
    dplyr::mutate(dplyr::across(everything(), dplyr::na_if, "null")) %>%
    dplyr::mutate(dplyr::across(everything(), dplyr::na_if, "-99")) %>%
    dplyr::mutate(dplyr::across(everything(), dplyr::na_if, -99)) %>%
    tibble::add_column(., !!!col_names[setdiff(names(col_names), names(.))])
}

conv_type_cols <- function(df) {
  df %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(date_cols), ~ lubridate::ymd(.x, truncated = 2))) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(complication_cols), conv_complication)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(numscale_cols), conv_numscale)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(yes_no_cols), conv_yesno)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(integer_cols), as.integer)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(numeric_cols), conv_numeric)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(reason_cols), conv_reasons))
}

conv_special_cols <- function(df) {
  df %>%
    dplyr::mutate(
      pufyear = conv_pufyear(caseid),
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
      dothgrafl = conv_dn_comagraftpn(dothgrafl, pufyear),
      typeintoc = conv_typeintoc(typeintoc)
    )
}
