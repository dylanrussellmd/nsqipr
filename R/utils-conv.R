conv_to_standard <- function(file, set, write_to_csv, append, return_df, col_names) {

  df <- readr::read_tsv(file, col_types = readr::cols(.default = "c")) %>%
    set_up_df(col_names) %>%
    conv_type_cols() %>%
    conv_special_cols() %>%
    dplyr::select(!dplyr::any_of(redundant_cols)) %>%
    reorder_cols()

  if(write_to_csv & !append) readr::write_csv(df, path = paste(tools::file_path_sans_ext(file), "_clean.csv", sep = ""), na = "", col_names = FALSE)
  if(write_to_csv & append) readr::write_csv(df, path = paste(dirname(file),"/full_clean.csv", sep = ""), na = "", col_names = FALSE, append = TRUE)

  if(return_df) return(df)
}

set_up_df <- function(df, col_names) {
  df %>%
    dplyr::rename_with(., tolower) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), tolower)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), dplyr::na_if, "unknown")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), dplyr::na_if, "null")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), dplyr::na_if, "n/a")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), dplyr::na_if, "-99")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), dplyr::na_if, -99)) %>%
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

# TODO Explore error function e and try to only capture the error for variable not found.
# TODO Split these into separate functions by file type and just parse on file name.
conv_special_cols <- function(df) {
  df %>%
    dplyr::mutate(
      pufyear = tryCatch(conv_pufyear(caseid), error = function(e) return(NULL)),
      sex = tryCatch(conv_sex(sex), error = function(e) return(NULL)),
      ethnicity_hispanic = tryCatch(conv_hispanic(ethnicity_hispanic, race), error = function(e) return(NULL)),
      race = tryCatch(conv_race(race, race_new), error = function(e) return(NULL)),
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
      typeintoc = tryCatch(conv_typeintoc(typeintoc), error = function(e) return(NULL)),
      col_steroid_unk = tryCatch(conv_1_to_true(col_steroid_unk), error = function(e) return(NULL)),
      col_oral_antibiotic_unk = tryCatch(conv_1_to_true(col_oral_antibiotic_unk), error = function(e) return(NULL)),
      col_chemo_unk = tryCatch(conv_1_to_true(col_chemo_unk), error = function(e) return(NULL)),
      col_margins_unk = tryCatch(conv_1_to_true(col_margins_unk), error = function(e) return(NULL)),
      col_ileus_unk = tryCatch(conv_1_to_true(col_ileus_unk), error = function(e) return(NULL)),
      col_indication = tryCatch(conv_col_indication(col_indication), error = function(e) return(NULL)),
      col_emergent = tryCatch(conv_col_emergent(col_emergent), error = function(e) return(NULL)),
      col_open_assist = tryCatch(conv_col_open_assist(col_approach), error = function(e) return(NULL)),
      col_unplanned_conversion = tryCatch(conv_col_unplanned_conversion(col_approach), error = function(e) return(NULL)),
      col_approach = tryCatch(conv_col_approach(col_approach), error = function(e) return(NULL)),
      col_malignancyt = tryCatch(conv_col_malignancyt(col_malignancyt), error = function(e) return(NULL)),
      col_malignancyn = tryCatch(conv_col_malignancyn(col_malignancyn), error = function(e) return(NULL)),
      col_malignancym = tryCatch(conv_col_malignancym(col_malignancym), error = function(e) return(NULL)),
      col_leak_treatment = tryCatch(conv_col_leak_treatment(col_anastomotic), error = function(e) return(NULL)),
      col_anastomotic = tryCatch(conv_col_anastomotic(col_anastomotic), error = function(e) return(NULL))
    )
}

reorder_cols <- function(df) {
  df %>%
    dplyr::select(dplyr::any_of(c("caseid", "pufyear", "sex", " race", "ethnicity_hispanic", "prncptx", "cpt", "workrvu", "inout", "transt", "age", "admyr,
                                admsyr", "operyr", "electsurg", "dischdest", "anesthes", "anesthes_other", "attend", "surgspec", "height", "weight", "diabetes", "insulin", "smoke", "packs,
                                etoh", "dyspnea", "when_dyspnea", "dnr", "fnstatus1", "fnstatus2", "ventilat", "hxcopd", "cpneumon", "ascites", "esovar,
                                hxchf", "hxmi", "prvpci", "prvpcs", "hxangina", "hypermed", "hxpvd", "restpain", "renafail", "dialysis,
                                impsens", "coma", "hemi", "hxtia", "cva", "cvano", "tumorcns", "para", "quad", "discancr", "wndinf", "steroid,
                                wtloss", "bleeddis", "transfus", "chemo", "radio", "prsepis", "type_prsepis", "pregnancy", "proper30", "dprna", "dprbun,
                                dprcreat", "dpralbum", "dprbili", "dprsgot", "dpralkph", "dprwbc", "dprhct", "dprplate", "dprptt", "dprpt,
                                dprinr", "prsodm", "prbun", "prcreat", "pralbum", "prbili", "prsgot", "pralkph", "prwbc", "prhct", "prplate,
                                prptt", "prinr", "prpt", "otherproc1", "othercpt1", "otherwrvu1", "otherproc2", "othercpt2", "otherwrvu2,
                                otherproc3", "othercpt3", "otherwrvu3", "otherproc4", "othercpt4", "otherwrvu4", "otherproc5", "othercpt5,
                                otherwrvu5", "otherproc6", "othercpt6", "otherwrvu6", "otherproc7", "othercpt7", "otherwrvu7", "otherproc8,
                                othercpt8", "otherwrvu8", "otherproc9", "othercpt9", "otherwrvu9", "otherproc10", "othercpt10", "otherwrvu10,
                                concurr1", "concpt1", "conwrvu1", "concurr2", "concpt2", "conwrvu2", "concurr3", "concpt3", "conwrvu3", "concurr4,
                                concpt4", "conwrvu4", "concurr5", "concpt5", "conwrvu5", "concurr6", "concpt6", "conwrvu6", "concurr7", "concpt7,
                                conwrvu7", "concurr8", "concpt8", "conwrvu8", "concurr9", "concpt9", "conwrvu9", "concurr10", "concpt10", "conwrvu10,
                                opnote", "pgy", "emergncy", "wndclas", "asaclas", "airtra", "mallamp", "rbc", "anesurg", "surgane", "dpatrm", "anetime,
                                optime", "typeintoc", "sdisdt", "hdisdt", "yrdeath", "tothlos", "admqtr", "htooday", "stooday", "totslos", "nsupinfec,
                                supinfec", "dsupinfec", "sssipatos", "nwndinfd", "wndinfd", "dwndinfd", "dssipatos", "norgspcssi", "orgspcssi", "dorgspcssi", "ossipatos", "ndehis", "dehis,
                                ddehis", "noupneumo", "oupneumo", "doupneumo", "pnapatos", "nreintub", "reintub", "dreintub", "npulembol", "pulembol", "dpulembol,
                                nfailwean", "failwean", "dfailwean", "ventpatos", "nrenainsf", "renainsf", "drenainsf", "noprenafl", "oprenafl", "doprenafl", "nurninfec,
                                urninfec", "durninfec", "utipatos", "ncnscva", "cnscva", "dcnscva", "ncnscoma", "cnscoma", "dcnscoma", "nneurodef", "neurodef", "dneurodef,
                                ncdarrest", "cdarrest", "dcdarrest", "ncdmi", "cdmi", "dcdmi", "nothbleed", "othbleed", "dothbleed", "nothgrafl", "othgrafl,
                                dothgrafl", "nothdvt", "othdvt", "dothdvt", "nothsysep", "othsysep", "dothsysep", "sepsispatos", "nothseshock", "othseshock", "dothseshock", "sepshockpatos", "othcdiff,
                                nothcdiff", "dothcdiff", "podiag", "podiagtx", "returnor", "dsdtohd", "dopertod", "doptodis", "mortprob", "morbprob,
                                stillinhosp,reoperation1", "retorpodays", "reoporcpt1", "retorrelated", "reoporicd91", "reoperation2", "retor2podays", "reopor2cpt1,
                                retor2related", "reopor2icd91", "reoperation3", "readmission1", "readmpodays1", "unplannedreadmission1", "readmrelated1,
                                readmsuspreason1", "readmrelicd91", "readmission2", "readmpodays2", "unplannedreadmission2", "readmrelated2,
                                readmsuspreason2", "readmrelicd92", "readmission3", "readmpodays3", "unplannedreadmission3", "readmrelated3,
                                readmsuspreason3", "readmrelicd93", "readmission4", "readmpodays4", "unplannedreadmission4", "readmrelated4,
                                readmsuspreason4", "readmrelicd94", "readmission5", "readmpodays5", "unplannedreadmission5", "readmrelated5,
                                readmsuspreason5", "readmrelicd95", "readmunrelsusp1", "readmunrelicd91", "readmunrelsusp2", "readmunrelicd92,
                                readmunrelsusp3", "readmunrelicd93", "readmunrelsusp4", "readmunrelicd94", "readmunrelsusp5", "readmunrelicd95,
                                podiag10", "podiagtx10", "reopor1icd101", "reopor2icd101", "readmrelicd101", "readmunrelicd101", "readmrelicd102,
                                readmunrelicd102", "readmrelicd103", "readmunrelicd103", "readmrelicd104", "readmunrelicd104", "readmrelicd105,
                                readmunrelicd105", "wound_closure", "podiag_other", "podiag_other10", "col_steroid", "col_steroid_unk", "col_mech_bowel_prep,
                                col_mech_bowel_prep_unk", "col_oral_antibiotic", "col_oral_antibiotic_unk", "col_chemo", "col_chemo_unk", "col_indication,
                                col_icd9_indication", "col_emergent", "col_icd9_emergent", "col_approach", "col_open_assist", "col_unplanned_conversion,
                                col_margins", "col_margins_unk", "col_malignancyt", "col_malignancyn", "col_malignancym", "col_anastomotic", "col_leak_treatment,
                                col_ileus", "col_ileus_unk", "col_nodeseval", "col_icd10_indication", "col_icd10_emergent")))
}

