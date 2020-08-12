#' Converts a single NSQIP \code{.txt} or a directory of NSQIP \code{.txt} files into a standardized NSQIP data frame.
#'
#' @return a data frame of class \code{tibble}.
#'
#' @param path path to file or directory.
#' @param write_to_csv write the resulting data frame to a \code{.csv} file. Produces individual files for each \code{.txt} input
#' @param append writes the resulting data frames all to a single \code{.csv} file called \code{nsqip_clean.csv}
#' @param return_df return the resulting data frame
#'
#' @export
#' @importFrom "utils" "file_test"
#'
nsqip <- function(path, write_to_csv = FALSE, append = FALSE, return_df = TRUE) {
  lapply(get_file_or_dir(path, pattern = "*.txt$"),
         conv_to_standard, write_to_csv = write_to_csv, append = append, return_df = return_df)
}

conv_to_standard <- function(file, write_to_csv, append, return_df) {

  df <- readr::read_tsv(file, col_types = readr::cols(.default = "c")) %>%
    set_up_df() %>%
    conv_type_cols() %>%
    conv_special_cols() %>%
    dplyr::select(!dplyr::any_of(redundant_cols)) %>%
    reorder_cols()

  if(write_to_csv & !append) readr::write_csv(df, path = paste(tools::file_path_sans_ext(file), "_clean.csv", sep = ""), na = "", col_names = FALSE)
  if(write_to_csv & append) readr::write_csv(df, path = paste(dirname(file),"/nsqip_clean.csv", sep = ""), na = "", col_names = FALSE, append = TRUE)

  if(return_df) return(df)
}

set_up_df <- function(df) {
  df %>%
    dplyr::rename_with(., tolower) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), tolower)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), dplyr::na_if, "unknown")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), dplyr::na_if, "null")) %>%
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

reorder_cols <- function(df) {
  df %>%
    dplyr::select(caseid, pufyear, sex,  race, ethnicity_hispanic, prncptx, cpt, workrvu, inout, transt, age, admyr,
                  admsyr, operyr, electsurg, dischdest, anesthes, anesthes_other, attend, surgspec, height, weight, diabetes, insulin, smoke, packs,
                  etoh, dyspnea, when_dyspnea, dnr, fnstatus1, fnstatus2, ventilat, hxcopd, cpneumon, ascites, esovar,
                  hxchf, hxmi, prvpci, prvpcs, hxangina, hypermed, hxpvd, restpain, renafail, dialysis,
                  impsens, coma, hemi, hxtia, cva, cvano, tumorcns, para, quad, discancr, wndinf, steroid,
                  wtloss, bleeddis, transfus, chemo, radio, prsepis, type_prsepis, pregnancy, proper30, dprna, dprbun,
                  dprcreat, dpralbum, dprbili, dprsgot, dpralkph, dprwbc, dprhct, dprplate, dprptt, dprpt,
                  dprinr, prsodm, prbun, prcreat, pralbum, prbili, prsgot, pralkph, prwbc, prhct, prplate,
                  prptt, prinr, prpt, otherproc1, othercpt1, otherwrvu1, otherproc2, othercpt2, otherwrvu2,
                  otherproc3, othercpt3, otherwrvu3, otherproc4, othercpt4, otherwrvu4, otherproc5, othercpt5,
                  otherwrvu5, otherproc6, othercpt6, otherwrvu6, otherproc7, othercpt7, otherwrvu7, otherproc8,
                  othercpt8, otherwrvu8, otherproc9, othercpt9, otherwrvu9, otherproc10, othercpt10, otherwrvu10,
                  concurr1, concpt1, conwrvu1, concurr2, concpt2, conwrvu2, concurr3, concpt3, conwrvu3, concurr4,
                  concpt4, conwrvu4, concurr5, concpt5, conwrvu5, concurr6, concpt6, conwrvu6, concurr7, concpt7,
                  conwrvu7, concurr8, concpt8, conwrvu8, concurr9, concpt9, conwrvu9, concurr10, concpt10, conwrvu10,
                  opnote, pgy, emergncy, wndclas, asaclas, airtra, mallamp, rbc, anesurg, surgane, dpatrm, anetime,
                  optime, typeintoc, sdisdt, hdisdt, yrdeath, tothlos, admqtr, htooday, stooday, totslos, nsupinfec,
                  supinfec, dsupinfec, sssipatos, nwndinfd, wndinfd, dwndinfd, dssipatos, norgspcssi, orgspcssi, dorgspcssi, ossipatos, ndehis, dehis,
                  ddehis, noupneumo, oupneumo, doupneumo, pnapatos, nreintub, reintub, dreintub, npulembol, pulembol, dpulembol,
                  nfailwean, failwean, dfailwean, ventpatos, nrenainsf, renainsf, drenainsf, noprenafl, oprenafl, doprenafl, nurninfec,
                  urninfec, durninfec, utipatos, ncnscva, cnscva, dcnscva, ncnscoma, cnscoma, dcnscoma, nneurodef, neurodef, dneurodef,
                  ncdarrest, cdarrest, dcdarrest, ncdmi, cdmi, dcdmi, nothbleed, othbleed, dothbleed, nothgrafl, othgrafl,
                  dothgrafl, nothdvt, othdvt, dothdvt, nothsysep, othsysep, dothsysep, sepsispatos, nothseshock, othseshock, dothseshock, sepshockpatos, othcdiff,
                  nothcdiff, dothcdiff, podiag, podiagtx, returnor, dsdtohd, dopertod, doptodis, mortprob, morbprob,
                  stillinhosp,reoperation1, retorpodays, reoporcpt1, retorrelated, reoporicd91, reoperation2, retor2podays, reopor2cpt1,
                  retor2related, reopor2icd91, reoperation3, readmission1, readmpodays1, unplannedreadmission1, readmrelated1,
                  readmsuspreason1, readmrelicd91, readmission2, readmpodays2, unplannedreadmission2, readmrelated2,
                  readmsuspreason2, readmrelicd92, readmission3, readmpodays3, unplannedreadmission3, readmrelated3,
                  readmsuspreason3, readmrelicd93, readmission4, readmpodays4, unplannedreadmission4, readmrelated4,
                  readmsuspreason4, readmrelicd94, readmission5, readmpodays5, unplannedreadmission5, readmrelated5,
                  readmsuspreason5, readmrelicd95, readmunrelsusp1, readmunrelicd91, readmunrelsusp2, readmunrelicd92,
                  readmunrelsusp3, readmunrelicd93, readmunrelsusp4, readmunrelicd94, readmunrelsusp5, readmunrelicd95,
                  podiag10, podiagtx10, reopor1icd101, reopor2icd101, readmrelicd101, readmunrelicd101, readmrelicd102,
                  readmunrelicd102, readmrelicd103, readmunrelicd103, readmrelicd104, readmunrelicd104, readmrelicd105,
                  readmunrelicd105, wound_closure, podiag_other, podiag_other10)
}
