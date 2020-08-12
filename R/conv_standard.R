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
nsqip <- function(path, set = "puf", write_to_csv = FALSE, append = FALSE, return_df = TRUE) {
  lapply(get_file_or_dir(path, pattern = "*.txt$"),
         conv_to_standard, set = set, write_to_csv = write_to_csv, append = append, return_df = return_df)
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

