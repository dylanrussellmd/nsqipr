#' Convert targeted hepatectomy columns
#'
#' @param df a data table to be cleaned
#' @param filename the name of the file from which the data table has been read in
#'
#' @details If the file being processed is a targeted hepatectomy data set,
#' it will be processed by this function. This function determines how data cleaning steps specific
#' to targeted hepatectomy files should proceed.
#'
#' @keywords internal
#'
conv_hep_cols <- function(df, filename) {
  get_pufyear(df, filename)
  conv_(df, "hep_viral", conv_hep_hepb, newcol = "hep_hepb")
  conv_(df, "hep_viral", conv_hep_hepc, newcol = "hep_hepc")
  conv_(df, "hep_viral", conv_hep_otherviral, newcol = "hep_otherviral")
  conv_(df, "hep_approach", conv_open_assist, newcol = "hep_open_assist")
  conv_(df, "hep_approach", conv_unplanned_conversion, newcol = "hep_unplanned_conversion")
  conv_(df, "hep_con_partres", conv_hep_con_partres)
  conv_(df, "hep_recon", conv_notno)
  conv_(df, "hep_bileleakage", conv_hep_bileleakage_type, newcol = "hep_bileleakage_type")
  conv_(df, "hep_bileleakage", conv_hep_bileleakage_intervention, newcol = "hep_bileleakage_intervention")
  conv_(df, "hep_bileleakage", conv_hep_bileleakage)
  conv_(df, "hep_liverfail", conv_hep_liverfail_type, newcol = "hep_liverfail_type")
  conv_(df, "hep_liverfail", conv_hep_liverfail)
  conv_(df, "hep_sec_numtumors", conv_hep_sec_numtumors)
  coalesce_cols(df, "ddrainsremoval", "damylase")
  make_long_cols(df, "hep_neotherapy_140101", hep_neotherapy_cols)
  make_long_cols(df, "hep_con_ablation_140101", hep_con_ablation_cols)
  make_long_cols(df, "hep_invasive_type", hep_invasive_type_cols)
}

#### ---- FACTOR LISTS (THESE DEFINE THE FACTOR LEVELS FOR VARIOUS COLUMNS) ---- #### BE SURE TO ADD THESE TO FACTOR_COLS IN COL_DEFINITIONS.R
hep_benign_lesion <- list(
  `<2 cm` = "<2 cm",
  `2-5 cm` = "2-5 cm",
  `>5 cm` = ">5 cm"
)
hep_benign_histologic <- list(
  `Focal nodular hyperplasia` = "Focal nodular hyperplasia",
  `Biliary cyst` = "Biliary cyst",
  `Hepatic abscess` = "Hepatic abscess",
  `Hepatic adenoma` = "Hepatic adenoma",
  `Hemangioma` = "Hemangioma",
  `Hepatic cyst` = "Hepatic cyst",
  `Other` = "Other"
)
hep_sec_tumorsize <- list(
  `<2 cm` = "<2 cm",
  `2-5 cm` = "2-5 cm",
  `>5 cm` = ">5 cm"
)
hep_sec_histologic <- list(
  `Colorectal` = "Colorectal metastasis",
  `Sarcoma` = "Sarcoma metastases",
  `Breast cancer` = "Breast cancer metastasis",
  `Neuroendocrine` = "Neuroendocrine metastasis",
  `Other` = "Other type"
)
hep_mstage <- list(
  `M0/Mx` = "M0/Mx",
  `M1` = "M1"
)
hep_tstage <- list(
  `T0` = "T0",
  `T1` = "T1",
  `T2` = "T2",
  `T3` = "T3",
  `T4` = "T4",
  `Tis` = "Tis",
  `Tx` = "Tx"
)
hep_nstage <- list(
  `N0` = "N0",
  `N1` = "N1",
  `N2` = "N2",
  `Nx` = "Nx"
)
hep_histologic <- list(
  `Hepatocellular carcinoma` = "Hepatocellular carcinoma",
  `Gallbladder cancer` = "Gallbladder cancer",
  `Intrahepatic cholangiocarcinoma` = "Intrahepatic cholangiocarcinoma",
  `Hilar cholangiocarcinoma` = "Hilar cholangiocarcinoma",
  `Other` = "Other type"
)
hep_pathres <- list(
  `Benign` = "Benign",
  `Primary hepatobiliary cancer` = "Primary hepatobiliary cancer",
  `Secondary (metastatic) tumor` = "Secondary (metastatic) tumor"
)
hep_livertext <- list(
  `Cirrhotic` = "Cirrhotic",
  `Congested` = "Congested",
  `Fatty` = "Fatty",
  `Normal` = "Normal"
)
hep_approach <- list(
  `Hybrid` = c("Hybrid", "Hybrid w/ open assist","Hybrid w/ unplanned conversion to open"),
  `Laparoscopic` = c("Laparoscopic","Laparoscopic w/ open assist","Laparoscopic w/ unplanned conversion to open"),
  `Open` = c("Open","Open (planned)"),
  `Other` = "Other",
  `Other MIS` = c("Other MIS approach","Other MIS approach w/ open assist","Other MIS approach w/ unplanned conversion to open"),
  `Robotic` = c("Robotic","Robotic w/ open assist","Robotic w/ unplanned conversion to open"),
  `Endoscopic` = c("Endoscopic","Endoscopic w/ unplanned conversion to open")
)
hep_biliarystent <- list(
  `None` = "No",
  `Endoscopic stent` = "Yes-endoscopic",
  `Percutaneous stent` = "Yes-percutaneous",
  `Stent of other unknown type` = "Yes-stent of unknown or other type"
)
hep_lapthor <- list(
  `Laparoscopic` = "47379",
  `Open` = "47399",
  `Other` = "Other"
)
hep_liverfail_grade <- list(
  `Grade A` = "Grade A",
  `Grade B` = "Grade B",
  `Grade C` = "Grade C"
)
hep_neotherapy_140101 <- list(
  `Other` = "Other type",
  `Locoregional interarterial infusion` = "Locoregional interarterial infusion",
  `Locoregional liver ablation` = "Locoregional liver ablation",
  `Portal vein embolization` = "Portal vein embolization",
  `Preoperative systemic chemotherapy` = "Preoperative systemic chemotherapy"
)
hep_neotherapy1 <- hep_neotherapy_140101
hep_neotherapy2 <- hep_neotherapy_140101
hep_neotherapy3 <- hep_neotherapy_140101
hep_neotherapy4 <- hep_neotherapy_140101
hep_neotherapy5 <- hep_neotherapy_140101

hep_con_ablation_140101 <- list(
  `Other` = "Other ablation",
  `Alcohol` = "Alcohol ablation",
  `Cryoablation` = "Cryoablation",
  `Microwave` = "Microwave ablation",
  `RFA` = "RFA ablation"
)
hep_con_ablation1 <- hep_con_ablation_140101
hep_con_ablation2 <- hep_con_ablation_140101
hep_con_ablation3 <- hep_con_ablation_140101
hep_con_ablation4 <- hep_con_ablation_140101
hep_con_ablation5 <- hep_con_ablation_140101

hep_invasive_type <- list(
  `Pus from drain or aspirate` = "Pus from drain or aspirate",
  `Other intervention` = "Other intervention",
  `Biliary stent for biliary obstruction/leak` = "Biliary stent for biliary obstruction/leak",
  `Bilirubin-rich fluid from drain or aspirate` = "Bilirubin-rich fluid from drain or aspirate",
  `Intervention other than transfusion for bleeding/hematoma` = "Intervention other than transfusion for bleeding/hematoma"
)

hep_invasive_type1 <- hep_invasive_type
hep_invasive_type2 <- hep_invasive_type
hep_invasive_type3 <- hep_invasive_type
hep_invasive_type4 <- hep_invasive_type
hep_invasive_type5 <- hep_invasive_type

#### ---- LONG COLUMNS ---- ####
hep_neotherapy_cols <- paste("hep_neotherapy", 1:5, sep = "")
hep_con_ablation_cols <- paste("hep_con_ablation", 1:5, sep = "")
hep_invasive_type_cols <- paste("hep_invasive_type", 1:5, sep = "")

#### ---- FUNCTIONS ---- ####

#' Parse entries that indicate a Hepatitis B infection
#'
#' @param vec a character vector to parse
#'
#' @details returns TRUE if case-insensitive "hepatitis b"
#' is detected in the character vector.
#'
#' @return a logical vector
#' @keywords internal
#' @examples
#' x <- c("None","Hepatitis B","Unknown","Hepatitis B and C","Hepatitis C",
#' "Other", NA)
#'
#' cbind(x, nsqipr:::conv_hep_hepb(x))
#'
conv_hep_hepb <- function(vec) {
  stringi::stri_detect_regex(vec, "^hepatitis b", opts_regex = list(case_insensitive = TRUE))
}

#' Parse entries that indicate a Hepatitis C infection
#'
#' @param vec a character vector to parse
#'
#' @details returns TRUE if case-insensitive "hepatitis c"
#' is detected in the character vector.
#'
#' @return a logical vector
#' @keywords internal
#' @examples
#' x <- c("None","Hepatitis B","Unknown","Hepatitis B and C","Hepatitis C",
#' "Other", NA)
#'
#' cbind(x, nsqipr:::conv_hep_hepc(x))
#'
conv_hep_hepc <- function(vec) {
  stringi::stri_detect_regex(vec, "^hepatitis c|^hepatitis b and c", opts_regex = list(case_insensitive = TRUE))
}

#' Parse entries that indicate a non-Hepatitis viral infection
#'
#' @param vec a character vector to parse
#'
#' @details returns TRUE if case-insensitive "other"
#' is detected in the character vector.
#'
#' @return a logical vector
#' @keywords internal
#' @examples
#' x <- c("None","Hepatitis B","Unknown","Hepatitis B and C","Hepatitis C",
#' "Other", NA)
#'
#' cbind(x, nsqipr:::conv_hep_otherviral(x))
#'
conv_hep_otherviral <- function(vec) {
  stringi::stri_detect_regex(vec, "^other", opts_regex = list(case_insensitive = TRUE))
}

#' Convert concurrent partial resections to integer
#'
#' @param vec a character vector of values to convert
#'
#' @details NSQIP encodes anyone with more than 10 concurrent partial resections as "10 or more".
#' This converts all "10 or more" to 10. If given NA, will return NA.
#'
#' @return an integer vector
#' @keywords internal
#'
#' @examples
#' nsqipr:::conv_hep_con_partres(c("1","2","10 or more",NA))
#'
conv_hep_con_partres <- function(vec) {
  as.integer(ifelse(stringi::stri_detect_fixed(vec, "10 or more", opts_fixed = list(case_insensitive = TRUE)), "10", vec))
}

#' Convert number of secondary tumors to integer
#'
#' @param vec a character vector of values to convert
#'
#' @details NSQIP encodes anyone with more than 8 secondary tumors as "8 or more".
#' This converts all "8 or more" to 8. If given NA, will return NA.
#'
#' @return an integer vector
#' @keywords internal
#'
#' @examples
#' nsqipr:::conv_hep_sec_numtumors(c("1","2","More than 8",NA))
#'
conv_hep_sec_numtumors <- function(vec) {
  as.integer(ifelse(stringi::stri_detect_fixed(vec, "More than 8", opts_fixed = list(case_insensitive = TRUE)), "8", vec))
}

#' Parse entries that indicate the presence of a bile leak
#'
#' @param vec a character vector to parse
#'
#' @details returns TRUE if case-insensitive "yes"
#' is detected in the character vector.
#'
#' @return a logical vector
#' @keywords internal
#' @examples
#' x <- c("No",
#' "Yes-clinical diagnosis, drain continued on or after POD3",
#' "Yes-clinical diagnosis, percutaneous drainage performed",
#' "Yes-clinical diagnosis, reoperation performed",
#' "Yes-clinical diagnosis, spontaneous wound drainage",
#' "Yes-persistent drainage, drain continued on or after POD3",
#' "Yes-persistent drainage, percutaneous drainage performed",
#' "Yes-persistent drainage, reoperation performed",
#' NA)
#'
#' cbind(x, nsqipr:::conv_hep_bileleakage(x))
#'
conv_hep_bileleakage <- function(vec) {
  stringi::stri_detect_regex(vec, "^yes", opts_regex = list(case_insensitive = TRUE))
}

#' Parse a column for type of bile leakage
#'
#' @param vec a character vector of values to convert
#'
#' @details NSQIP encodes the \code{hep_bileleakage} column as either a clinical diagnosis or
#' persistent drainage. This function extracts those values from character vectors
#' and factors them.
#'
#' @return a factor vector
#' @keywords internal
#'
#' @examples
#' x <- c("No",
#' "Yes-clinical diagnosis, drain continued on or after POD3",
#' "Yes-clinical diagnosis, percutaneous drainage performed",
#' "Yes-clinical diagnosis, reoperation performed",
#' "Yes-clinical diagnosis, spontaneous wound drainage",
#' "Yes-persistent drainage, drain continued on or after POD3",
#' "Yes-persistent drainage, percutaneous drainage performed",
#' "Yes-persistent drainage, reoperation performed",
#' NA)
#'
#' nsqipr:::conv_hep_bileleakage_type(x)
#'
conv_hep_bileleakage_type <- function(vec) {
  vec %^% list(
    `Clinical diagnosis` = "Yes-clinical diagnosis, drain continued on or after POD3",
    `Clinical diagnosis` = "Yes-clinical diagnosis, percutaneous drainage performed",
    `Clinical diagnosis` = "Yes-clinical diagnosis, reoperation performed",
    `Clinical diagnosis` = "Yes-clinical diagnosis, spontaneous wound drainage",
    `Persistent drainage` = "Yes-persistent drainage, drain continued on or after POD3",
    `Persistent drainage` = "Yes-persistent drainage, percutaneous drainage performed",
    `Persistent drainage` = "Yes-persistent drainage, reoperation performed"
  )
}

#' Parse a column for type of bile leak intervention
#'
#' @param vec a character vector of values to convert
#'
#' @details NSQIP encodes the \code{hep_bileleakage} column as having one of multiple interventions.
#' This function extracts those values from character vectors and factors them.
#'
#' @return a factor vector
#' @keywords internal
#'
#' @examples
#' x <- c("No",
#' "Yes-clinical diagnosis, drain continued on or after POD3",
#' "Yes-clinical diagnosis, percutaneous drainage performed",
#' "Yes-clinical diagnosis, reoperation performed",
#' "Yes-clinical diagnosis, spontaneous wound drainage",
#' "Yes-persistent drainage, drain continued on or after POD3",
#' "Yes-persistent drainage, percutaneous drainage performed",
#' "Yes-persistent drainage, reoperation performed",
#' NA)
#'
#' nsqipr:::conv_hep_bileleakage_intervention(x)
#'
conv_hep_bileleakage_intervention <- function(vec) {
  vec %^% list(
    `Drain continued on or after POD3` = "Yes-clinical diagnosis, drain continued on or after POD3",
    `Percutaneous drainage` = "Yes-clinical diagnosis, percutaneous drainage performed",
    `Reoperation` = "Yes-clinical diagnosis, reoperation performed",
    `Spontaneous wound drainage` = "Yes-clinical diagnosis, spontaneous wound drainage",
    `Drain continued on or after POD3` = "Yes-persistent drainage, drain continued on or after POD3",
    `Percutaneous drainage` = "Yes-persistent drainage, percutaneous drainage performed",
    `Reoperation` = "Yes-persistent drainage, reoperation performed"
  )
}

#' Parse entries that indicate the presence of liver failure
#'
#' @param vec a character vector to parse
#'
#' @details returns TRUE if case-insensitive "yes"
#' is detected in the character vector.
#'
#' @return a logical vector
#' @keywords internal
#' @examples
#' x <- c("No-does not meet criteria for PHLF",
#' "Yes-meets criteria for PHLF",
#' "Yes-PHLF (receiving clotting factors to maintain INR)",
#' NA)
#'
#' cbind(x, nsqipr:::conv_hep_liverfail(x))
#'
conv_hep_liverfail <- function(vec) {
  stringi::stri_detect_regex(vec, "^yes", opts_regex = list(case_insensitive = TRUE))
}

#' Parse a column for type of liver failure
#'
#' @param vec a character vector of values to convert
#'
#' @details NSQIP encodes the \code{hep_liverfail} column as either "meets criteria for PHLF" or
#' "PHFL (receiving clotting factors to maintain INR)". This function extracts those values from character vectors
#' and factors them.
#'
#' @return a factor vector
#' @keywords internal
#'
#' @examples
#' x <- c("No-does not meet criteria for PHLF",
#' "Yes-meets criteria for PHLF",
#' "Yes-PHLF (receiving clotting factors to maintain INR)",
#' NA)
#'
#' nsqipr:::conv_hep_liverfail_type(x)
#'
conv_hep_liverfail_type <- function(vec) {
  vec %^% list(
    `Meets criteria for PHLF` = "Yes-meets criteria for PHLF",
    `PHLF (receiving clotting factors to maintain INR)` = "Yes-PHLF (receiving clotting factors to maintain INR)"
  )
}
