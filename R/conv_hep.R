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
  make_long_cols(df, "hep_neotherapy_140101", hep_neotherapy_cols)
  make_long_cols(df, "hep_con_ablation_140101", hep_con_ablation_cols)
  make_long_cols(df, "hep_invasive_type", hep_invasive_type_cols)
}

#### ---- FACTOR LISTS (THESE DEFINE THE FACTOR LEVELS FOR VARIOUS COLUMNS) ---- ####
pan_drainsys_type <- list(
  `Closed` = "Closed",
  `Open` = "Open",
  `Closed and open` = "Closed and Open"
)
pan_oincis_type <- list(
  `Subcostal type` = "Subcostal type",
  `Upper midline` = "Upper midline",
  `Other` = "Other"
)
pan_intra_antibiotics <- list(
  `1st generation cephalosporin` = "1st generation cephalosporin",
  `2nd or 3rd generation cephalosporin` = "2nd or 3rd generation cephalosporin",
  `Broad spectrum` = "Broad spectrum",
  `Other` = "Other"
)
pan_benign_tumorsize <- list(
  `<2 cm` = "<2 cm",
  `2-5 cm` = "2-5 cm",
  `>5 cm` = ">5 cm"
)
pan_benign_histologic <- list(
  `Chronic pancreatitis` = "Chronic pancreatitis",
  `IPMN-noninvasive` = "IPMN-noninvasive",
  `Mucinous cystic neoplasm` = "Mucinous cystic neoplasm",
  `Neuroendocrine w/ no metastases` = "Neuroendocrine w/ no metastases",
  `Serous cystadenoma` = "Serous cystadenoma",
  `Solid pseudopapillary neoplasm` = "Solid pseudopapillary neoplasm",
  `Other` = "Other"
)
pan_mstage <- list(
  `M0/Mx` = "M0/Mx",
  `M1` = "M1"
)
pan_tstage <- list(
  `T0` = "T0",
  `T1` = "T1",
  `T2` = "T2",
  `T3` = "T3",
  `T4` = "T4",
  `Tis` = "Tis",
  `Tx` = "Tx"
)
pan_nstage <- list(
  `N0` = "N0",
  `N1` = "N1",
  `Nx` = "Nx"
)
pan_malig_histologic <- list(
  `Ampullary carcinoma` = "Ampullary carcinoma",
  `Cystadenocarcinoma` = "Cystadenocarcinoma",
  `Distal cholangiocarcinoma` = "Distal cholangiocarcinoma",
  `Duodenal carcinoma` = "Duodenal carcinoma",
  `IPMN-invasive` = "IPMN-invasive",
  `Neuroendocrine-functioning` = "Neuroendocrine-functioning",
  `Neuroendocrine-nonfunctioning` = "Neuroendocrine-nonfunctioning",
  `Pancreatic adenocarcinoma` = "Pancreatic adenocarcinoma",
  `Other` = "Other type"
)
pan_resection <- list(
  `Vein` = "Vein",
  `Artery` = "Artery",
  `Vein and artery` = "Vein and artery",
  `Not performed` = "Not performed"
)
pan_drains_type <- list(
  `Pancreatic anastomosis` = "Pancreatic anastomosis",
  `Biliary anastomosis` = "Biliary anastomosis",
  `Pancreatic and biliary anastomosis` = "Pancreatic & Biliary Anastomosis",
  `Pancreatic parenchyma` = "Pancreatic parenchyma"
)
pan_gastduo <- list(
  `Antecolic fashion` = "Antecolic fashion",
  `Retrocolic fasion` = "Retrocolic fashion",
  `Not performed` = "Not performed"
)
pan_reconstruction <- list(
  `Pancreaticogastrostomy` = "Pancreaticogastrostomy",
  `Pancreaticojejunal invagination` = "Pancreaticojejunal invagination",
  `Pancreaticojejunal duct-to-mucosal` = "Pancreaticojejunal duct-to-mucosal",
  `Not performed` = "Not performed"
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
  `Percutaneous stent` = "Yes-percutaneous stent",
  `Stent of other unknown type` = "Yes-stent of unknown or other type"
)
hep_lapthor <- list(
  `Laparoscopic` = "47379",
  `Open` = "47399",
  `Other` = "Other"
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

#' Parse entries that indicate the presence of a fistula
#'
#' @param vec a character vector to parse
#'
#' @details returns TRUE if case-insensitive "yes" or "biochemical leak only'
#' is detected in the character vector.
#'
#' @return a logical vector
#' @keywords internal
#' @examples
#' x <- c("No","No evidence of Biochemical Leak or POPF","Biochemical Leak only",
#' "Yes, Grade B POPF present","Yes, Grade C POPF present","Yes-clinical diagnosis, NPO-TPN",
#' "Yes-clinical diagnosis, drain continued >7 days",
#' "Yes-clinical diagnosis, percutaneous drainage performed",
#' "Yes-clinical diagnosis, reoperation performed",
#' "Yes-clinical diagnosis, spontaneous wound drainage","Yes-persistent drainage, NPO-TPN",
#' "Yes-persistent drainage, drain continued >7 days",
#' "Yes-persistent drainage, percutaneous drainage performed",
#' "Yes-persistent drainage, reoperation performed",
#' NA)
#'
#' cbind(x, nsqipr:::conv_pan_fistula(x))
#'
conv_pan_fistula <- function(vec) {
  stringi::stri_detect_regex(vec, "^yes|^biochemical leak only", opts_regex = list(case_insensitive = TRUE))
}

#' Parse a column for type of fistula
#'
#' @param vec a character vector of values to convert
#'
#' @details NSQIP encodes the \code{pan_fistula} column as either a biochemical leak, a clinical diagnosis,
#' persistent drainage, or a grade B or C POPF. This function extracts those values from character vectors
#' and factors them.
#'
#' @return a factor vector
#' @keywords internal
#'
#' @examples
#' fistulas <- c("No", "Yes-persistent drainage, drain continued >7 days",
#' "Yes-clinical diagnosis, drain continued >7 days",
#' "Yes-persistent drainage, percutaneous drainage performed",
#' "Yes-clinical diagnosis, percutaneous drainage performed",
#' "Yes-persistent drainage, reoperation performed",
#' "Unknown", "Yes-clinical diagnosis, reoperation performed",
#' "Yes-clinical diagnosis, spontaneous wound drainage",
#' "Yes-persistent drainage, NPO-TPN", "Yes-clinical diagnosis, NPO-TPN",
#' "No evidence of Biochemical Leak or POPF",
#' "Biochemical Leak only", "Yes, Grade B POPF present", "Yes, Grade C POPF present",
#' NA)
#'
#' nsqipr:::conv_pan_fistula_type(fistulas)
#'
conv_pan_fistula_type <- function(vec) {
  vec %^% list(
    `Biochemical leak only` = "Biochemical Leak only",
    `Grade B POPF` = "Yes, Grade B POPF present",
    `Grade C POPF` = "Yes, Grade C POPF present",
    `Clinical diagnosis` = "Yes-clinical diagnosis, NPO-TPN",
    `Clinical diagnosis` = "Yes-clinical diagnosis, drain continued >7 days",
    `Clinical diagnosis` = "Yes-clinical diagnosis, percutaneous drainage performed",
    `Clinical diagnosis` = "Yes-clinical diagnosis, reoperation performed",
    `Clinical diagnosis` = "Yes-clinical diagnosis, spontaneous wound drainage",
    `Persistent drainage` = "Yes-persistent drainage, NPO-TPN",
    `Persistent drainage` = "Yes-persistent drainage, drain continued >7 days",
    `Persistent drainage` = "Yes-persistent drainage, percutaneous drainage performed",
    `Persistent drainage` = "Yes-persistent drainage, reoperation performed"
  )
}

#' Parse a column for type of fistula intervention
#'
#' @param vec a character vector of values to convert
#'
#' @details NSQIP encodes the \code{pan_fistula} column as having one of multiple interventions.
#' This function extracts those values from character vectors and factors them.
#'
#' @return a factor vector
#' @keywords internal
#'
#' @examples
#' fistulas <- c("No", "Yes-persistent drainage, drain continued >7 days",
#' "Yes-clinical diagnosis, drain continued >7 days",
#' "Yes-persistent drainage, percutaneous drainage performed",
#' "Yes-clinical diagnosis, percutaneous drainage performed",
#' "Yes-persistent drainage, reoperation performed",
#' "Unknown", "Yes-clinical diagnosis, reoperation performed",
#' "Yes-clinical diagnosis, spontaneous wound drainage",
#' "Yes-persistent drainage, NPO-TPN", "Yes-clinical diagnosis, NPO-TPN",
#' "No evidence of Biochemical Leak or POPF",
#' "Biochemical Leak only", "Yes, Grade B POPF present", "Yes, Grade C POPF present")
#'
#' nsqipr:::conv_pan_fistula_intervention(fistulas)
#'
conv_pan_fistula_intervention <- function(vec) {
  vec %^% list(
    `NPO-TPN` = "Yes-clinical diagnosis, NPO-TPN",
    `Drain continued >7 days` = "Yes-clinical diagnosis, drain continued >7 days",
    `Percutaneous drainage` = "Yes-clinical diagnosis, percutaneous drainage performed",
    `Reoperation` = "Yes-clinical diagnosis, reoperation performed",
    `Spontaneous wound drainage` = "Yes-clinical diagnosis, spontaneous wound drainage",
    `NPO-TPN` = "Yes-persistent drainage, NPO-TPN",
    `Drain continued >7 days` = "Yes-persistent drainage, drain continued >7 days",
    `Percutaneous drainage` = "Yes-persistent drainage, percutaneous drainage performed",
    `Reoperation` = "Yes-persistent drainage, reoperation performed"
  )
}

#' Parse a column for type of delayed gastric emptying treatment
#'
#' @param vec a character vector of values to convert
#'
#' @details NSQIP encodes the \code{pan_delgastric_20140315} column as either no or
#' having one of multiple interventions. This function extracts those values from
#' character vectors and factors them.
#'
#' @return a factor vector
#' @keywords internal
#'
#' @examples
#' delgastric <- c("No", "Yes-no oral intake by POD 14",
#' "Yes-tube to external drainage/NG tube present/reinserted")
#'
#' nsqipr:::conv_pan_delgastric(delgastric)
#'
conv_pan_delgastric <- function(vec) {
  vec %^% list(
    `No oral intake by POD 14` = "Yes-no oral intake by POD 14",
    `Tube to external drainage/NG tube present/reinserted` = "Yes-tube to external drainage/NG tube present/reinserted"
  )
}
