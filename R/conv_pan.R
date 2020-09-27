#' Convert targeted pancreatectomy columns
#'
#' @param df a data table to be cleaned
#' @param filename the name of the file from which the data table has been read in
#'
#' @details If the file being processed is a targeted pancreatectomy data set,
#' it will be processed by this function. This function determines how data cleaning steps specific
#' to targeted pancreatectomy files should proceed.
#'
#' @keywords internal
#'
conv_pan_cols <- function(df, filename) {
  get_pufyear(df, filename)
  conv_(df, "pan_approach", conv_open_assist, newcol = "pan_open_assist")
  conv_(df, "pan_approach", conv_unplanned_conversion, newcol = "pan_unplanned_conversion")
  conv_(df, "pan_fistula", conv_pan_fistula_type, newcol = "pan_fistula_type")
  conv_(df, "pan_fistula", conv_pan_fistula_intervention, newcol = "pan_fistula_intervention")
  conv_(df, "pan_fistula", conv_pan_fistula)
  conv_(df, "pan_delgastric_20140315", conv_notno, newcol = "pan_delgastric")
  conv_(df, "pan_delgastric_20140315", conv_pan_delgastric, newcol = "pan_delgastric_tx")
  make_pan_percdrainage_cols(df)
  data.table::setnames(df, "pan_percdrain_20140315", "pan_percdrain") # Ugly, and maybe needs to be more elegantly applied.
}

#### ---- FACTOR LISTS (THESE DEFINE THE FACTOR LEVELS FOR VARIOUS COLUMNS) ----
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
pan_glandtext <- list(
  `Soft` = "Soft",
  `Intermediate` = "Intermediate",
  `Hard` = "Hard"
)
pan_ductsize <- list(
  `<3 mm` = "<3 mm",
  `3-6 mm` = "3-6 mm",
  `>6 mm` = ">6 mm"
)
pan_approach <- list(
  `Hybrid` = c("Hybrid", "Hybrid w/ open assist","Hybrid w/ unplanned conversion to open"),
  `Laparoscopic` = c("Laparoscopic","Laparoscopic w/ open assist","Laparoscopic w/ unplanned conversion to open","Laparoscopic hand assisted"),
  `NOTES` = c("NOTES","NOTES w/ open assist","NOTES w/ unplanned conversion to open"),
  `Open` = c("Open","Open (planned)"),
  `Other` = "Other",
  `Other MIS` = c("Other MIS approach","Other MIS approach w/ open assist","Other MIS approach w/ unplanned conversion to open"),
  `Robotic` = c("Robotic","Robotic w/ open assist","Robotic w/ unplanned conversion to open"),
  `SILS` = c("SILS","SILS w/ open assist","SILS w/ unplanned conversion to open")
)
pan_biliarystent <- list(
  `No stent at time of surgery` = "No stent at time of surgery",
  `Endoscopic stent` = "Endoscopic stent",
  `Percutaneous stent` = "Percutaneous stent",
  `Stent of other unknown type` = "Stent of other or unknown type"
)
pan_lapthor <- list(
  `Laparoscopic` = "49329",
  `Open` = "48999",
  `Other` = "Other"
)
pan_percdrainage <- list(
  `Other` = "Yes-other",
  `Bile` = "Yes-bile",
  `Pus` = "Yes-pus",
  `Amylase-rich fluid` = "Yes-amylase-rich fluid"
)
pan_percdrainage1 <- pan_percdrainage
pan_percdrainage2 <- pan_percdrainage
pan_percdrainage3 <- pan_percdrainage
pan_percdrainage4 <- pan_percdrainage

#### ---- LONG COLUMNS ---- ####
pan_percdrainage_cols <- paste("pan_percdrainage", 1:4, sep = "")
pan_amylase_cols <- c("pan_amylase_pod1", "pan_amylase_pod230")
#### ---- FUNCTIONS ---- ####

#' Create percutaneous drainage columns for long conversion
#'
#' @param df a data table to add the columns to
#'
#' @details First checks if the data table contains a "pan_percdrainage" column.
#' If so, the column is split into 4 columns according to the regex pattern ",\\s?".
#'
#' @keywords internal
#'
#' @examples
#' x <- data.table::data.table(
#' pan_percdrainage = c("Yes-other", "Yes-bile", "Yes-pus", "Yes-amylase-rich fluid",
#' "Yes-amylase-rich fluid,Yes-pus", "Yes-bile,Yes-other", "Yes-amylase-rich fluid,Yes-pus,Yes-other",
#' "Yes-amylase-rich fluid,Yes-bile,Yes-other", "Yes-pus,Yes-bile", "Yes-pus,Yes-other",
#' "Yes-amylase-rich fluid,Yes-pus,Yes-bile", "Yes-amylase-rich fluid,Yes-other",
#' "Yes-amylase-rich fluid,Yes-pus,Yes-bile,Yes-other", "Yes-amylase-rich fluid,Yes-bile",
#' "Yes-pus,Yes-bile,Yes-other")
#' )
#'
#' nsqipr:::make_pan_percdrainage_cols(x)
#' x
#'
make_pan_percdrainage_cols <- function(df) {
  if("pan_percdrainage" %qsin% names(df)) {
    mat <- stringi::stri_split_regex(df[["pan_percdrainage"]], ",\\s?", simplify = NA, n = 4, omit_empty = TRUE, opts_regex = list(case_insensitive = TRUE))
    for(j in seq_along(pan_percdrainage_cols)) data.table::set(df, j = pan_percdrainage_cols[[j]], value = mat[, j])
  }
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
