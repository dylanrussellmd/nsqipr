#' Convert targeted colectomy columns
#'
#' @param df a data table to be cleaned
#' @param filename the name of the file from which the data table has been read in
#'
#' @details If the file being processed is a targeted colectomy dataset,
#' it will be processed by this function. This function determines how data cleaning steps specific
#' to targeted colectomy dataset files should proceed.
#'
#' @keywords internal
#'
conv_col_cols <- function(df, filename) {
  get_pufyear(df, filename)
  conv_(df, "col_approach", conv_open_assist, newcol = "col_open_assist")
  conv_(df, "col_approach", conv_unplanned_conversion, newcol = "col_unplanned_conversion")
  conv_(df, "col_anastomotic", conv_col_leak_treatment, newcol = "col_leak_treatment")
  conv_(df, "col_anastomotic", conv_col_anastomotic)
  data.table::setnames(df, c("col_icd10_indication","col_icd10_emergent"), c("col_indication_icd","col_emergent_icd"))
}

#### ---- FACTOR LISTS (THESE DEFINE THE FACTOR LEVELS FOR VARIOUS COLUMNS) ---- ####
col_malignancym <- list(
  `M0/Mx` = c("M0/Mx", "M0"),
  `M1` = "M1",
  `M1a` = "M1a",
  `M1b` = "M1b"
)
col_malignancyt <- list(
  `T0` = "T0",
  `T1` = "T1",
  `T2` = "T2",
  `T3` = "T3",
  `T4` = "T4",
  `T4a` = "T4a",
  `T4b` = "T4b",
  `Tis` = "Tis",
  `Tx` = "Tx"
)
col_malignancyn <- list(
  `N0` = "N0",
  `N1` = "N1",
  `N1a` = "N1a",
  `N1b` = "N1b",
  `N1c` = "N1c",
  `N2` = "N2",
  `N2a` = "N2a",
  `N2b` = "N2b",
  `Nx` = "Nx"
)
col_approach <- list(
  `Endoscopic` = c("Endoscopic","Endoscopic w/ open assist","Endoscopic w/ unplanned conversion to open"),
  `Hybrid` = c("Hybrid", "Hybrid w/ open assist","Hybrid w/ unplanned conversion to open"),
  `Laparoscopic` = c("Laparoscopic","Laparoscopic w/ open assist","Laparoscopic w/ unplanned conversion to open","Laparoscopic w/ unplanned conversion to Open","Laparoscopic hand assisted","Laparoscopic Hand Assisted"),
  `NOTES` = c("NOTES","NOTES w/ open assist","NOTES w/ unplanned conversion to open"),
  `Open` = c("Open","Open (planned)"),
  `Other` = "Other",
  `Other MIS` = c("Other MIS approach","Other MIS approach w/ open assist","Other MIS approach w/ unplanned conversion to open"),
  `Robotic` = c("Robotic","Robotic w/ open assist","Robotic w/ unplanned conversion to open"),
  `SILS` = c("SILS","SILS w/ open assist","SILS w/ unplanned conversion to open")
)
col_emergent <- list(
  `Bleeding` = "Bleeding",
  `Obstruction` = "Obstruction",
  `Perforation` = "Perforation",
  `Toxic colitis` = "Toxic colitis (Toxic Megacolon, C. diff w/out perforation, Ischemic Colitis)",
  `Other` = c("Other (enter ICD-9 code)", "Other (enter ICD-10 code)")
)
col_indication = list(
  `Acute diverticulitis` = "Acute diverticulitis",
  `Bleeding` = "Bleeding",
  `Chronic diverticular disease` = "Chronic diverticular disease",
  `Colon cancer` = "Colon cancer",
  `Colon cancer w/ obstruction` = "Colon cancer w/ obstruction",
  `Chrohn's disease` = "Crohn's Disease",
  `Enterocolitis` = "Enterocolitis (e.g. C. Difficile)",
  `Non-malignant polyp` = "Non-malignant polyp",
  `Other` =  c("Other-Enter ICD-9 for diagnosis","Other-Enter ICD-10 for diagnosis"),
  `Ulcerative colitis` = "Ulcerative colitis",
  `Volvulus` = "Volvulus"
)

#### ---- FUNCTIONS ---- ####

#' Parse entries that indicate the presence of an anastomotic leak
#'
#' @param vec a character vector to parse
#'
#' @details returns TRUE if case-insensitive "yes" or "leak'
#' is detected in the character vector.
#'
#' @return a logical vector
#' @keywords internal
#' @examples
#' x <- c("No", "Yes-reoperation", "Yes-percutaneous intervention",
#' "Yes-no intervention required",
#' "No definitive diagnosis of leak/leak related abscess",
#' "Leak, treated w/ reoperation", "Leak, treated w/ interventional means",
#' "Leak, no treatment intervention documented",
#' "Leak, treated w/ non-interventional/non-operative means", NA)
#'
#' cbind(x, nsqipr:::conv_col_anastomotic(x))
#'
conv_col_anastomotic <- function(vec) {
  stringi::stri_detect_regex(vec, "^yes-|^leak,", opts_regex = list(case_insensitive = TRUE))
}

#' Parse a column for type of anastomotic leak intervention
#'
#' @param vec a character vector of values to convert
#'
#' @details NSQIP encodes the \code{col_anastomotic} column as having one of multiple interventions.
#' This function extracts those values from character vectors and factors them.
#'
#' @return a factor vector
#' @keywords internal
#'
#' @examples
#' x <- c("No", "Yes-reoperation", "Yes-percutaneous intervention",
#' "Yes-no intervention required",
#' "No definitive diagnosis of leak/leak related abscess",
#' "Leak, treated w/ reoperation", "Leak, treated w/ interventional means",
#' "Leak, no treatment intervention documented",
#' "Leak, treated w/ non-interventional/non-operative means", NA)
#'
#' nsqipr:::conv_col_leak_treatment(x)
#'
conv_col_leak_treatment <- function(vec) {
  vec %^% list(
    `Reoperation` = "Yes-reoperation",
    `Percutaneous intervention` = "Yes-percutaneous intervention",
    `No intervention` = "Yes-no intervention required",
    `Reoperation` = "Leak, treated w/ reoperation",
    `Percutaneous intervention` = "Leak, treated w/ interventional means",
    `No intervention` = "Leak, no treatment intervention documented",
    `Non-operative intervention` = "Leak, treated w/ non-interventional/non-operative means"
  )
}
