#' Convert targeted appendectomy columns
#'
#' @param df a data table to be cleaned
#' @param filename the name of the file from which the data table has been read in
#'
#' @details If the file being processed is a targeted appendectomy data set,
#' it will be processed by this function. This function determines how data cleaning steps specific
#' to targeted appendectomy files should proceed.
#'
#' @keywords internal
#'
conv_app_cols <- function(df, filename) {
  get_pufyear(df, filename)
  conv_(df, "app_img_ultra", conv_app_img_ultra, newcol = "app_ultra")
  conv_(df, "app_img_ct", conv_app_img_ct, newcol = "app_ct")
  data.table::setnames(df, "app_mri", "app_img_mri")
  conv_(df, "app_img_mri", conv_app_img_mri, newcol = "app_mri")
}

#### ---- FACTOR LISTS (THESE DEFINE THE FACTOR LEVELS FOR VARIOUS COLUMNS) ---- #### BE SURE TO ADD THESE TO FACTOR_COLS IN COL_DEFINITIONS.R
app_img_ultra <- list(
  `Consistent with appendicitis` = "US done-consistent with appendicitis",
  `Indeterminate` = "US done-indeterminate; result uncertain",
  `Not consistent with appendicitis` = "US done-not consistent with appendicitis"
)

app_setting_ultra <- list(
  `Operating hospital` = "US in operating hospital",
  `Outside facility` = "US at outside facility"
)

app_img_ct <- list(
  `Consistent with appendicitis` = "CT done-result consistent with diagnosis of appendicitis",
  `Indeterminate` = "CT done-result indeterminate; result uncertain",
  `Not consistent with appendicitis` = "CT done-result not consistent with appendicitis"
)

app_setting_ct <- list(
  `Operating hospital` = "CT in operating hospital",
  `Outside facility` = "CT at outside facility"
)

app_img_mri <- list(
  `Consistent with appendicitis` = "Result consistent w/ diagnosis of Appendicitis",
  `Indeterminate` = "Result Indeterminate / Uncertain",
  `Not consistent with appendicitis` = 'Result Not Consistent w/ Appendicitis; Appendix "Normal"'
)

app_setting_mri <- list(
  `Operating hospital` = "Performed in Operating Hospital",
  `Outside facility` = "Performed at Outside Facility"
)

app_pathres <- list(
  `Consistent with appendicitis` = "Consistent with appendicitis",
  `Not consistent with appendicitis` = "Not consistent with appendicitis",
  `Indeterminate` = "Result is indeterminate or uncertain",
  `Other appendiceal pathology` = "Other appendiceal pathology",
  `Tumor/malignancy involving appendix` = "Tumor/malignancy involving appendix"
)

#### ---- LONG COLUMNS ---- ####

#### ---- FUNCTIONS ---- ####

#' Parse entries that indicate an ultrasound was performed
#'
#' @param vec a character vector to parse
#'
#' @details returns TRUE if case-insensitive "us done-"
#' is detected in the character vector.
#'
#' @return a logical vector
#' @keywords internal
#' @examples
#' x <- c("US done-consistent with appendicitis","US not performed/not documented",
#' "US done-indeterminate; result uncertain","US done-not consistent with appendicitis", NA)
#'
#' cbind(x, nsqipr:::conv_app_img_ultra(x))
#'
conv_app_img_ultra <- function(vec) {
  stringi::stri_detect_regex(vec, "^US done-", opts_regex = list(case_insensitive = TRUE))
}

#' Parse entries that indicate a CT was performed
#'
#' @param vec a character vector to parse
#'
#' @details returns TRUE if case-insensitive "ct done-"
#' is detected in the character vector.
#'
#' @return a logical vector
#' @keywords internal
#' @examples
#' x <- c("CT not performed/not documented","CT done-result consistent with diagnosis of appendicitis",
#' "CT done-result not consistent with appendicitis","CT done-result indeterminate; result uncertain", NA)
#'
#' cbind(x, nsqipr:::conv_app_img_ct(x))
#'
conv_app_img_ct <- function(vec) {
  stringi::stri_detect_regex(vec, "^CT done-", opts_regex = list(case_insensitive = TRUE))
}

#' Parse entries that indicate an MRI was performed
#'
#' @param vec a character vector to parse
#'
#' @details returns TRUE if case-insensitive "result"
#' is detected in the character vector.
#'
#' @return a logical vector
#' @keywords internal
#' @examples
#' x <- c("MRI / Other Definitive Imaging Modality Not Performed / Not Documented","Result Indeterminate / Uncertain",
#' "Result consistent w/ diagnosis of Appendicitis",'Result Not Consistent w/ Appendicitis; Appendix "Normal"', NA)
#'
#' cbind(x, nsqipr:::conv_app_img_mri(x))
#'
conv_app_img_mri <- function(vec) {
  stringi::stri_detect_regex(vec, "^Result", opts_regex = list(case_insensitive = TRUE))
}
