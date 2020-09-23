#' Parse entries with open assist or hand assist
#'
#' @param vec a character vector to parse
#'
#' @details returns TRUE if either case-insensitive "w/ open assist" or
#' "hand assisted" is detected in the character vector. Cases with an
#' "open" or "open (planned)" approach will return NA.
#'
#' @return a logical vector
#' @keywords internal
#' @examples
#' x <- c("Endoscopic","Endoscopic w/ open assist","Endoscopic w/ unplanned conversion to open",
#' "Hybrid","Hybrid w/ open assist","Hybrid w/ unplanned conversion to open",
#' "Laparoscopic","Laparoscopic w/ open assist","Laparoscopic w/ unplanned conversion to open",
#' "Laparoscopic hand assisted","NOTES","NOTES w/ open assist","NOTES w/ unplanned conversion to open",
#' "Open","Open (planned)","Other","Other MIS approach","Other MIS approach w/ open assist",
#' "Other MIS approach w/ unplanned conversion to open","Robotic","Robotic w/ open assist",
#' "Robotic w/ unplanned conversion to open","SILS","SILS w/ open assist",
#' "SILS w/ unplanned conversion to open", NA)
#'
#' cbind(x, nsqipr:::conv_open_assist(x))
#'
conv_open_assist <- function(vec) {
  ifelse(stringi::stri_detect_regex(vec, "^Open", opts_regex = list(case_insensitive = TRUE)), NA,
         stringi::stri_detect_regex(vec, "w/ open assist$|hand assisted$", opts_regex = list(case_insensitive = TRUE)))
}

#' Parse entries with unplanned conversion to open
#'
#' @param vec a character vector to parse
#'
#' @details returns TRUE if case-insensitive "w/ unplanned conversion to open"
#' is detected in the character vector. Cases with an "open" or "open (planned)"
#' approach will return NA.
#'
#' @return a logical vector
#' @keywords internal
#' @examples
#' x <- c("Endoscopic","Endoscopic w/ open assist","Endoscopic w/ unplanned conversion to open",
#' "Hybrid","Hybrid w/ open assist","Hybrid w/ unplanned conversion to open",
#' "Laparoscopic","Laparoscopic w/ open assist","Laparoscopic w/ unplanned conversion to open",
#' "Laparoscopic hand assisted","NOTES","NOTES w/ open assist","NOTES w/ unplanned conversion to open",
#' "Open","Open (planned)","Other","Other MIS approach","Other MIS approach w/ open assist",
#' "Other MIS approach w/ unplanned conversion to open","Robotic","Robotic w/ open assist",
#' "Robotic w/ unplanned conversion to open","SILS","SILS w/ open assist",
#' "SILS w/ unplanned conversion to open", NA)
#'
#' cbind(x, nsqipr:::conv_unplanned_conversion(x))
#'
conv_unplanned_conversion <- function(vec) {
  ifelse(stringi::stri_detect_regex(vec, "^Open", opts_regex = list(case_insensitive = TRUE)), NA,
         stringi::stri_detect_regex(vec, "w/ unplanned conversion to open$", opts_regex = list(case_insensitive = TRUE)))
}
