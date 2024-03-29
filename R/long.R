#' Convert pan_percdrainage column from wide to long format
#'
#' @param df a data.table
#'
#' @details The data contained in the 4 "pan_percdrainage" columns created by \code{make_pan_percdrainage_cols}
#' are converted into a long format.
#'
#' If "pan_percdrainage" is a column in \code{df}, it will be broken into a long format with
#' \code{caseid} as the ID variable for joining back to the main table. This is because the targeted pancreatectomy
#' datasets input multiple values into a single "pan_percdrainage" column. For example,
#' "Yes-other, Yes-bile, Yes-pus" may be an entry in the raw data set. This makes
#' parsing for patients that had purulent percutaneous drainage at any point, for example, very difficult.
#'
#' Note that this does not alter the "pan_percdrainage" column.
#'
#' @return a data.table
#'
#' @keywords internal
#' @examples
#' x <- data.table::data.table(caseid = 1:15,
#' pan_percdrainage = c("Yes-other", "Yes-bile", "Yes-pus", "Yes-amylase-rich fluid",
#' "Yes-amylase-rich fluid,Yes-pus", "Yes-bile,Yes-other", "Yes-amylase-rich fluid,Yes-pus,Yes-other",
#' "Yes-amylase-rich fluid,Yes-bile,Yes-other", "Yes-pus,Yes-bile", "Yes-pus,Yes-other",
#' "Yes-amylase-rich fluid,Yes-pus,Yes-bile", "Yes-amylase-rich fluid,Yes-other",
#' "Yes-amylase-rich fluid,Yes-pus,Yes-bile,Yes-other", "Yes-amylase-rich fluid,Yes-bile",
#' "Yes-pus,Yes-bile,Yes-other"))
#'
#' nsqipr:::make_pan_percdrainage_cols(x)
#' nsqipr:::make_pan_percdrainage_long(x)
#'
make_pan_percdrainage_long <- function(df) {
  if("pan_percdrainage" %qsin% names(df)) {
    long <- suppressWarnings(data.table::melt(df, id.vars = "caseid",
                                              measure.vars = pan_percdrainage_cols,
                                              variable.name = "npercdrainage",
                                              value.name = "percdrainage",
                                              na.rm = TRUE,
                                              variable.factor = FALSE,
                                              value.factor = TRUE))
    data.table::set(long, j = "npercdrainage", value = stringi::stri_extract_all_regex(long[["npercdrainage"]], "\\d", simplify = TRUE))
    data.table::set(long, j = "npercdrainage", value = as.integer(long[["npercdrainage"]]))
    data.table::setorder(long, caseid)
    return(long)
  }
}

#' Convert amylase columns from wide to long format
#'
#' @param df a data.table
#'
#' @details The data contained in the 3 amylase columns of the targeted pancreatectomy
#' dataset ("pan_amylase_pod1", "pan_amylase_pod230", and "damylase") are converted into a
#' long format. This reduces the total number of columns from 3 to 2 and makes the data
#' more intuitive.
#'
#' If both "pan_amylase_pod1" and "pan_amylase_pod230" are columns in \code{df},
#' it will be broken into a long format with \code{caseid} as the ID variable
#' for joining back to the main table. The amylase values from both "pan_amylase_pod1"
#' and "pan_amylase_pod230" are placed in the "amylase" column". The POD on which
#' this value was acquired (either 1 if from "pan_amylase_pod1" column or set equal
#' to the value of the "damylase" column) is placed in the "pod" column.
#'
#' Note that this does not alter the original columns.
#'
#' @return a data.table
#'
#' @keywords internal
#' @examples
#' x <- data.table::data.table(caseid = 1:30,
#' pan_amylase_pod1 = c(NA, sample(1:10000, 29)),
#' pan_amylase_pod230 = c(NA, sample(1000:10000, 29)),
#' damylase = c(NA, sample(2:30, 29)))
#'
#' nsqipr:::make_amylase_long(x)
#'
make_amylase_long <- function(df) {
  if(all(pan_amylase_cols %qsin% names(df))) {
    melted <- suppressWarnings(data.table::melt(df, id.vars = c("caseid", "damylase"),
                                                measure.vars = pan_amylase_cols,
                                                variable.name = "pod",
                                                value.name = "amylase",
                                                variable.factor = FALSE,
                                                value.factor = FALSE,
                                                na.rm = TRUE))
    data.table::set(melted, j = c("damylase","pod"), value = list(
                        ifelse(melted[["pod"]] == "pan_amylase_pod1", 1, melted[["damylase"]]),
                        NULL))
    data.table::setnames(melted, "damylase","pod")
    data.table::setorder(melted, caseid)
    return(melted)
  }
}

#' Convert hep_neotherapy_140101 column from wide to long format
#'
#' @param df a data.table
#'
#' @details The data contained in the 5 "hep_neotherapy" columns created by \code{make_hep_neotherapy_cols}
#' are converted into a long format.
#'
#' If "hep_neotherapy_140101" is a column in \code{df}, it will be broken into a long format with
#' \code{caseid} as the ID variable for joining back to the main table. This is because the targeted hepatectomy
#' datasets input multiple values into a single "hep_neotherapy_140101" column. For example,
#' "Locoregional interarterial infusion,Other type" may be an entry in the raw data set. This makes
#' parsing for patients that had other types of neoadjuvant therapy at any point, for example, very difficult.
#'
#' Note that this does not alter the "hep_neotherapy_140101" column.
#'
#' @return a data.table
#'
#' @keywords internal
#' @examples
#' x <- data.table::data.table(caseid = 1:22,
#' hep_neotherapy_140101 = c("Preoperative systemic chemotherapy", "Portal vein embolization",
#' "Locoregional liver ablation", "Preoperative systemic chemotherapy,Portal vein embolization",
#' "Locoregional interarterial infusion,Portal vein embolization",
#' "Locoregional liver ablation,Portal vein embolization", "Other type",
#' "Preoperative systemic chemotherapy,Locoregional liver ablation",
#' "Preoperative systemic chemotherapy,Other type", "Locoregional interarterial infusion",
#' "Portal vein embolization,Other type", "Locoregional interarterial infusion,Other type",
#' "Preoperative systemic chemotherapy,Locoregional interarterial infusion",
#' "Preoperative systemic chemotherapy,Portal vein embolization,Other type",
#' "Preoperative systemic chemotherapy,Locoregional liver ablation,Other type",
#' "Preoperative systemic chemotherapy,Locoregional liver ablation,Portal vein embolization",
#' "Locoregional liver ablation,Other type",
#' "Locoregional interarterial infusion,Locoregional liver ablation,Portal vein embolization",
#' "Preoperative systemic chemotherapy,Locoregional interarterial infusion,Portal vein embolization",
#' "Locoregional interarterial infusion,Locoregional liver ablation",
#' "Preoperative systemic chemotherapy,Locoregional interarterial infusion,Locoregional liver ablation",
#' "Locoregional interarterial infusion,Locoregional liver ablation,Portal vein embolization,Other type"))
#'
#' nsqipr:::make_hep_neotherapy_cols(x)
#' nsqipr:::make_hep_neotherapy_long(x)
#'
make_hep_neotherapy_long <- function(df) {
  if("hep_neotherapy_140101" %qsin% names(df)) {
    long <- suppressWarnings(data.table::melt(df, id.vars = "caseid",
                                              measure.vars = hep_neotherapy_cols,
                                              variable.name = "nneotherapy",
                                              value.name = "neotherapy",
                                              na.rm = TRUE,
                                              variable.factor = FALSE,
                                              value.factor = TRUE))
    data.table::set(long, j = "nneotherapy", value = stringi::stri_extract_all_regex(long[["nneotherapy"]], "\\d", simplify = TRUE))
    data.table::set(long, j = "nneotherapy", value = as.integer(long[["nneotherapy"]]))
    data.table::setorder(long, caseid)
    return(long)
  }
}

#' Convert hep_con_ablation_140101 column from wide to long format
#'
#' @param df a data.table
#'
#' @details The data contained in the 5 "hep_con_ablation" columns created by \code{make_hep_con_ablation_cols}
#' are converted into a long format.
#'
#' If "hep_con_ablation_140101" is a column in \code{df}, it will be broken into a long format with
#' \code{caseid} as the ID variable for joining back to the main table. This is because the targeted hepatectomy
#' datasets input multiple values into a single "hep_con_ablation_140101" column. For example,
#' "Microwave ablation,Other ablation" may be an entry in the raw data set. This makes
#' parsing for patients that had other types of concurrent ablation at any point, for example, very difficult.
#'
#' Note that this does not alter the "hep_con_ablation_140101" column.
#'
#' @return a data.table
#'
#' @keywords internal
#' @examples
#' x <- data.table::data.table(caseid = 1:12,
#' hep_con_ablation_140101 = c("Microwave ablation", "RFA ablation", "Other ablation",
#' "Microwave ablation,Other ablation", "RFA ablation,Microwave ablation",
#' "RFA ablation,Other ablation", "RFA ablation,Alcohol ablation", "Alcohol ablation",
#' "Microwave ablation,Alcohol ablation", "Cryoablation", NA, "RFA ablation,Cryoablation"))
#'
#' nsqipr:::make_hep_con_ablation_cols(x)
#' nsqipr:::make_hep_con_ablation_long(x)
#'
make_hep_con_ablation_long <- function(df) {
  if("hep_con_ablation_140101" %qsin% names(df)) {
    long <- suppressWarnings(data.table::melt(df, id.vars = "caseid",
                                              measure.vars = hep_con_ablation_cols,
                                              variable.name = "ncon_ablation",
                                              value.name = "con_ablation",
                                              na.rm = TRUE,
                                              variable.factor = FALSE,
                                              value.factor = TRUE))
    data.table::set(long, j = "ncon_ablation", value = stringi::stri_extract_all_regex(long[["ncon_ablation"]], "\\d", simplify = TRUE))
    data.table::set(long, j = "ncon_ablation", value = as.integer(long[["ncon_ablation"]]))
    data.table::setorder(long, caseid)
    return(long)
  }
}

#' Convert hep_invasive_type column from wide to long format
#'
#' @param df a data.table
#'
#' @details The data contained in the 5 "hep_invasive_type" columns created by \code{make_hep_invasive_type_cols}
#' are converted into a long format.
#'
#' If "hep_invasive_type" is a column in \code{df}, it will be broken into a long format with
#' \code{caseid} as the ID variable for joining back to the main table. This is because the targeted hepatectomy
#' datasets input multiple values into a single "hep_invasive_type" column. For example,
#' "Biliary stent for biliary obstruction/leak,Other intervention" may be an entry in the raw data set. This makes
#' parsing for patients that had other types of invasive procedures at any point, for example, very difficult.
#'
#' Note that this does not alter the "hep_invasive_type" column.
#'
#' @return a data.table
#'
#' @keywords internal
#' @examples
#' x <- data.table::data.table(caseid = 1:7,
#' hep_invasive_type = c("Biliary stent for biliary obstruction/leak", "Bilirubin-rich fluid from drain or aspirate", "Other intervention",
#' "Biliary stent for biliary obstruction/leak,Other intervention", "Bilirubin-rich fluid from drain or aspirate,Biliary stent for biliary obstruction/leak",
#' "Bilirubin-rich fluid from drain or aspirate,Other intervention", NA))
#'
#' hep_invasive_type_cols <- paste("hep_invasive_type", 1:5, sep = "")
#'
#' nsqipr:::make_long_cols(x, "hep_invasive_type", hep_invasive_type_cols)
#' nsqipr:::make_hep_invasive_type_long(x)
#'
make_hep_invasive_type_long <- function(df) {
  if("hep_invasive_type" %qsin% names(df)) {
    long <- suppressWarnings(data.table::melt(df, id.vars = "caseid",
                                              measure.vars = hep_invasive_type_cols,
                                              variable.name = "ninvasive_type",
                                              value.name = "invasive_type",
                                              na.rm = TRUE,
                                              variable.factor = FALSE,
                                              value.factor = TRUE))
    data.table::set(long, j = "ninvasive_type", value = stringi::stri_extract_all_regex(long[["ninvasive_type"]], "\\d", simplify = TRUE))
    data.table::set(long, j = "ninvasive_type", value = as.integer(long[["ninvasive_type"]]))
    data.table::setorder(long, caseid)
    return(long)
  }
}
