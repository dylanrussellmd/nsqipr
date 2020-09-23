conv_aaa_cols <- function(df, filename) {
  get_pufyear(df, filename)
  conv_(df, "aaa_iculos", conv_aaa_iculos)
  data.table::setnames(df, "aaa_colitiis_treat", "aaa_colitis_treat")
}

#### ---- FACTOR LISTS (THESE DEFINE THE FACTOR LEVELS FOR VARIOUS COLUMNS) ----
aaa_surgind <- list(
  `Diameter` = "Diameter",
  `Dissection` = "Dissection",
  `Embolization` = "Embolization",
  `Non-ruptured symptomatic` = "Non-ruptured symptomatic",
  `Other` = "Other indication for surgery",
  `Prior endovascular intervention with unsatisfactory result` = "Prior endovascular intervention w/ unsatisfactory result",
  `Rupture with hypotension or use of pressors` = "Rupture w/ hypotension or use of pressors",
  `Rupture without hypotension` = "Rupture w/out hypotension",
  `Thrombosis` = "Thrombosis",
  `Prior open intervention with unsatisfactory result` = "Prior open intervention w/ unsatisfactory result"
)
aaa_surgap <- list(
  `Retroperitoneal` = "Retroperitoneal",
  `Transperitoneal midline` = "Transperitoneal-midline",
  `Transperitoneal transverse` = "Transperitoneal-transverse"
)
aaa_pcl <- list(
  `Above one renal` = "Above one renal",
  `Between SMA and renals` = c("Between SMA & renals","Between SMA CHR(38) renals"),
  `Infrarenal` = "Infrarenal",
  `Supraceliac` = "Supraceliac"
)
aaa_pae <- list(
  `Infrarenal` = "Infrarenal",
  `Juxtarenal` = "Juxtarenal",
  `Pararenal` = "Pararenal",
  `Supra-renal` = "Supra-renal",
  `Type IV thoracoabdominal aneurysm` = "Type IV Thoracoabdominal aneurysm"
)
aaa_distext <- list(
  `Aortic` = "Aortic",
  `Common iliac` = "Common iliac",
  `External iliac` = "External iliac",
  `Internal iliac` = "Internal iliac"
)
aaa_mima <- list(
  `Chronically occluded` = "Chronically occluded",
  `Implanted` = "Implanted",
  `Ligated` = "Ligated"
)
aaa_colitis_treat <- list(
  `Medical` = "Medical treatment",
  `Surgical` = "Surgical treatment"
)

#### ---- FUNCTIONS ---- ####

#' Convert targeted AAA ICU length of stay to integer
#'
#' @param vec a character vector of values to convert
#'
#' @details NSQIP encodes anyone staying 30 days or more as "30 or more".
#' This converts all "30 or more" to 30. If given NA, will return NA.
#'
#' @return an integer vector
#' @keywords internal
#'
#' @examples
#' nsqipr:::conv_aaa_iculos(c("14","2","30 or more",NA))
#'
conv_aaa_iculos <- function(vec) {
  as.integer(ifelse(stringi::stri_detect_fixed(vec, "30 or more", opts_fixed = list(case_insensitive = TRUE)), "30", vec))
}
