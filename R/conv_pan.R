conv_pan_cols <- function(df) {
  df %>%
    dplyr::mutate(
      pufyear = tryCatch(conv_pufyear(caseid), error = function(e) return(NULL)),
      pan_lapthor = tryCatch(conv_pan_lapthor(pan_lapthor), error = function(e) return(NULL)),
      pan_biliarystent = tryCatch(conv_pan_biliarystent(pan_biliarystent), error = function(e) return(NULL)),
      pan_open_assist = tryCatch(conv_pan_open_assist(pan_approach), error = function(e) return(NULL)),
      pan_unplanned_conversion = tryCatch(conv_pan_unplanned_conversion(pan_approach), error = function(e) return(NULL)),
      pan_approach = tryCatch(conv_pan_approach(pan_approach), error = function(e) return(NULL)),
      pan_ductsize = tryCatch(conv_pan_ductsize(pan_ductsize), error = function(e) return(NULL)),
      pan_glandtext = tryCatch(conv_pan_glandtext(pan_glandtext), error = function(e) return(NULL)),
      pan_reconstruction = tryCatch(conv_pan_reconstruction(pan_reconstruction), error = function(e) return(NULL)),
      pan_gastduo = tryCatch(conv_pan_gastduo(pan_gastduo), error = function(e) return(NULL)),
      pan_drains_type = tryCatch(conv_pan_drains_type(pan_drains_type), error = function(e) return(NULL)),
      pan_resection = tryCatch(conv_pan_resection(pan_resection), error = function(e) return(NULL)),
      pan_fistula_intervention = tryCatch(conv_pan_fistula_intervention(pan_fistula), error = function(e) return(NULL)),
      pan_fistula_type = tryCatch(conv_pan_fistula_type(pan_fistula), error = function(e) return(NULL)),
      pan_fistula = tryCatch(conv_pan_fistula(pan_fistula), error = function(e) return(NULL)),
      pan_delgastric_20140315 = tryCatch(conv_pan_delgastric_20140315(pan_delgastric_20140315), error = function(e) return(NULL)),
      pan_percdrainage_amylase = tryCatch(conv_pan_percdrainage_amylase(pan_percdrainage), error = function(e) return(NULL)),
      pan_percdrainage_bile = tryCatch(conv_pan_percdrainage_bile(pan_percdrainage), error = function(e) return(NULL)),
      pan_percdrainage_pus = tryCatch(conv_pan_percdrainage_pus(pan_percdrainage), error = function(e) return(NULL)),
      pan_percdrainage_other = tryCatch(conv_pan_percdrainage_other(pan_percdrainage), error = function(e) return(NULL)),
      pan_malig_histologic = tryCatch(conv_pan_malig_histologic(pan_malig_histologic), error = function(e) return(NULL)),
      pan_tstage = tryCatch(conv_pan_tstage(pan_tstage), error = function(e) return(NULL)),
      pan_nstage = tryCatch(conv_pan_nstage(pan_nstage), error = function(e) return(NULL)),
      pan_mstage = tryCatch(conv_pan_mstage(pan_mstage), error = function(e) return(NULL)),
      pan_benign_histologic = tryCatch(conv_pan_benign_histologic(pan_benign_histologic), error = function(e) return(NULL)),
      pan_benign_tumorsize = tryCatch(conv_pan_benign_tumorsize(pan_benign_tumorsize), error = function(e) return(NULL)),
      pan_intra_antibiotics = tryCatch(conv_pan_intra_antibiotics(pan_intra_antibiotics), error = function(e) return(NULL)),
      pan_oincis_type = tryCatch(conv_pan_oincis_type(pan_oincis_type), error = function(e) return(NULL)),
      pan_drainsys_type = tryCatch(conv_pan_drainsys_type(pan_drainsys_type), error = function(e) return(NULL))
    )
}

conv_pan_drainsys_type <- function(vec) {
  val <- c("closed" = 1L,
           "open" = 2L,
           "closed and open" = 3L)

  unname(val[vec])
}

conv_pan_oincis_type <- function(vec) {
  val <- c("subcostal type" = 1L,
           "upper midline" = 2L,
           "other" = 3L,
           "unknown" = NA)

  unname(val[vec])
}

conv_pan_intra_antibiotics <- function(vec) {
  val <- c("1st generation cephalosporin" = 1L,
           "2nd or 3rd generation cephalosporin" = 2L,
           "broad spectrum" = 3L,
           "other" = 4L,
           "unknown" = NA)

  unname(val[vec])
}

conv_pan_benign_tumorsize <- function(vec) {
  val <- c("<2 cm" = 1L,
           "2-5 cm" = 2L,
           ">5 cm" = 3L,
           "unknown" = NA)

  unname(val[vec])
}

conv_pan_benign_histologic <- function(vec) {
  val <- c("chronic pancreatitis" = 1L,
           "ipmn-noninvasive" = 2L,
           "mucinous cystic neoplasm" = 3L,
           "neuroendocrine w/ no metastases" = 4L,
           "serous cystadenoma" = 5L,
           "solid pseudopapillary neoplasm" = 6L,
           "other" = 7L,
           "unknown" = NA)

  unname(val[vec])
}

conv_pan_mstage <- function(vec) {
  val <- c("m0/mx" = 1L,
           "m1" = 2L,
           "unknown" = NA,
           "n/a" = NA
  )
  unname(val[vec])
}

conv_pan_tstage <- function(vec) {
  val <- c("t0" = 1L,
           "t1" = 2L,
           "t2" = 3L,
           "t3" = 4L,
           "t4" = 5L,
           "tis" = 6L,
           "tx" = 7L,
           "unknown" = NA,
           "n/a" = NA
  )
  unname(val[vec])
}

conv_pan_nstage <- function(vec) {
  val <- c("n0" = 1L,
           "n1" = 2L,
           "nx" = 3L,
           "unknown" = NA,
           "n/a" = NA
  )
  unname(val[vec])
}

conv_pan_malig_histologic <- function(vec) {
  val <- c("ampullary carcinoma" = 1L,
           "cystadenocarcinoma" = 2L,
           "distal cholangiocarcinoma" = 3L,
           "duodenal carcinoma" = 4L,
           "ipmn-invasive" = 5L,
           "neuroendocrine-functioning" = 6L,
           "neuroendocrine-nonfunctioning" = 7L,
           "pancreatic adenocarcinoma" = 8L,
           "other type" = 9L,
           "unknown" = NA)

  unname(val[vec])
}

conv_pan_percdrainage_other <- function(vec) {
  stringr::str_detect(vec, "other")
}

conv_pan_percdrainage_pus <- function(vec) {
  stringr::str_detect(vec, "pus")
}

conv_pan_percdrainage_bile <- function(vec) {
  stringr::str_detect(vec, "bile")
}

conv_pan_percdrainage_amylase <- function(vec) {
  stringr::str_detect(vec, "amylase-rich")
}

conv_pan_delgastric_20140315 <- function(vec) {
  val <- c("yes-no oral intake by pod 14" = 1L,
           "yes-tube to external drainage/ng tube present/reinserted" = 2L,
           "no" = NA,
           "unknown" = NA)

  unname(val[vec])
}

conv_pan_fistula_intervention <- function(vec) {
  val <- c("biochemical leak only" = NA,
           "yes-grade b popf present" = NA,
           "yes-grade c popf present" = NA,
           "yes-clinical diagnosis, npo-tpn" = 1L,
           "yes-clinical diagnosis, drain continued >7 days" = 2L,
           "yes-clinical diagnosis, percutaneous drainage performed" = 3L,
           "yes-clinical diagnosis, reoperation performed" = 4L,
           "yes-clinical diagnosis, spontaneous wound drainage" = 5L,
           "yes-persistent drainage, npo-tpn" = 1L,
           "yes-persistent drainage, drain continued >7 days" = 2L,
           "yes-persistent drainage, percutaneous drainage performed" = 3L,
           "yes-persistent drainage, reoperation performed" = 4L,
           "no" = NA,
           "unknown" = NA,
           "no evidence of biochemical leak or popf" = NA)

  unname(val[vec])
}

conv_pan_fistula_type <- function(vec) {
  val <- c("biochemical leak only" = 1L,
           "yes-grade b popf present" = 2L,
           "yes-grade c popf present" = 3L,
           "yes-clinical diagnosis, npo-tpn" = 4L,
           "yes-clinical diagnosis, drain continued >7 days" = 4L,
           "yes-clinical diagnosis, percutaneous drainage performed" = 4L,
           "yes-clinical diagnosis, reoperation performed" = 4L,
           "yes-clinical diagnosis, spontaneous wound drainage" = 4L,
           "yes-persistent drainage, npo-tpn" = 5L,
           "yes-persistent drainage, drain continued >7 days" = 5L,
           "yes-persistent drainage, percutaneous drainage performed" = 5L,
           "yes-persistent drainage, reoperation performed" = 5L,
           "no" = NA,
           "unknown" = NA,
           "no evidence of biochemical leak or popf" = NA)

  unname(val[vec])
}

conv_pan_fistula <- function(vec) {
  stringr::str_detect(vec, "yes|biochemical leak only")
}

conv_pan_resection <- function(vec) {
  val <- c("vein" = 1L,
           "artery" = 2L,
           "vein and artery" = 3L,
           "not performed" = 4L,
           "unknown" = NA)

  unname(val[vec])
}

conv_pan_drains_type <- function(vec) {
  val <- c("pancreatic anastomosis" = 1L,
           "biliary anastomosis" = 2L,
           "pancreatic & biliary anastomosis" = 3L,
           "pancreatic parenchyma" = 4L,
           "type(s) cannot be determined" = NA)

  unname(val[vec])
}

conv_pan_gastduo <- function(vec) {
  val <- c("antecolic fashion" = 1L,
           "retrocolic fashion" = 2L,
           "not performed" = 3L,
           "unknown" = NA)

  unname(val[vec])
}

conv_pan_reconstruction <- function(vec) {
  val <- c("pancreaticogastrostomy" = 1L,
           "pancreaticojejunal invagination" = 2L,
           "pancreaticojejunal duct-to-mucosal" = 3L,
           "not performed" = 4L,
           "unknown" = NA)

  unname(val[vec])
}

conv_pan_glandtext <- function(vec) {
  val <- c("soft" = 1L,
           "intermediate" = 2L,
           "hard" = 3L,
           "unknown" = NA)

  unname(val[vec])
}

conv_pan_ductsize <- function(vec) {
  val <- c("<3 mm" = 1L,
           "3-6 mm" = 2L,
           ">6 mm" = 3L,
           "unknown" = NA)

  unname(val[vec])
}

conv_pan_open_assist <- function(vec) {
  stringr::str_detect(vec, "w/ open assist|hand assisted")
}

conv_pan_unplanned_conversion <- function(vec) {
  stringr::str_detect(vec, "w/ unplanned conversion to open")
}

conv_pan_approach <- function(vec) {
  val <- c("hybrid" = 1L,
           "hybrid w/ open assist" = 1L,
           "hybrid w/ unplanned conversion to open" = 1L,
           "laparoscopic" = 2L,
           "laparoscopic w/ open assist" = 2L,
           "laparoscopic w/ unplanned conversion to open" = 2L,
           "laparoscopic hand assisted" = 2L,
           "notes" = 3L,
           "notes w/ open assist" = 3L,
           "notes w/ unplanned conversion to open" = 3L,
           "open" = 4L,
           "open (planned)" = 4L,
           "other"= 5L,
           "other mis approach" = 6L,
           "other mis approach w/ open assist" = 6L,
           "other mis approach w/ unplanned conversion to open" = 6L,
           "robotic" = 7L,
           "robotic w/ open assist" = 7L,
           "robotic w/ unplanned conversion to open" = 7L,
           "sils" = 8L,
           "sils w/ open assist" = 8L,
           "sils w/ unplanned conversion to open" = 8L,
           "unknown" = NA
  )
  unname(val[vec])
}

conv_pan_biliarystent <- function(vec) {
  val <- c("no stent at time of surgery" = 1L,
           "endoscopic stent" = 2L,
           "percutaneous stent" = 3L,
           "stent of other or unknown type" = 4L)

  unname(val[vec])
}

conv_pan_lapthor <- function(vec) { # This may potentially break in the future if additional CPT codes are added.
  val <- c("49329" = 1L,
           "48999" = 2L,
           "other" = 3L)

  unname(val[vec])
}
