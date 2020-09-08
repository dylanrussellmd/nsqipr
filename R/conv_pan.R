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
  c(`Closed` = "closed",
    `Open` = "open",
    `Closed and open` = "closed and open"
    ) %>% fact(vec)
}

conv_pan_oincis_type <- function(vec) {
  c(`Subcostal type` = "subcostal type",
    `Upper midline` = "upper midline",
    `Other` = "other"
    ) %>% fact(vec)
}

conv_pan_intra_antibiotics <- function(vec) {
  c(`1st generation cephalosporin` = "1st generation cephalosporin",
    `2nd or 3rd generation cephalosporin` = "2nd or 3rd generation cephalosporin",
    `Broad spectrum` = "broad spectrum",
    `Other` = "other"
    ) %>% fact(vec)
}

conv_pan_benign_tumorsize <- function(vec) {
  c(`<2 cm` = "<2 cm",
    `2-5 cm` = "2-5 cm",
    `>5 cm` = ">5 cm"
    ) %>% fact(vec)
}

conv_pan_benign_histologic <- function(vec) {
  c(`Chronic pancreatitis` = "chronic pancreatitis",
    `IPMN-noninvasive` = "ipmn-noninvasive",
    `Mucinous cystic neoplasm` = "mucinous cystic neoplasm",
    `Neuroendocrine w/ no metastases` = "neuroendocrine w/ no metastases",
    `Serous cystadenoma` = "serous cystadenoma",
    `Solid pseudopapillary neoplasm` = "solid pseudopapillary neoplasm",
    `Other` = "other"
    ) %>% fact(vec)
}

conv_pan_mstage <- function(vec) {
  c(`M0/Mx` = "m0/mx",
    `M1` = "m1"
  ) %>% fact(vec)
}

conv_pan_tstage <- function(vec) {
  c(`T0` = "t0",
    `T1` = "t1",
    `T2` = "t2",
    `T3` = "t3",
    `T4` = "t4",
    `Tis` = "tis",
    `Tx` = "tx"
  ) %>% fact(vec)
}

conv_pan_nstage <- function(vec) {
  c(`N0` = "n0",
    `N1` = "n1",
    `Nx` = "nx"
  ) %>% fact(vec)
}

conv_pan_malig_histologic <- function(vec) {
 c(`Ampullary carcinoma` = "ampullary carcinoma",
   `Cystadenocarcinoma` = "cystadenocarcinoma",
   `Distal cholangiocarcinoma` = "distal cholangiocarcinoma",
   `Duodenal carcinoma` = "duodenal carcinoma",
   `IPMN-invasive` = "ipmn-invasive",
   `Neuroendocrine-functioning` = "neuroendocrine-functioning",
   `Neuroendocrine-nonfunctioning` = "neuroendocrine-nonfunctioning",
   `Pancreatic adenocarcinoma` = "pancreatic adenocarcinoma",
   `Other` = "other type"
   ) %>% fact(vec)
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
  c(`No oral intake by POD 14` = "yes-no oral intake by pod 14",
    `Tube to external drainage/NG tube present/reinserted` = "yes-tube to external drainage/ng tube present/reinserted"
    ) %>% fact(vec)
}

conv_pan_fistula_intervention <- function(vec) {
  c(`NPO-TPN` = "yes-clinical diagnosis, npo-tpn",
    `Drain continued >7 days` = "yes-clinical diagnosis, drain continued >7 days",
    `Percutaneous drainage` = "yes-clinical diagnosis, percutaneous drainage performed",
    `Reoperation` = "yes-clinical diagnosis, reoperation performed",
    `Spontaneous wound drainage` = "yes-clinical diagnosis, spontaneous wound drainage",
    `NPO-TPN` = "yes-persistent drainage, npo-tpn",
    `Drain continued >7 days` = "yes-persistent drainage, drain continued >7 days",
    `Percutaneous drainage` = "yes-persistent drainage, percutaneous drainage performed",
    `Reoperation` = "yes-persistent drainage, reoperation performed"
    ) %>% fact(vec)
}

conv_pan_fistula_type <- function(vec) {
  c(`Biochemical leak only` = "biochemical leak only",
    `Grade B POPF` = "yes-grade b popf present",
    `Grade C POPF` = "yes-grade c popf present",
    `Clinical diagnosis` = "yes-clinical diagnosis, npo-tpn",
    `Clinical diagnosis` = "yes-clinical diagnosis, drain continued >7 days",
    `Clinical diagnosis` = "yes-clinical diagnosis, percutaneous drainage performed",
    `Clinical diagnosis` = "yes-clinical diagnosis, reoperation performed",
    `Clinical diagnosis` = "yes-clinical diagnosis, spontaneous wound drainage",
    `Persistent drainage` = "yes-persistent drainage, npo-tpn",
    `Persistent drainage` = "yes-persistent drainage, drain continued >7 days",
    `Persistent drainage` = "yes-persistent drainage, percutaneous drainage performed",
    `Persistent drainage` = "yes-persistent drainage, reoperation performed"
    ) %>% fact(vec)
}

conv_pan_fistula <- function(vec) {
  stringr::str_detect(vec, "yes|biochemical leak only")
}

conv_pan_resection <- function(vec) {
  c(`Vein` = "vein",
    `Artery` = "artery",
    `Vein and artery` = "vein and artery",
    `Not performed` = "not performed"
    ) %>% fact(vec)
}

conv_pan_drains_type <- function(vec) {
  c(`Pancreatic anastomosis` = "pancreatic anastomosis",
    `Biliary anastomosis` = "biliary anastomosis",
    `Pancreatic and biliary anastomosis` = "pancreatic & biliary anastomosis",
    `Pancreatic parenchyma` = "pancreatic parenchyma"
    ) %>% fact(vec)
}

conv_pan_gastduo <- function(vec) {
  c(`Antecolic fashion` = "antecolic fashion",
    `Retrocolic fasion` = "retrocolic fashion",
    `Not performed` = "not performed"
    ) %>% fact(vec)
}

conv_pan_reconstruction <- function(vec) {
  c(`Pancreaticogastrostomy` = "pancreaticogastrostomy",
    `Pancreaticojejunal invagination` = "pancreaticojejunal invagination",
    `Pancreaticojejunal duct-to-mucosal` = "pancreaticojejunal duct-to-mucosal",
    `Not performed` = "not performed"
    ) %>% fact(vec)
}

conv_pan_glandtext <- function(vec) {
  c(`Soft` = "soft",
    `Intermediate` = "intermediate",
    `Hard` = "hard"
    ) %>% fact(vec)
}

conv_pan_ductsize <- function(vec) {
  c(`<3 mm` = "<3 mm",
    `3-6 mm` = "3-6 mm",
    `>6 mm` = ">6 mm"
    ) %>% fact(vec)
}

conv_pan_open_assist <- function(vec) {
  stringr::str_detect(vec, "w/ open assist|hand assisted")
}

conv_pan_unplanned_conversion <- function(vec) {
  stringr::str_detect(vec, "w/ unplanned conversion to open")
}

conv_pan_approach <- function(vec) {
  c(`Hybrid` = "hybrid",
    `Hybrid` = "hybrid w/ open assist",
    `Hybrid` = "hybrid w/ unplanned conversion to open",
    `Laparoscopic` = "laparoscopic",
    `Laparoscopic` = "laparoscopic w/ open assist",
    `Laparoscopic` = "laparoscopic w/ unplanned conversion to open",
    `Laparoscopic` = "laparoscopic hand assisted",
    `NOTES` = "notes",
    `NOTES` = "notes w/ open assist",
    `NOTES` = "notes w/ unplanned conversion to open",
    `Open` = "open",
    `Open` = "open (planned)",
    `Other` = "other",
    `Other MIS` = "other mis approach",
    `Other MIS` = "other mis approach w/ open assist",
    `Other MIS` = "other mis approach w/ unplanned conversion to open",
    `Robotic` = "robotic",
    `Robotic` = "robotic w/ open assist",
    `Robotic` = "robotic w/ unplanned conversion to open",
    `SILS` = "sils",
    `SILS` = "sils w/ open assist",
    `SILS` = "sils w/ unplanned conversion to open"
  ) %>% fact(vec)
}

conv_pan_biliarystent <- function(vec) {
  c(`No stent at time of surgery` = "no stent at time of surgery",
    `Endoscopic stent` = "endoscopic stent",
    `Percutaneous stent` = "percutaneous stent",
    `Stent of other unknown type` = "stent of other or unknown type"
    ) %>% fact(vec)
}

conv_pan_lapthor <- function(vec) { # This may potentially break in the future if additional CPT codes are added.
  c(`Laparoscopic` = "49329",
    `Open` = "48999",
    `Other` = "other"
    ) %>% fact(vec)
}



