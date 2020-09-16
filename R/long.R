reoperations <- paste("reoperation", 1:3, sep = "")
retorpodays <- c("retorpodays","retor2podays", "retor3podays")
reoporcpt1 <- c("reoporcpt1","reopor2cpt1","reopor3cpt1")
retorrelated <- c("retorrelated","retor2related","retor3related")
reoporicd91 <- c("reoporicd91","reopor2icd91","reopor3icd91")
reoporicd10 <- c("reopor1icd101", "reopor2icd101", "reopor3icd101")

x$retor2related <- as.logical(x$retor2related)

x <- x[, "retor3podays" := NA_integer_]
x <- x[, c("reopor3cpt1", "reopor3icd91", "reopor3icd101") := NA_character_]
x <- x[, "retor3related" := NA]

y <- data.table::melt(x, id.vars = "caseid", measure.vars = list(reoperations, retorpodays, reoporcpt1, retorrelated, reoporicd91, reoporicd10),
                      variable.name = "nreoperation", value.name = c("reoperation", "retorpodays", "reoporcpt1", "retorrelated", "reoporicd91", "reoporicd10"))
y <- na.omit(y, cols = "reoperation")



readmission <- paste("readmission", 1:5, sep = "")
readmpodays <- paste("readmpodays", 1:5, sep = "")
readmrelated <- paste("readmrelated", 1:5, sep = "")
readmsuspreason <- paste("readmsuspreason", 1:5, sep = "")
readmrelicd9 <- paste("readmrelicd9", 1:5, sep = "")
readmrelicd10 <- paste("readmrelicd10", 1:5, sep = "")
unplannedreadmission <- paste("unplannedreadmission", 1:5, sep = "")
readmunrelsusp <- paste("readmunrelsusp", 1:5, sep = "")
readmunrelicd9 <- paste("readmunrelicd9", 1:5, sep = "")
readmunrelicd10 <- paste("readmunrelicd10", 1:5, sep = "")


z <- data.table::melt(x, id.vars = "caseid", measure.vars = list(readmission, readmpodays, readmrelated, readmsuspreason, readmrelicd9, readmrelicd10, unplannedreadmission, readmunrelsusp, readmunrelicd9, readmunrelicd10), variable.name = "nreadmission", value.name = c("readmission","readmpodays","readmrelated","readmsuspreason","readmrelicd9","readmrelicd10","unplannedreadmission","readmunrelsusp","readmunrelicd9",'readmunrelicd10'))
z <- na.omit(z, cols = "readmission")

anesthes_other = paste("anesthes_other", 1:8, sep = "")
y <- data.table::as.data.table(cbind(x[["CaseID"]],stringi::stri_split_fixed(x$ANESTHES_OTHER, ",", simplify = TRUE, n = 8, omit_empty = NA)))
data.table::setnames(y, names(y), c("caseid", anesthes_other))
y <- data.table::melt(y, id.vars = "caseid", measure.vars = anesthes_other, variable.name = "nanesthes_other", value.name = "anesthes_other", na.rm = TRUE)
