testthat::test_that("conv_sex works", {
  testthat::expect_equal(conv_sex(c("male","MALE","female","FEMALE",NA)), c(TRUE, TRUE, FALSE, FALSE, NA))
})

testthat::test_that("conv_age works", {
  testthat::expect_equal(conv_age(c("18","45","90+",NA)), c(18,45,90,NA))
})

testthat::test_that("insulin works", {
  testthat::expect_equal(insulin(c("NO","NON-INSULIN","INSULIN", "ORAL", NA)), c(NA, FALSE, TRUE,FALSE,NA))
  testthat::expect_equal(insulin(c("no","non-insulin","insulin", "oral", NA)), c(NA, FALSE, TRUE,FALSE,NA))
})

testthat::test_that("when_dyspnea works", {
  testthat::expect_equal(sort(levels(when_dyspnea(c("AT REST","MODERATE EXERTION", NA)))), sort(levels(factor(c("At rest","Moderate exertion", NA)))))
  testthat::expect_equal(as.character(when_dyspnea(c("AT REST","MODERATE EXERTION", NA))), as.character(factor(c("At rest","Moderate exertion", NA))))
})

testthat::test_that("type_prsepis works", {
  testthat::expect_equal(sort(levels(type_prsepis(c("SIRS","Sepsis", "Septic Shock", NA)))), sort(levels(factor(c("SIRS","Sepsis","Septic shock",NA)))))
  testthat::expect_equal(as.character(type_prsepis(c("SIRS","Sepsis", "Septic Shock", NA))), as.character(factor(c("SIRS","Sepsis","Septic shock",NA))))
})

testthat::test_that("check_comaneurograft works", {
  x <- data.table::data.table(coma = c(TRUE, TRUE, FALSE), cnscoma = c(TRUE, TRUE, FALSE), ncnscoma = c(1,2,3), dcnscoma = c(1,2,3),
                              neurodef = c(TRUE, TRUE, FALSE), nneurodef = c(1,2,3), dneurodef = c(1,2,3),
                              othgrafl = c(TRUE, TRUE, FALSE), nothgrafl = c(1,2,3), dothgrafl = c(1,2,3),
                              distraction = c("Test","test","test"))
  y <- data.table::data.table(coma = c(NA, NA, NA), cnscoma = c(NA, NA, NA), ncnscoma = c(NA_integer_, NA_integer_, NA_integer_), dcnscoma = c(NA_integer_, NA_integer_, NA_integer_),
                              neurodef = c(NA, NA, NA), nneurodef = c(NA_integer_, NA_integer_, NA_integer_), dneurodef = c(NA_integer_, NA_integer_, NA_integer_),
                              othgrafl = c(NA, NA, NA), nothgrafl = c(NA_integer_, NA_integer_, NA_integer_), dothgrafl = c(NA_integer_, NA_integer_, NA_integer_),
                              distraction = c("Test","test","test"))
  get_pufyear(x, "acs_nsqip_puf12.txt")
  get_pufyear(y, "acs_nsqip_puf12.txt")
  testthat::expect_equal(check_comaneurograft(x), y)

  x <- data.table::data.table(coma = c(TRUE, TRUE, FALSE), cnscoma = c(TRUE, TRUE, FALSE), ncnscoma = c(1,2,3), dcnscoma = c(1,2,3),
                              neurodef = c(TRUE, TRUE, FALSE), nneurodef = c(1,2,3), dneurodef = c(1,2,3),
                              othgrafl = c(TRUE, TRUE, FALSE), nothgrafl = c(1,2,3), dothgrafl = c(1,2,3),
                              distraction = c("Test","test","test"))
  y <- data.table::data.table(coma = c(TRUE, TRUE, FALSE), cnscoma = c(TRUE, TRUE, FALSE), ncnscoma = c(1,2,3), dcnscoma = c(1,2,3),
                              neurodef = c(TRUE, TRUE, FALSE), nneurodef = c(1,2,3), dneurodef = c(1,2,3),
                              othgrafl = c(TRUE, TRUE, FALSE), nothgrafl = c(1,2,3), dothgrafl = c(1,2,3),
                              distraction = c("Test","test","test"))
  get_pufyear(x, "acs_nsqip_puf10.txt")
  get_pufyear(y, "acs_nsqip_puf10.txt")
  testthat::expect_equal(check_comaneurograft(x), y)
})

testthat::test_that("conv_hispanic and conv_hispanic_helper work", {
  x <- data.table::data.table(race = c("Hispanic, White", "White, Not of Hispanic Origin","Hispanic, Black","Black, Not of Hispanic Origin",
  "American Indian or Alaska Native","Asian","Native Hawaiian or Pacific Islander","Asian or Pacific Islander"))
  y <- data.table::data.table(race = c("Hispanic, White", "White, Not of Hispanic Origin","Hispanic, Black","Black, Not of Hispanic Origin",
                                       "American Indian or Alaska Native","Asian","Native Hawaiian or Pacific Islander","Asian or Pacific Islander"),
                              ethnicity_hispanic = c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  testthat::expect_equal(conv_hispanic(x), y)

  x <- data.table::data.table(race = c("Hispanic, White", "White, Not of Hispanic Origin","White","Hispanic, Black","Black, Not of Hispanic Origin", "Black or African American",
                                       "American Indian or Alaska Native","Asian","Native Hawaiian or Pacific Islander","Asian or Pacific Islander"),
                              ethnicity_hispanic = c(NA, NA, "yes", NA, NA, NA, NA, NA, NA, NA))
  y <- data.table::data.table(race = c("Hispanic, White", "White, Not of Hispanic Origin","White","Hispanic, Black","Black, Not of Hispanic Origin", "Black or African American",
                                       "American Indian or Alaska Native","Asian","Native Hawaiian or Pacific Islander","Asian or Pacific Islander"),
                                     ethnicity_hispanic = c(TRUE, FALSE, TRUE, TRUE, FALSE, NA, FALSE, FALSE, FALSE, FALSE))
  testthat::expect_equal(conv_hispanic(x), y)
})

testthat::test_that("conv_race works", {
  x <- c("White","Black or African American","Asian or Pacific Islander")
  testthat::expect_equal(sort(levels(conv_race(x, pacific = "hawaiian"))), sort(levels(factor(c("White","Black","Native Hawaiian or Pacific islander","American Indian or Alaska native","Asian")))))
  testthat::expect_equal(as.character(conv_race(x, pacific = "hawaiian")), as.character(factor(c("White","Black","Native Hawaiian or Pacific islander"))))
  testthat::expect_equal(sort(levels(conv_race(x))), sort(levels(factor(c("White","Black","Native Hawaiian or Pacific islander","American Indian or Alaska native","Asian")))))
  testthat::expect_equal(as.character(conv_race(x)), as.character(factor(c("White","Black","Asian"))))
})

testthat::test_that("make_readm_cols works", {
  x <- data.table::data.table(readmission1 = TRUE)
  y <- c("readmission1", "readmission2", "readmission3", "readmission4", "readmission5", "readmpodays1", "readmpodays2", "readmpodays3", "readmpodays4", "readmpodays5", "readmrelated1", "readmrelated2", "readmrelated3", "readmrelated4", "readmrelated5", "readmsuspreason1", "readmsuspreason2", "readmsuspreason3", "readmsuspreason4", "readmsuspreason5", "readmrelicd91", "readmrelicd92", "readmrelicd93", "readmrelicd94", "readmrelicd95", "readmrelicd101", "readmrelicd102", "readmrelicd103", "readmrelicd104", "readmrelicd105", "unplannedreadmission1", "unplannedreadmission2", "unplannedreadmission3", "unplannedreadmission4", "unplannedreadmission5", "readmunrelsusp1",
"readmunrelsusp2",  "readmunrelsusp3",  "readmunrelsusp4",  "readmunrelsusp5",  "readmunrelicd91",  "readmunrelicd92",  "readmunrelicd93",  "readmunrelicd94",  "readmunrelicd95", "readmunrelicd101", "readmunrelicd102", "readmunrelicd103", "readmunrelicd104", "readmunrelicd105")
  make_readm_cols(x)
  testthat::expect_equal(names(x), y)
})

testthat::test_that("make_reop_cols works", {
  x <- data.table::data.table(reoperation1 = TRUE)
  y <- c("reoperation1", "reoperation2", "reoperation3", "retorpodays",  "retor2podays", "retor3podays", "reoporcpt1", "reopor2cpt1",  "reopor3cpt1",  "retorrelated", "retor2related", "retor3related", "reoporicd91",  "reopor2icd91", "reopor3icd91",  "reopor1icd101", "reopor2icd101", "reopor3icd101")
  make_reop_cols(x)
  testthat::expect_equal(names(x), y)
})

testthat::test_that("make_anesthes_other_cols works", {
  x <- data.table::data.table(anesthes_other = c("General","General, Spinal", "General, Spinal, MAC/IV Sedation", NA))
  y <- data.table::data.table(anesthes_other = c("General", "General, Spinal", "General, Spinal, MAC/IV Sedation", NA), anesthes_other1 = c("General", "General", "General", NA), anesthes_other2 = c(NA, "Spinal", "Spinal", NA), anesthes_other3 = c(NA, NA, "MAC/IV Sedation", NA), anesthes_other4 = c(NA_character_, NA_character_, NA_character_, NA_character_), anesthes_other5 = c(NA_character_, NA_character_, NA_character_, NA_character_), anesthes_other6 = c(NA_character_, NA_character_, NA_character_, NA_character_), anesthes_other7 = c(NA_character_, NA_character_, NA_character_, NA_character_), anesthes_other8 = c(NA_character_, NA_character_, NA_character_, NA_character_))
  make_anesthes_other_cols(x)
  testthat::expect_equal(x, y)
})



