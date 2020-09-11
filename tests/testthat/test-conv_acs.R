testthat::test_that("conv_sex works", {
  testthat::expect_equal(conv_sex(c("male","MALE","female","FEMALE",NA)), c(TRUE, TRUE, FALSE, FALSE, NA))
})

testthat::test_that("conv_age works", {
  testthat::expect_equal(conv_age(c("18","45","90+",NA)), c(18,45,90,NA))
})

testthat::test_that("insulin works", {
  testthat::expect_equal(insulin(c("no","non-insulin","insulin",NA)), c(FALSE, FALSE, TRUE ,NA))
})

testthat::test_that("when_dyspnea works", {
  testthat::expect_equal(sort(levels(when_dyspnea(c("at rest","moderate exertion", NA)))), sort(levels(factor(c("At rest","Moderate exertion", NA)))))
  testthat::expect_equal(as.character(when_dyspnea(c("at rest","moderate exertion", NA))), as.character(factor(c("At rest","Moderate exertion", NA))))
})

testthat::test_that("type_prsepis works", {
  testthat::expect_equal(sort(levels(type_prsepis(c("sirs","sepsis", "septic shock", NA)))), sort(levels(factor(c("SIRS","Sepsis","Septic shock",NA)))))
  testthat::expect_equal(as.character(type_prsepis(c("sirs","sepsis", "septic shock", NA))), as.character(factor(c("SIRS","Sepsis","Septic shock",NA))))
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
  conv_pufyear(x, "acs_nsqip_puf12.txt")
  conv_pufyear(y, "acs_nsqip_puf12.txt")
  testthat::expect_equal(check_comaneurograft(x), y)

  x <- data.table::data.table(coma = c(TRUE, TRUE, FALSE), cnscoma = c(TRUE, TRUE, FALSE), ncnscoma = c(1,2,3), dcnscoma = c(1,2,3),
                              neurodef = c(TRUE, TRUE, FALSE), nneurodef = c(1,2,3), dneurodef = c(1,2,3),
                              othgrafl = c(TRUE, TRUE, FALSE), nothgrafl = c(1,2,3), dothgrafl = c(1,2,3),
                              distraction = c("Test","test","test"))
  y <- data.table::data.table(coma = c(TRUE, TRUE, FALSE), cnscoma = c(TRUE, TRUE, FALSE), ncnscoma = c(1,2,3), dcnscoma = c(1,2,3),
                              neurodef = c(TRUE, TRUE, FALSE), nneurodef = c(1,2,3), dneurodef = c(1,2,3),
                              othgrafl = c(TRUE, TRUE, FALSE), nothgrafl = c(1,2,3), dothgrafl = c(1,2,3),
                              distraction = c("Test","test","test"))
  conv_pufyear(x, "acs_nsqip_puf10.txt")
  conv_pufyear(y, "acs_nsqip_puf10.txt")
  testthat::expect_equal(check_comaneurograft(x), y)
})

testthat::test_that("conv_hispanic and conv_hispanic_helper work", {
  x <- data.table::data.table(race = c("hispanic, white", "white, not of hispanic origin","hispanic, black","black, not of hispanic origin",
  "american indian or alaska native","asian","native hawaiian or pacific islander","asian or pacific islander"))
  y <- data.table::data.table(race = c("hispanic, white", "white, not of hispanic origin","hispanic, black","black, not of hispanic origin",
                                       "american indian or alaska native","asian","native hawaiian or pacific islander","asian or pacific islander"),
                              ethnicity_hispanic = c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  testthat::expect_equal(conv_hispanic(x), y)

  x <- data.table::data.table(race = c("hispanic, white", "white, not of hispanic origin","white","hispanic, black", "black, not of hispanic origin","black or african american",
                                       "american indian or alaska native","asian","native hawaiian or pacific islander","asian or pacific islander"),
                              ethnicity_hispanic = c(NA, NA, "yes", NA, NA, NA, NA, NA, NA, NA))
  y <- data.table::data.table(race = c("hispanic, white", "white, not of hispanic origin","white","hispanic, black","black, not of hispanic origin","black or african american",
                                              "american indian or alaska native","asian","native hawaiian or pacific islander","asian or pacific islander"),
                                     ethnicity_hispanic = c(TRUE, FALSE, TRUE, TRUE, FALSE, NA, FALSE, FALSE, FALSE, FALSE))
  testthat::expect_equal(conv_hispanic(x), y)
})

testthat::test_that("conv_race works", {
  x <- c("white","black or african american","asian or pacific islander")
  testthat::expect_equal(sort(levels(conv_race(x, pacific = "hawaiian"))), sort(levels(factor(c("White","Black","Native Hawaiian or Pacific islander","American Indian or Alaska native","Asian")))))
  testthat::expect_equal(as.character(conv_race(x, pacific = "hawaiian")), as.character(factor(c("White","Black","Native Hawaiian or Pacific islander"))))
  testthat::expect_equal(sort(levels(conv_race(x))), sort(levels(factor(c("White","Black","Native Hawaiian or Pacific islander","American Indian or Alaska native","Asian")))))
  testthat::expect_equal(as.character(conv_race(x)), as.character(factor(c("White","Black","Asian"))))
})
