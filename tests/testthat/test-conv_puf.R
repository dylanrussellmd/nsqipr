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
  x <- data.table::data.table(cnscoma = c(TRUE, TRUE, FALSE), ncnscoma = c(1,2,3), dcnscoma = c(1,2,3),
                              neurodef = c(TRUE, TRUE, FALSE), nneurodef = c(1,2,3), dneurodef = c(1,2,3),
                              othgrafl = c(TRUE, TRUE, FALSE), nothgrafl = c(1,2,3), dothgrafl = c(1,2,3),
                              distraction = c("Test","test","test"))
  y <- data.table::data.table(cnscoma = c(NA, NA, NA), ncnscoma = c(NA_integer_, NA_integer_, NA_integer_), dcnscoma = c(NA_integer_, NA_integer_, NA_integer_),
                              neurodef = c(NA, NA, NA), nneurodef = c(NA_integer_, NA_integer_, NA_integer_), dneurodef = c(NA_integer_, NA_integer_, NA_integer_),
                              othgrafl = c(NA, NA, NA), nothgrafl = c(NA_integer_, NA_integer_, NA_integer_), dothgrafl = c(NA_integer_, NA_integer_, NA_integer_),
                              distraction = c("Test","test","test"))
  get_pufyear(x, "acs_nsqip_puf12.txt")
  get_pufyear(y, "acs_nsqip_puf12.txt")
  testthat::expect_equal(check_comaneurograft(x), y)

  x <- data.table::data.table(cnscoma = c(TRUE, TRUE, FALSE), ncnscoma = c(1,2,3), dcnscoma = c(1,2,3),
                              neurodef = c(TRUE, TRUE, FALSE), nneurodef = c(1,2,3), dneurodef = c(1,2,3),
                              othgrafl = c(TRUE, TRUE, FALSE), nothgrafl = c(1,2,3), dothgrafl = c(1,2,3),
                              distraction = c("Test","test","test"))
  y <- data.table::data.table(cnscoma = c(TRUE, TRUE, FALSE), ncnscoma = c(1,2,3), dcnscoma = c(1,2,3),
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
                              ethnicity_hispanic = c(TRUE, FALSE, TRUE, FALSE, NA, NA, NA, NA))
  testthat::expect_equal(conv_hispanic(x), y)

  x <- data.table::data.table(race = c("Hispanic, White", "White, Not of Hispanic Origin","White","Hispanic, Black","Black, Not of Hispanic Origin", "Black or African American",
                                       "American Indian or Alaska Native","Asian","Native Hawaiian or Pacific Islander","Asian or Pacific Islander"),
                              ethnicity_hispanic = c(NA, NA, "yes", NA, NA, NA, "yes", "yes", "no", NA))
  y <- data.table::data.table(race = c("Hispanic, White", "White, Not of Hispanic Origin","White","Hispanic, Black","Black, Not of Hispanic Origin", "Black or African American",
                                       "American Indian or Alaska Native","Asian","Native Hawaiian or Pacific Islander","Asian or Pacific Islander"),
                                     ethnicity_hispanic = c(TRUE, FALSE, TRUE, TRUE, FALSE, NA, TRUE, TRUE, FALSE, NA))
  testthat::expect_equal(conv_hispanic(x), y)
})

testthat::test_that("conv_race works", {

  # All possible inputs
  all_inputs <- c("Hispanic, White",
                  "White, Not of Hispanic Origin",
                  "White",
                  "Hispanic, Black",
                  "Black, Not of Hispanic Origin",
                  "Black or African American",
                  "American Indian or Alaska Native",
                  "Race combinations with low frequency",
                  "Some Other Race",
                  "Asian",
                  "Asian or Pacific Islander",
                  "Native Hawaiian or Pacific Islander")

  # Expected outputs for pacific = "asian"
  expected_output_asian <- factor(c("White",
                             "White",
                             "White",
                             "Black",
                             "Black",
                             "Black",
                             "American Indian or Alaska native",
                             "Race combinations with low frequency",
                             "Some other race",
                             "Asian",
                             "Asian",
                             "Native Hawaiian or Pacific islander"))

  result_asian <- conv_race(all_inputs)

  testthat::expect_equal(as.character(result_asian), as.character(expected_output_asian))
  testthat::expect_equal(sort(levels((result_asian))), sort(levels(expected_output_asian)))

  # Expected outputs for pacific = "hawaiian"
  expected_output_hawaiian <- factor(c("White",
                                "White",
                                "White",
                                "Black",
                                "Black",
                                "Black",
                                "American Indian or Alaska native",
                                "Race combinations with low frequency",
                                "Some other race",
                                "Asian",
                                "Native Hawaiian or Pacific islander",
                                "Native Hawaiian or Pacific islander"))

  result_hawaiian <- conv_race(all_inputs, pacific = "hawaiian")

  testthat::expect_equal(as.character(result_hawaiian), as.character(expected_output_hawaiian))
  testthat::expect_equal(sort(levels((result_hawaiian))), sort(levels(expected_output_hawaiian)))

})

testthat::test_that("conv_delirium works", {
  results <- nsqipr:::conv_delirium(c("Not screened for delirium", "Delirium present on screening", "No delirium present on screening", NA,
                                      "not screened for delirium", "delirium present on screening", "no delirium present on screening", NA))
  testthat::expect_equal(results, c(NA, TRUE, FALSE, NA, NA, TRUE, FALSE, NA))
})
