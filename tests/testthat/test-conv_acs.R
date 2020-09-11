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
  testthat::expect_equal(sort(levels(when_dyspnea(c("at rest","moderate exertion", NA)))), sort(levels(when_dyspnea(c("at rest","moderate exertion", NA)))))
  testthat::expect_equal(as.character(when_dyspnea(c("at rest","moderate exertion", NA))), as.character(when_dyspnea(c("at rest","moderate exertion", NA))))
})

testthat::test_that("type_prsepis works", {
  testthat::expect_equal(sort(levels(type_prsepis(c("sirs","sepsis", "septic shock", NA)))), sort(levels(factor(c("SIRS","Sepsis","Septic shock",NA)))))
  testthat::expect_equal(as.character(type_prsepis(c("sirs","sepsis", "septic shock", NA))), as.character(factor(c("SIRS","Sepsis","Septic shock",NA))))
})

