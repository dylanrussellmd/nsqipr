testthat::test_that("qs works", {
  x <- c("apple","banana","cherry", NA)
  testthat::expect_true(all("apple" %qsin% x, "APPLE" %qsin% x, "AppLE" %qsin% x))
  testthat::expect_true(!any("appl" %qsin% x, "APPL" %qsin% x, "AppL" %qsin% x))
  testthat::expect_identical(c("APPLE","BANANA","coconut", NA) %qsin% x, c(TRUE, TRUE, FALSE, FALSE))
})
