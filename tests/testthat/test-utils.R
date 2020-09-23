testthat::test_that("qs works", {
  x <- c("apple","banana","cherry", NA)
  testthat::expect_true(all("apple" %qsin% x, "APPLE" %qsin% x, "AppLE" %qsin% x))
  testthat::expect_true(!any("appl" %qsin% x, "APPL" %qsin% x, "AppL" %qsin% x))
  testthat::expect_identical(c("APPLE","BANANA","coconut", NA) %qsin% x, c(TRUE, TRUE, FALSE, FALSE))
})

testthat::test_that("isFullDT works", {
 testthat::expect_false(isFullDT(NULL))
 testthat::expect_false(isFullDT("test"))
 testthat::expect_false(isFullDT(NA))
 testthat::expect_false(isFullDT(data.table::data.table()))
 testthat::expect_false(isFullDT(data.table::data.table(test = numeric(), test2 = logical(), test3 = character())))
 testthat::expect_true(isFullDT(data.table::data.table(test = 1, test2 = TRUE, test3 = "character")))
})
