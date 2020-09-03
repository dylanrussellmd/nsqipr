testthat::test_that("checkTrue assigns TRUE correctly", {
  testthat::expect_true(checkTrue(TRUE))
  testthat::expect_false(checkTrue(NA))
  testthat::expect_false(checkTrue(FALSE))
})

testthat::test_that("checkAnyTrue assigns TRUE correctly", {
  testthat::expect_equal(checkAnyTrue(c(TRUE, FALSE, TRUE, FALSE), c(FALSE, FALSE, NA, NA )), c(TRUE, FALSE, TRUE, FALSE))
})

testthat::test_that("checkTrue and checkAnyTrue are equal if given a single vector", {
  testthat::expect_equal(checkTrue(c(TRUE, TRUE, TRUE, FALSE)), checkAnyTrue(c(TRUE, TRUE, TRUE, FALSE)))
})

testthat::test_that("isDead assigns TRUE correctly", {
  testthat::expect_true(isDead("dead"))
  testthat::expect_false(isDead(NA))
})

testthat::test_that("checkAnyDead assigns TRUE correctly", {
  testthat::expect_equal(checkAnyDead(c("dead", NA, NA), c(NA, "dead", NA)), c(TRUE, TRUE, FALSE))
})
