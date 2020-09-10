testthat::test_that("setlowernames works", {
  x <- data.table::data.table(X = NA, Y = NA, Z = NA)
  testthat::expect_equal(names(setlowernames(x)), c("x","y","z"))
})

testthat::test_that("setlower works", {
  x <- data.table::data.table(x = rep("APPLE", 10), y = rep("BANANA", 10), z = rep("CHERRY", 10))
  testthat::expect_identical(setlower(x), data.table::data.table(x = rep("apple", 10), y = rep("banana", 10), z = rep("cherry", 10)))
})

testthat::test_that("setna works", {
  x <- data.table::data.table(x = rep("unknown", 10), y = rep("unknown/not reported", 10), z = rep("null", 10),
                              xx = rep("not documented", 10), yy = rep("none/not documented", 10), zz = rep("not entered", 10),
                              xxx = rep("-99", 10), yyy = rep("test", 10))
  testthat::expect_identical(setna(x, c("unknown", "unknown/not reported", "null", "n/a", "not documented", "none/not documented", "not entered","-99")),
                             data.table::data.table(x = rep(NA_character_, 10), y = rep(NA_character_, 10), z = rep(NA_character_, 10),
                                                    xx = rep(NA_character_, 10), yy = rep(NA_character_), zz = rep(NA_character_),
                                                    xxx = rep(NA_character_, 10), yyy = rep("test", 10)))
})

testthat::test_that("conv_yesno works", {
  x <- data.frame(x = rep("yes", 10), y = rep("YES", 10), z = rep("no", 10), xx = rep("NO", 10), yy = rep(NA, 10))
  testthat::expect_true(all(conv_yesno(x$x)))
  testthat::expect_true(all(conv_yesno(x$y)))
  testthat::expect_true(all(!conv_yesno(x$z)))
  testthat::expect_true(all(!conv_yesno(x$xx)))
  testthat::expect_true(all(is.na(conv_yesno(x$yy))))
})

testthat::test_that("conv_notno works", {
  x <- data.frame(x = rep("no", 10), y = rep("NO", 10), z = rep("yes", 10), xx = rep("", 10), yy = rep(NA, 10))
  testthat::expect_true(all(!conv_notno(x$x)))
  testthat::expect_true(all(!conv_notno(x$y)))
  testthat::expect_true(all(conv_notno(x$z)))
  testthat::expect_true(all(conv_notno(x$xx)))
  testthat::expect_true(all(is.na(conv_notno(x$yy))))
})

testthat::test_that("conv_complication works", {
  x <- data.frame(x = rep("no complication", 10), y = rep("NO COMPLICATION", 10), z = rep("complication", 10), xx = rep("", 10), yy = rep(NA, 10))
  testthat::expect_true(all(!conv_complication(x$x)))
  testthat::expect_true(all(!conv_complication(x$y)))
  testthat::expect_true(all(conv_complication(x$z)))
  testthat::expect_true(all(conv_complication(x$xx)))
  testthat::expect_true(all(is.na(conv_complication(x$yy))))
})

testthat::test_that("conv_numscale works", {
  x <- data.frame(x = rep("1 - test", 10), y = rep("2- test", 10), z = rep("  3 test", 10), xx = rep("4            5", 10), yy = rep(NA, 10))
  testthat::expect_equal(conv_numscale(x$x), rep(1, 10))
  testthat::expect_equal(conv_numscale(x$y), rep(2, 10))
  testthat::expect_equal(conv_numscale(x$z), rep(3, 10))
  testthat::expect_equal(conv_numscale(x$xx), rep(4, 10))
  testthat::expect_equal(conv_numscale(x$yy), rep(NA_integer_, 10))
})

testthat::test_that("conv_date works", {
  testthat::expect_equal(conv_date("2000"), as.Date("2000", "%Y"))
})
