testthat::test_that("addmissingcolumns works", {
  x <- data.table::data.table(a = c(1,2,3))
  cols <- c("a","b","c")
  addmissingcolumns(x, cols)
  testthat::expect_equal(x[["b"]], c(rep(NA_character_, 3)))
  testthat::expect_equal(x[["c"]], c(rep(NA_character_, 3)))
})

testthat::test_that("setlowernames works", {
  x <- data.table::data.table(X = NA, Y = NA, Z = NA)
  testthat::expect_equal(names(setlowernames(x)), c("x","y","z"))
})

testthat::test_that("setlowernames is not creating copies", {
  x <- data.table::data.table(X = NA, Y = NA, Z = NA)
  tracemem(x)
  testthat::expect_identical(testthat::capture_output(setlowernames(x)), "")
  untracemem(x)
})

testthat::test_that("conv_ works", {
  x <- data.table::data.table(x = rep("APPLE", 10), y = rep("BANANA", 10), z = rep("CHERRY", 10))
  y <- data.table::data.table(x = rep("apple", 10), y = rep("banana", 10), z = rep("CHERRY", 10))
  z <- data.table::data.table(x = rep("APPLE", 10), y = rep("BANANA", 10), z = rep("CHERRY", 10), X = rep("apple", 10), Y = rep("banana", 10))
  zz <- data.table::data.table(x = rep("APPLE", 10), y = rep("BANANA", 10), z = rep("CHERRY", 10), drink = rep("APPLE JUICE", 10))
  testthat::expect_identical(conv_(x, c("a","b"), tolower), x)
  testthat::expect_identical(conv_(x, c("x","y"), tolower), y)
  x <- data.table::data.table(x = rep("APPLE", 10), y = rep("BANANA", 10), z = rep("CHERRY", 10))
  testthat::expect_identical(conv_(x, c("x","y"), tolower, newcol = c("X","Y")), z)
  x <- data.table::data.table(x = rep("APPLE", 10), y = rep("BANANA", 10), z = rep("CHERRY", 10))
  testthat::expect_identical(conv_(x, "x", paste, "JUICE", sep = " ", newcol = "drink"), zz)
})

testthat::test_that("conv_ is not creating copies", {
  x <- data.table::data.table(x = rep("APPLE", 10), y = rep("BANANA", 10), z = rep("CHERRY", 10))
  tracemem(x)
  testthat::expect_identical(testthat::capture_output(conv_(x, c("x","y"), tolower)), "")
  untracemem(x)
  x <- data.table::data.table(x = rep("APPLE", 10), y = rep("BANANA", 10), z = rep("CHERRY", 10))
  tracemem(x)
  testthat::expect_identical(testthat::capture_output(conv_(x, c("x","y"), tolower, newcol = c("X","Y"))), "")
  untracemem(x)
  x <- data.table::data.table(x = rep("APPLE", 10), y = rep("BANANA", 10), z = rep("CHERRY", 10))
  tracemem(x)
  testthat::expect_identical(testthat::capture_output(conv_(x, "x", paste, "JUICE", sep = " ", newcol = "drink")), "")
  untracemem(x)
})

testthat::test_that("conv_yesno works", {
  x <- data.table::data.table(x = rep("yes", 10), y = rep("YES", 10), z = rep("no", 10), xx = rep("NO", 10), yy = rep(NA, 10))
  testthat::expect_true(all(conv_yesno(x$x)))
  testthat::expect_true(all(conv_yesno(x$y)))
  testthat::expect_true(all(!conv_yesno(x$z)))
  testthat::expect_true(all(!conv_yesno(x$xx)))
  testthat::expect_true(all(is.na(conv_yesno(x$yy))))
})

testthat::test_that("conv_notno works", {
  x <- data.table::data.table(x = rep("no", 10), y = rep("NO", 10), z = rep("yes", 10), xx = rep("", 10), yy = rep(NA, 10), zz = rep("NONE", 10), a = rep("NON-INSULIN", 10))
  testthat::expect_true(all(!conv_notno(x$x)))
  testthat::expect_true(all(!conv_notno(x$y)))
  testthat::expect_true(all(!conv_notno(x$zz)))
  testthat::expect_true(all(conv_notno(x$z)))
  testthat::expect_true(all(conv_notno(x$xx)))
  testthat::expect_true(all(conv_notno(x$a)))
  testthat::expect_true(all(is.na(conv_notno(x$yy))))
})

testthat::test_that("conv_complication works", {
  x <- data.table::data.table(x = rep("no complication", 10), y = rep("NO COMPLICATION", 10), z = rep("complication", 10), xx = rep("", 10), yy = rep(NA, 10))
  testthat::expect_true(all(!conv_complication(x$x)))
  testthat::expect_true(all(!conv_complication(x$y)))
  testthat::expect_true(all(conv_complication(x$z)))
  testthat::expect_true(all(conv_complication(x$xx)))
  testthat::expect_true(all(is.na(conv_complication(x$yy))))
})

testthat::test_that("conv_numscale works", {
  x <- data.table::data.table(x = rep("1 - test", 10), y = rep("2- test", 10), z = rep("  3 test", 10), xx = rep("4            5", 10), yy = rep(NA, 10))
  testthat::expect_equal(conv_numscale(x$x), rep(1, 10))
  testthat::expect_equal(conv_numscale(x$y), rep(2, 10))
  testthat::expect_equal(conv_numscale(x$z), rep(3, 10))
  testthat::expect_equal(conv_numscale(x$xx), rep(4, 10))
  testthat::expect_equal(conv_numscale(x$yy), rep(NA_integer_, 10))
})

testthat::test_that("conv_date works", {
  testthat::expect_equal(conv_date("2000"), as.Date("2000-1-1", "%Y-%m-%d"))
})

testthat::test_that("get_pufyear works", {
  x <- data.table::data.table(x = rep("name", 10))
  get_pufyear(x, "acs_nsqip_puf12.txt")
  testthat::expect_true(all(x[["pufyear"]] > "2010"))
  testthat::expect_true(all(x[["pufyear"]] <= "2012"))
  testthat::expect_true(all(x[["pufyear"]] >= "2012"))
  testthat::expect_true(all(x[["pufyear"]] < "2018"))
})

testthat::test_that("factorpipe works", {
  x <- c("apple","cherry", "pork  butt")
  levels <- list(fruit = c("apple", "banana", "cherry"), meat = c("steak", "chicken", "pork butt"))
  testthat::expect_equal(x %^% levels, factor(c("fruit","fruit","meat")))
})

testthat::test_that("conv_factor works", {
  x <- data.table::data.table(foods = c("apple","banana","cherry","steak","chicken","pork"), drinks = c("milk","water","oj","beer","vodka","rum"))
  foods <- list(fruit = c("apple", "banana", "cherry"), meat = c("steak","chicken","pork"))
  drinks <- list(alcoholic = c("beer","vodka","rum"), `non-alcoholic` = c("milk","water","oj"))
  factor_cols <- c("foods", "drinks")
  y <- data.table::data.table(foods = factor(c("fruit","fruit","fruit","meat","meat","meat")),
                              drinks = factor(c("non-alcoholic", "non-alcoholic", "non-alcoholic", "alcoholic", "alcoholic", "alcoholic")))
  testthat::expect_identical(conv_factor(x, factor_cols), y)
})

testthat::test_that("conv_factor is not creating copies", {
  x <- data.table::data.table(foods = c("apple","banana","cherry","steak","chicken","pork"), drinks = c("milk","water","oj","beer","vodka","rum"))
  foods <- list(fruit = c("apple", "banana", "cherry"), meat = c("steak","chicken","pork"))
  drinks <- list(alcoholic = c("beer","vodka","rum"), `non-alcoholic` = c("milk","water","oj"))
  factor_cols <- c("foods", "drinks")
  tracemem(x)
  testthat::expect_identical(testthat::capture_output(conv_factor(x, factor_cols)), "")
  untracemem(x)
})

testthat::test_that("colorder works", {
  x <- data.table::data.table(c = c(1,2,3), b = c(1,2,3), a = c(1,2,3))
  col_order <- c("a","b","c")
  colorder(x, col_order)
  testthat::expect_equal(names(x), col_order)
})

testthat::test_that("colorder is not creating copies", {
  x <- data.table::data.table(c = c(1,2,3), b = c(1,2,3), a = c(1,2,3))
  col_order <- c("a","b","c")
  tracemem(x)
  testthat::expect_identical(testthat::capture_output(colorder(x, col_order)), "")
  untracemem(x)
})

testthat::test_that("remove_undesired works", {
  x <- data.table::data.table(a = c(1,2,3), b = c(1,2,3), c = c(1,2,3))
  undesired_cols <- c("a","b","d")
  remove_undesired(x, undesired_cols)
  testthat::expect_equal(names(x), "c")
})

testthat::test_that("remove_undesired is not creating copies", {
  x <- data.table::data.table(a = c(1,2,3), b = c(1,2,3), c = c(1,2,3))
  undesired_cols <- c("a","b","d")
  tracemem(x)
  testthat::expect_identical(testthat::capture_output(remove_undesired(x, undesired_cols)), "")
  untracemem(x)
})

testthat::test_that("coalesce_cols and coalesce works", {
   x <- data.table::data.table(a = c(NA, TRUE, FALSE), b = c(NA, TRUE, FALSE), c = c(NA, TRUE, FALSE), d = c(NA, TRUE, FALSE), A = c(TRUE, NA, TRUE), B = c(TRUE, NA, TRUE))
   coalesce_in_cols <- c("A","B","C","D")
   coalesce_out_cols <- c("a","b","c","d")
   coalesce_cols(x, coalesce_in_cols, coalesce_out_cols)
   testthat::expect_equal(names(x), c("a","b","C","D","A","B"))
   testthat::expect_true(all(x[["A"]]) & all(x[["B"]]))
   testthat::expect_equal(x[["a"]], c(NA, TRUE, FALSE))
   testthat::expect_equal(x[["b"]], c(NA, TRUE, FALSE))
   testthat::expect_equal(x[["C"]], c(NA, TRUE, FALSE))
   testthat::expect_equal(x[["D"]], c(NA, TRUE, FALSE))
})
