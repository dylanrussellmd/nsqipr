#' Test against a gold standard file
#'
#' This function compares a gold standard clean data set (already loaded in the
#' `test-data` folder) and compares it to a provided file. This should be a post-
#' cleaning file.
#'
#' @param folder the folder in which to find the test file (post-cleaning). This
#' should correspond to the name of the data set (i.e. acs_nsqip_puf, puf_tar_col,
#' etc.)
#'
#' @param file the name of the file (should match between gold standard and test
#' file). This should correspond to the name of the data set (i.e. acs_nsqip_puf,
#' puf_tar_col, etc.)
#'
#' @return a testthat test (invisible if passed)
#'
test_goldstandard <- function(folder, file) {
  file <- paste(file, ".rds", sep = "")
  goldstandard <- readRDS(file.path("test-data", file))
  test <- readRDS(file.path("test-data", folder, "rds", file))
  testthat::expect_identical(goldstandard, test)
}

#' Test a function converts to a factor vector with appropriate levels
#'
#' @param func the conversion function to be tested
#' @param test the character vector to be converted as a test
#' @param gs the gold standard levels, passed as a character vector
#'
#' @return a testthat test (invisible if passed)
#'
#' @details The order of the vectors passed do not matter as the function will
#' sort the resultant levels alphabetically.
#'
test_levels <- function(func, test, gs) {
  output_levels <- sort(levels(func(test)))
  gs_levels <- sort(levels(factor(gs)))
  testthat::expect_equal(output_levels, gs_levels)
}
