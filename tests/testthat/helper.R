test_goldstandard <- function(folder, file) {
  file <- paste(file, ".rds", sep = "")
  goldstandard <- readRDS(file.path("test-data", file))
  test <- readRDS(file.path("test-data", folder,"rds", file))
  testthat::expect_identical(goldstandard, test)
}
