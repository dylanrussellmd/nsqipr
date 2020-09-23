testthat::test_that("conv_col_anastomotic works", {
  test <- c("No", "Yes-reoperation", "Yes-percutaneous intervention", "Yes-no intervention required", "No definitive diagnosis of leak/leak related abscess", "Leak, treated w/ reoperation", "Leak, treated w/ interventional means", "Leak, no treatment intervention documented", "Leak, treated w/ non-interventional/non-operative means", NA)
  testthat::expect_equal(conv_col_anastomotic(test), c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, NA))
})

testthat::test_that("conv_col_leak_treatment works", {
  test <- c("No", "Yes-reoperation", "Yes-percutaneous intervention", "Yes-no intervention required", "No definitive diagnosis of leak/leak related abscess", "Leak, treated w/ reoperation", "Leak, treated w/ interventional means", "Leak, no treatment intervention documented", "Leak, treated w/ non-interventional/non-operative means", NA)
  testthat::expect_equal(sort(levels(conv_col_leak_treatment(test))), sort(levels(factor(c("Reoperation","Percutaneous intervention","No intervention","Reoperation","Percutaneous intervention","No intervention","Non-operative intervention")))))
  testthat::expect_equal(sort(unique(as.character(na.omit(conv_col_leak_treatment(test))))), sort(as.character(factor(c("Reoperation","Percutaneous intervention","No intervention","Non-operative intervention")))))
})


