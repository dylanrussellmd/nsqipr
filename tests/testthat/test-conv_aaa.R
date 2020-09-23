testthat::test_that("conv_aaa_iculos works", {
  testthat::expect_equal(conv_aaa_iculos(c("14","2","30 or more",NA)), c(14,2,30,NA))
})
