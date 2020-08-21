testthat::test_that("set_up_df function works correctly", {
  set_up_df(test_df1, test_col_names) %>%
    purrr::imap(~testthat::expect_equal(.x, test_df2[[.y]]))
}) # test that test_df1 is converted to test_df2 correctly.


testthat::test_that("conv_type_cols function works correctly", {
  conv_type_cols(test_df2) %>%
    purrr::imap(~testthat::expect_equal(.x, test_df3[[.y]]))
}) # test that test_df2 is converted to test_df3 correctly.

testthat::test_that("assert_before_puf11 selects only years before 2011", {
  testthat::expect_true(assert_before_puf11(1))
  testthat::expect_true(assert_before_puf11(5))
  testthat::expect_false(assert_before_puf11(6))
})

testthat::test_that("conv_special_cols function works correctly", {
  conv_special_cols(test_df3, "acs_nsqip_puf18.txt") %>%
    purrr::imap(~testthat::expect_equal(.x, test_df4[[.y]]))
}) # test that test_df3 is converted to test_df4 correctly.




