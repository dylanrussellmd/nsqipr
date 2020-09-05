load(file.path("test_dataframe.rda"))

test_set <- function(name) {
  testthat::test_that(paste("set_up_df function works correctly for",name), {
    set_up_df(df(name, 1)) %>%
      purrr::imap(~testthat::expect_equal(.x, df(name, 2)[[.y]]))
  })


  testthat::test_that(paste("conv_type_cols function works correctly for",name), {
    conv_type_cols(df(name, 2)) %>%
      purrr::imap(~testthat::expect_equal(.x, df(name, 3)[[.y]]))
  })

  testthat::test_that(paste("conv_special_cols function works correctly for",name), {
    conv_special_cols(df(name, 3), name) %>%
      purrr::imap(~testthat::expect_equal(.x, df(name,4)[[.y]]))
  })
}

df <- function(name, end) {
  get(paste(name, end, sep = "_"))
}

