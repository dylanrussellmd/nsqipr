testthat::test_that("make_reop_long works", {
  x <- data.table::data.table(caseid = c(1,2,3,4),
                              reoperation1 = c(TRUE, TRUE, FALSE, NA),
                              retorpodays = c(10, 7, NA, NA),
                              reoporcpt1 = c("44005", "37211", NA, NA),
                              retorrelated = c(TRUE, TRUE, NA, NA),
                              reoporicd91 = c("K56.69","T82.868A", NA, NA),
                              reopor1icd101 = c("K56.59", "T82.868A", NA, NA),
                              reoperation2 = c(TRUE, TRUE, FALSE, NA),
                              retor2podays = c(10, 7, NA, NA),
                              reopor2cpt1 = c("44005", "37211", NA, NA),
                              retor2related = c(TRUE, TRUE, NA, NA),
                              reopor2icd91 = c("K56.69","T82.868A", NA, NA),
                              reopor2icd101 = c("K56.59", "T82.868A", NA, NA),
                              reoperation3 = c(TRUE, TRUE, FALSE, NA),
                              retor3podays = c(10, 7, NA, NA),
                              reopor3cpt1 = c("44005", "37211", NA, NA),
                              retor3related = c(TRUE, TRUE, NA, NA),
                              reopor3icd91 = c("K56.69","T82.868A", NA, NA),
                              reopor3icd101 = c("K56.59", "T82.868A", NA, NA))

  y <- data.table::data.table(caseid = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
                              nreoperation = c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L),
                              reoperation = c(TRUE,TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
                              retorpodays = c(10, 10, 10, 7, 7, 7, NA, NA, NA),
                              reoporcpt = c("44005", "44005", "44005", "37211", "37211", "37211", NA, NA, NA),
                              retorrelated = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, NA, NA, NA),
                              reoporicd9 = c("K56.69", "K56.69", "K56.69", "T82.868A", "T82.868A", "T82.868A", NA, NA, NA),
                              reoporicd10 = c("K56.59", "K56.59", "K56.59", "T82.868A", "T82.868A", "T82.868A", NA, NA, NA))

  testthat::expect_equal(make_reop_long(x), y)
  testthat::expect_equal(make_reop_long(x, TRUE), y[-7:-9, ])
  testthat::expect_null(make_reop_long(data.table::data.table()))
})

testthat::test_that("make_readm_long works", {
  x <- data.table::data.table(caseid = c(1,2,3,4),
                              readmission1 = c(TRUE, TRUE, FALSE, NA),
                              readmpodays1 = c(10, 7, NA, NA),
                              readmrelated1 = c(TRUE, FALSE, NA, NA),
                              readmsuspreason1 = c("Reason", "Reason", NA, NA),
                              readmrelicd91 = c("111", "222", NA, NA),
                              readmrelicd101 = c("1111","2222", NA, NA),
                              unplannedreadmission1 = c(TRUE, FALSE, NA, NA),
                              readmunrelsusp1 = c("Reason", NA, NA, NA),
                              readmunrelicd91 = c("111", NA, NA, NA),
                              readmunrelicd101 = c("1111",NA,NA,NA),
                              readmission2 = c(TRUE, NA, FALSE, NA),
                              readmpodays2 = c(10, 7, NA, NA),
                              readmrelated2 = c(TRUE, FALSE, NA, NA),
                              readmsuspreason2 = c("Reason", "Reason", NA, NA),
                              readmrelicd92 = c("111", "222", NA, NA),
                              readmrelicd102 = c("1111","2222", NA, NA),
                              unplannedreadmission2 = c(TRUE, FALSE, NA, NA),
                              readmunrelsusp2 = c("Reason", NA, NA, NA),
                              readmunrelicd92 = c("111", NA, NA, NA),
                              readmunrelicd102 = c("1111",NA,NA,NA),
                              readmission3 = c(TRUE, TRUE, FALSE, NA),
                              readmpodays3 = c(10, 7, NA, NA),
                              readmrelated3 = c(TRUE, FALSE, NA, NA),
                              readmsuspreason3 = c("Reason", "Reason", NA, NA),
                              readmrelicd93 = c("111", "222", NA, NA),
                              readmrelicd103 = c("1111","2222", NA, NA),
                              unplannedreadmission3 = c(TRUE, FALSE, NA, NA),
                              readmunrelsusp3 = c("Reason", NA, NA, NA),
                              readmunrelicd93 = c("111", NA, NA, NA),
                              readmunrelicd103 = c("1111",NA,NA,NA),
                              readmission4 = c(TRUE, TRUE, FALSE, NA),
                              readmpodays4 = c(10, 7, NA, NA),
                              readmrelated4 = c(TRUE, FALSE, NA, NA),
                              readmsuspreason4 = c("Reason", "Reason", NA, NA),
                              readmrelicd94 = c("111", "222", NA, NA),
                              readmrelicd104 = c("1111","2222", NA, NA),
                              unplannedreadmission4 = c(TRUE, FALSE, NA, NA),
                              readmunrelsusp4 = c("Reason", NA, NA, NA),
                              readmunrelicd94 = c("111", NA, NA, NA),
                              readmunrelicd104 = c("1111",NA,NA,NA),
                              readmission5 = c(TRUE, TRUE, FALSE, NA),
                              readmpodays5 = c(10, 7, NA, NA),
                              readmrelated5 = c(TRUE, FALSE, NA, NA),
                              readmsuspreason5 = c("Reason", "Reason", NA, NA),
                              readmrelicd95 = c("111", "222", NA, NA),
                              readmrelicd105 = c("1111","2222", NA, NA),
                              unplannedreadmission5 = c(TRUE, FALSE, NA, NA),
                              readmunrelsusp5 = c("Reason", NA, NA, NA),
                              readmunrelicd95 = c("111", NA, NA, NA),
                              readmunrelicd105 = c("1111",NA,NA,NA))

  y <- data.table::data.table(caseid = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3),
                              nreadmission = c(1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 5L),
                              readmission = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
                              readmpodays = c(10, 10, 10, 10, 10, 7, 7, 7, 7, NA, NA, NA, NA, NA),
                              unplannedreadmission = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, NA, NA, NA, NA, NA),
                              readmrelated = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, NA, NA, NA, NA, NA),
                              readmsuspreason = c("Reason", "Reason", "Reason", "Reason", "Reason", "Reason", "Reason", "Reason", "Reason", NA, NA, NA, NA, NA),
                              readmunrelsusp = c("Reason", "Reason", "Reason", "Reason", "Reason", NA, NA, NA, NA, NA, NA, NA, NA, NA),
                              readmrelicd9 = c("111", "111", "111", "111", "111", "222", "222", "222", "222", NA, NA, NA, NA, NA),
                              readmrelicd10 = c("1111", "1111", "1111", "1111", "1111", "2222", "2222", "2222", "2222", NA, NA, NA, NA, NA),
                              readmunrelicd9 = c("111", "111", "111", "111", "111", NA, NA, NA, NA, NA, NA, NA, NA, NA),
                              readmunrelicd10 = c("1111", "1111", "1111", "1111", "1111", NA, NA, NA, NA, NA, NA, NA, NA, NA))

  testthat::expect_equal(make_readm_long(x), y)
  testthat::expect_equal(make_readm_long(x, TRUE), y[-10:-14, ])
  testthat::expect_null(make_readm_long(data.table::data.table()))
})

testthat::test_that("make_anesthes_other_long works", {
  x <- data.table::data.table(caseid = c(1,2,3,4),
                              anesthes_other = c("General","General, Spinal", "General, Spinal, MAC/IV Sedation", NA))

  y <- data.table::data.table(caseid = c(1, 2, 2, 3, 3, 3),
                              nanesthes_other = c(1L, 1L, 2L, 1L, 2L, 3L),
                              anesthes_other = factor(c("General", "General", "Spinal", "General", "Spinal", "Monitored anesthesia care"),
                                                      levels = c("Epidural","General","Local","Monitored anesthesia care","None","Other","Regional","Spinal")))

  testthat::expect_equal(make_anesthes_other_long(x), y)
  testthat::expect_null(make_anesthes_other_long(data.table::data.table()))
})

testthat::test_that("make_pan_percdrainage_long works", {
  x <- data.table::data.table(caseid = 1:15,
                              pan_percdrainage = c("Yes-other", "Yes-bile", "Yes-pus",
                                                   "Yes-amylase-rich fluid", "Yes-amylase-rich fluid,Yes-pus",
                                                   "Yes-bile,Yes-other", "Yes-amylase-rich fluid,Yes-pus,Yes-other",
                                                   "Yes-amylase-rich fluid,Yes-bile,Yes-other", "Yes-pus,Yes-bile",
                                                   "Yes-pus,Yes-other", "Yes-amylase-rich fluid,Yes-pus,Yes-bile",
                                                   "Yes-amylase-rich fluid,Yes-other", "Yes-amylase-rich fluid,Yes-pus,Yes-bile,Yes-other",
                                                   "Yes-amylase-rich fluid,Yes-bile", "Yes-pus,Yes-bile,Yes-other"))
  make_pan_percdrainage_cols(x)

  y <- data.table::data.table(caseid = c(1L, 2L, 3L, 4L, 5L, 5L, 6L,6L, 7L, 7L, 7L, 8L, 8L, 8L, 9L, 9L, 10L, 10L,
                                         11L, 11L, 11L, 12L, 12L, 13L, 13L, 13L, 13L, 14L, 14L, 15L, 15L, 15L),
                              npercdrainage = c(1L, 1L, 1L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L,
                                                1L, 2L, 1L, 2L, 3L, 1L, 2L, 1L, 2L, 3L, 4L, 1L, 2L, 1L, 2L, 3L),
                              percdrainage = c("Yes-other", "Yes-bile", "Yes-pus", "Yes-amylase-rich fluid",
                                               "Yes-amylase-rich fluid", "Yes-pus", "Yes-bile", "Yes-other",
                                               "Yes-amylase-rich fluid", "Yes-pus", "Yes-other", "Yes-amylase-rich fluid",
                                               "Yes-bile", "Yes-other", "Yes-pus", "Yes-bile", "Yes-pus", "Yes-other",
                                               "Yes-amylase-rich fluid", "Yes-pus", "Yes-bile", "Yes-amylase-rich fluid",
                                               "Yes-other", "Yes-amylase-rich fluid", "Yes-pus", "Yes-bile", "Yes-other",
                                               "Yes-amylase-rich fluid", "Yes-bile", "Yes-pus", "Yes-bile", "Yes-other"))

  testthat::expect_equal(make_pan_percdrainage_long(x), y)
  testthat::expect_null(make_pan_percdrainage_long(data.table::data.table()))
})

testthat::test_that("make_amylase_long works", {
  x <- data.table::data.table(caseid = 1:30,
                              pan_amylase_pod1 = c(NA, 3439L, 8817L, 6082L, 9805L, 7159L, 5389L, 7994L, 9258L,
                                                   6324L, 3610L, 9366L, 2870L, 4513L, 9146L, 1895L, 5985L, 5463L,
                                                   1045L, 1032L, 8639L, 9672L, 5401L, 6309L, 3063L, 7331L, 8368L, 1341L, 3158L, 9575L),
                              pan_amylase_pod230 = c(NA, 5829L, 7374L, 8896L, 8113L, 1529L, 3858L, 7262L, 4652L, 2942L, 3474L,
                                                     4057L, 5721L, 5126L, 4818L, 4812L, 8941L, 1135L, 3906L, 7273L, 3748L,
                                                     5734L, 3663L, 3659L, 2371L, 8570L, 8137L, 1887L, 9045L, 3509L),
                              damylase = c(NA, 29L, 19L, 20L, 30L, 27L, 3L, 15L, 9L, 22L, 4L, 25L, 23L, 2L, 24L, 21L, 13L,
                                           18L, 28L, 10L, 11L, 5L, 16L, 8L, 26L, 7L, 6L, 12L, 17L, 14L))

  y <- data.table::data.table(caseid = c(2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 6L, 6L, 7L, 7L, 8L, 8L, 9L, 9L, 10L, 10L,
                                         11L, 11L, 12L, 12L, 13L, 13L, 14L, 14L, 15L, 15L, 16L, 16L, 17L, 17L, 18L,
                                         18L, 19L, 19L, 20L, 20L, 21L, 21L, 22L, 22L, 23L, 23L, 24L, 24L, 25L, 25L,
                                         26L, 26L, 27L, 27L, 28L, 28L, 29L, 29L, 30L, 30L),
                              pod = c(1, 29, 1, 19, 1, 20, 1, 30, 1, 27, 1, 3, 1, 15, 1, 9, 1, 22, 1, 4, 1, 25, 1,
                                      23, 1, 2, 1, 24, 1, 21, 1, 13, 1, 18, 1, 28, 1, 10, 1, 11, 1, 5, 1, 16, 1, 8,
                                      1, 26, 1, 7, 1, 6, 1, 12, 1, 17, 1, 14),
                              amylase = c(3439L, 5829L, 8817L, 7374L, 6082L, 8896L, 9805L, 8113L, 7159L, 1529L,
                                          5389L, 3858L, 7994L, 7262L, 9258L, 4652L, 6324L, 2942L, 3610L, 3474L,
                                          9366L, 4057L, 2870L, 5721L, 4513L, 5126L, 9146L, 4818L, 1895L, 4812L,
                                          5985L, 8941L, 5463L, 1135L, 1045L, 3906L, 1032L, 7273L, 8639L, 3748L,
                                          9672L, 5734L, 5401L, 3663L, 6309L, 3659L, 3063L, 2371L, 7331L, 8570L,
                                          8368L, 8137L, 1341L, 1887L, 3158L, 9045L, 9575L, 3509L))

  testthat::expect_equal(make_amylase_long(x), y)
  testthat::expect_null(make_amylase_long(data.table::data.table()))
})
