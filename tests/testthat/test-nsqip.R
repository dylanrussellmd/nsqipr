testthat::test_that("puf nsqip works", {

  testthat::expect_equal(sort(fs::path_file(fs::dir_ls(file.path("test-data","acs_nsqip_puf", "rds"),
                                                       type = "file"))),
                         sort(fs::path_file(fs::dir_ls(file.path("test-data"),
                                                       type = "file",
                                                       glob = "*acs_nsqip_puf*",
                                                       ignore.case = TRUE))))

  test_goldstandard("acs_nsqip_puf", "ACS_NSQIP_PUF_05_06_vr1_clean")
  test_goldstandard("acs_nsqip_puf", "ACS_NSQIP_PUF07_TXT_clean")
  test_goldstandard("acs_nsqip_puf", "ACS_NSQIP_PUF08_TXT_clean")
  test_goldstandard("acs_nsqip_puf", "ACS_NSQIP_PUF09_TXT_clean")
  test_goldstandard("acs_nsqip_puf", "ACS_NSQIP_PUF10_TXT_clean")
  test_goldstandard("acs_nsqip_puf", "ACS_NSQIP_PUF11_TXT_clean")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf12_clean")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf13_clean")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf14_clean")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf15_v2_clean")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf16_clean")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf17_clean")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf18_v2_clean")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf19_clean")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf20_clean")

  test_goldstandard("acs_nsqip_puf", "ACS_NSQIP_PUF_05_06_vr1_cpt")
  test_goldstandard("acs_nsqip_puf", "ACS_NSQIP_PUF07_TXT_cpt")
  test_goldstandard("acs_nsqip_puf", "ACS_NSQIP_PUF08_TXT_cpt")
  test_goldstandard("acs_nsqip_puf", "ACS_NSQIP_PUF09_TXT_cpt")
  test_goldstandard("acs_nsqip_puf", "ACS_NSQIP_PUF10_TXT_cpt")
  test_goldstandard("acs_nsqip_puf", "ACS_NSQIP_PUF11_TXT_cpt")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf12_cpt")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf13_cpt")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf14_cpt")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf15_v2_cpt")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf16_cpt")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf17_cpt")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf18_v2_cpt")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf19_cpt")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf20_cpt")

  test_goldstandard("acs_nsqip_puf", "ACS_NSQIP_PUF11_TXT_readmission")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf12_readmission")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf13_readmission")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf14_readmission")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf15_v2_readmission")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf16_readmission")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf17_readmission")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf18_v2_readmission")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf19_readmission")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf20_readmission")

  test_goldstandard("acs_nsqip_puf", "ACS_NSQIP_PUF11_TXT_reoperation")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf12_reoperation")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf13_reoperation")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf14_reoperation")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf15_v2_reoperation")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf16_reoperation")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf17_reoperation")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf18_v2_reoperation")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf19_reoperation")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf20_reoperation")

  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf14_anesthes_other")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf15_v2_anesthes_other")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf16_anesthes_other")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf17_anesthes_other")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf18_v2_anesthes_other")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf19_anesthes_other")
  test_goldstandard("acs_nsqip_puf", "acs_nsqip_puf20_anesthes_other")
})

testthat::test_that("puf tar app works", {

  testthat::expect_equal(sort(fs::path_file(fs::dir_ls(file.path("test-data","puf_tar_app", "rds"),
                                                       type = "file"))),
                         sort(fs::path_file(fs::dir_ls(file.path("test-data"),
                                                       type = "file",
                                                       glob = "*puf_tar_app*",
                                                       ignore.case = TRUE))))

  test_goldstandard("puf_tar_app", "puf_tar_app_2016_clean")
  test_goldstandard("puf_tar_app", "puf_tar_app_2017_clean")
  test_goldstandard("puf_tar_app", "puf_tar_app_2018_clean")
  test_goldstandard("puf_tar_app", "puf_tar_app_2019_clean")
  test_goldstandard("puf_tar_app", "puf_tar_app_2020_clean")
})

testthat::test_that("puf tar col works", {

  testthat::expect_equal(sort(fs::path_file(fs::dir_ls(file.path("test-data","puf_tar_col", "rds"),
                                                       type = "file"))),
                         sort(fs::path_file(fs::dir_ls(file.path("test-data"),
                                                       type = "file",
                                                       glob = "*puf_tar_col*",
                                                       ignore.case = TRUE))))

  test_goldstandard("puf_tar_col", "puf_tar_col_2012_clean")
  test_goldstandard("puf_tar_col", "puf_tar_col_2013_clean")
  test_goldstandard("puf_tar_col", "puf_tar_col_2014_clean")
  test_goldstandard("puf_tar_col", "puf_tar_col_2015_clean")
  test_goldstandard("puf_tar_col", "puf_tar_col_2016_clean")
  test_goldstandard("puf_tar_col", "puf_tar_col_2017_clean")
  test_goldstandard("puf_tar_col", "puf_tar_col_2018_clean")
  test_goldstandard("puf_tar_col", "puf_tar_col_2019_clean")
  test_goldstandard("puf_tar_col", "puf_tar_col_2020_clean")
})
