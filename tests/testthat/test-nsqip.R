test_that("nsqip 2018 works", {
  goldstandard <- readRDS(file.path("test-data/goldstandard18.rds"))
  nsqipr::nsqip(file.path("test-data/test_acs_nsqip_puf18.txt"), csv = NA)
  testthat::expect_equal(basename(fs::dir_ls("test-data/acs_nsqip_puf")), c("test_acs_nsqip_puf18.txt","test_acs_nsqip_puf18_anesthes_other.rds","test_acs_nsqip_puf18_clean.rds","test_acs_nsqip_puf18_cpt.rds","test_acs_nsqip_puf18_readmission.rds","test_acs_nsqip_puf18_reoperation.rds"))
  test <- readRDS(file.path("test-data/acs_nsqip_puf/test_acs_nsqip_puf18_clean.rds"))
  expect_identical(goldstandard, test)
  fs::file_move(file.path("test-data/acs_nsqip_puf/test_acs_nsqip_puf18.txt"),file.path("test-data/test_acs_nsqip_puf18.txt"))
  fs::dir_delete("test-data/acs_nsqip_puf")
})

test_that("nsqip 2005-2006 works", {
  goldstandard <- readRDS(file.path("test-data/goldstandard_05_06.rds"))
  nsqipr::nsqip(file.path("test-data/test_acs_nsqip_puf_05_06.txt"), csv = NA)
  testthat::expect_equal(basename(fs::dir_ls("test-data/acs_nsqip_puf")), c("test_acs_nsqip_puf_05_06.txt","test_acs_nsqip_puf_05_06_anesthes_other.rds","test_acs_nsqip_puf_05_06_clean.rds","test_acs_nsqip_puf_05_06_cpt.rds","test_acs_nsqip_puf_05_06_readmission.rds","test_acs_nsqip_puf_05_06_reoperation.rds"))
  test <- readRDS(file.path("test-data/acs_nsqip_puf/test_acs_nsqip_puf_05_06_clean.rds"))
  expect_identical(goldstandard, test)
  fs::file_move(file.path("test-data/acs_nsqip_puf/test_acs_nsqip_puf_05_06.txt"),file.path("test-data/test_acs_nsqip_puf_05_06.txt"))
  fs::dir_delete("test-data/acs_nsqip_puf")
})
