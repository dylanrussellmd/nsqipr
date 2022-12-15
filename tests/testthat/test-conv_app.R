testthat::test_that("conv_app_img_ultra works", {
  test <- c("US done-consistent with appendicitis",
            "US not performed/not documented",
            "US done-indeterminate; result uncertain",
            "US done-not consistent with appendicitis",
            NA)

  testthat::expect_equal(conv_app_img_ultra(test),
                         c(TRUE,
                           FALSE,
                           TRUE,
                           TRUE,
                           NA))
})

testthat::test_that("conv_app_img_ct works", {
  test <- c("CT not performed/not documented",
            "CT done-result consistent with diagnosis of appendicitis",
            "CT done-result not consistent with appendicitis",
            "CT done-result indeterminate; result uncertain",
            NA)

  testthat::expect_equal(conv_app_img_ct(test),
                         c(FALSE,
                           TRUE,
                           TRUE,
                           TRUE,
                           NA))
})


testthat::test_that("conv_app_img_mri works", {
  test <- c("MRI / Other Definitive Imaging Modality Not Performed / Not Documented",
            "Result Indeterminate / Uncertain",
            "Result consistent w/ diagnosis of Appendicitis",
            'Result Not Consistent w/ Appendicitis; Appendix "Normal"',
            NA)

  testthat::expect_equal(conv_app_img_mri(test),
                         c(FALSE,
                           TRUE,
                           TRUE,
                           TRUE,
                           NA))
})

testthat::test_that("conv_app_abscess works", {
  test <- c("No mention of perforation or abscess",
            "Perforation and abscess",
            "Perforation only",
            "Abscess only",
            NA)

  testthat::expect_equal(conv_app_abscess(test),
                         c(FALSE,
                           TRUE,
                           FALSE,
                           TRUE,
                           NA))
})

testthat::test_that("conv_app_perforation works", {
  test <- c("No mention of perforation or abscess",
            "Perforation and abscess",
            "Perforation only",
            "Abscess only",
            NA)

  testthat::expect_equal(conv_app_perforation(test),
                         c(FALSE,
                           TRUE,
                           TRUE,
                           FALSE,
                           NA))
})

testthat::test_that("conv_app_intraabscess works", {
  test <- c("No diagnosis of postoperative abscess",
            "Yes-percutaneous drainage",
            "Yes-no treatment or intervention",
            "Yes-IV antibiotics w/out procedural intervention",
            "Yes-oral/IM antibiotics w/out procedural intervention",
            "Yes-reoperation for surgical drainage, open",
            "Yes-reoperation for surgical drainage, minimally invasive",
            "Yes-transrectal/other endoscopic drainage",
            NA)

  testthat::expect_equal(conv_app_intraabscess(test),
                         c(FALSE,
                           TRUE,
                           TRUE,
                           TRUE,
                           TRUE,
                           TRUE,
                           TRUE,
                           TRUE,
                           NA))
})

testthat::test_that("conv_app_intraabscess_intervention works", {
  test <- c("No diagnosis of postoperative abscess",
            "Yes-percutaneous drainage",
            "Yes-no treatment or intervention",
            "Yes-IV antibiotics w/out procedural intervention",
            "Yes-oral/IM antibiotics w/out procedural intervention",
            "Yes-reoperation for surgical drainage, open",
            "Yes-reoperation for surgical drainage, minimally invasive",
            "Yes-transrectal/other endoscopic drainage",
            NA)
  gs <- c("Percutaneous drainage",
          "IV antibiotics",
          "PO/IM antibiotics",
          "Open reoperation",
          "MIS reoperation",
          "Transrectal or other endoscopic drainage")

  test_levels(conv_app_intraabscess_intervention, test, gs)
})
