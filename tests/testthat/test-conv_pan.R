testthat::test_that("conv_pan_fistula works", {
  test <- c("No","No evidence of Biochemical Leak or POPF","Biochemical Leak only","Yes, Grade B POPF present","Yes, Grade C POPF present","Yes-clinical diagnosis, NPO-TPN","Yes-clinical diagnosis, drain continued >7 days","Yes-clinical diagnosis, percutaneous drainage performed","Yes-clinical diagnosis, reoperation performed", "Yes-clinical diagnosis, spontaneous wound drainage","Yes-persistent drainage, NPO-TPN","Yes-persistent drainage, drain continued >7 days","Yes-persistent drainage, percutaneous drainage performed","Yes-persistent drainage, reoperation performed", NA)
  testthat::expect_equal(conv_pan_fistula(test), c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, NA))
})

testthat::test_that("conv_pan_fistula_type works", {
  test <- c("No", "Yes-persistent drainage, drain continued >7 days","Yes-clinical diagnosis, drain continued >7 days", "Yes-persistent drainage, percutaneous drainage performed","Yes-clinical diagnosis, percutaneous drainage performed", "Yes-persistent drainage, reoperation performed","Unknown", "Yes-clinical diagnosis, reoperation performed", "Yes-clinical diagnosis, spontaneous wound drainage","Yes-persistent drainage, NPO-TPN", "Yes-clinical diagnosis, NPO-TPN", "No evidence of Biochemical Leak or POPF","Biochemical Leak only", "Yes, Grade B POPF present", "Yes, Grade C POPF present")
  testthat::expect_equal(sort(levels(conv_pan_fistula_type(test))), sort(levels(factor(c("Biochemical leak only", "Grade B POPF", "Grade C POPF", "Clinical diagnosis", "Persistent drainage"  )))))
  testthat::expect_equal(sort(unique(as.character(na.omit(conv_pan_fistula_type(test))))), sort(as.character(factor(c("Biochemical leak only", "Grade B POPF", "Grade C POPF", "Clinical diagnosis", "Persistent drainage")))))
})

testthat::test_that("conv_pan_fistula_intervention works", {
  test <- c("No", "Yes-persistent drainage, drain continued >7 days","Yes-clinical diagnosis, drain continued >7 days", "Yes-persistent drainage, percutaneous drainage performed","Yes-clinical diagnosis, percutaneous drainage performed", "Yes-persistent drainage, reoperation performed","Unknown", "Yes-clinical diagnosis, reoperation performed", "Yes-clinical diagnosis, spontaneous wound drainage","Yes-persistent drainage, NPO-TPN", "Yes-clinical diagnosis, NPO-TPN", "No evidence of Biochemical Leak or POPF","Biochemical Leak only", "Yes, Grade B POPF present", "Yes, Grade C POPF present")
  testthat::expect_equal(sort(levels(conv_pan_fistula_intervention(test))), sort(levels(factor(c("NPO-TPN", "Drain continued >7 days", "Percutaneous drainage", "Reoperation", "Spontaneous wound drainage")))))
  testthat::expect_equal(sort(unique(as.character(na.omit(conv_pan_fistula_intervention(test))))), sort(as.character(factor(c("NPO-TPN", "Drain continued >7 days", "Percutaneous drainage", "Reoperation", "Spontaneous wound drainage")))))
})

testthat::test_that("conv_pan_delgastric works", {
  test <- c(NA, "No", "Yes-no oral intake by POD 14", "Yes-tube to external drainage/NG tube present/reinserted")
  testthat::expect_equal(sort(levels(conv_pan_delgastric(test))), sort(levels(factor(c("No oral intake by POD 14", "Tube to external drainage/NG tube present/reinserted")))))
  testthat::expect_equal(sort(unique(as.character(na.omit(conv_pan_delgastric(test))))), sort(as.character(factor(c("No oral intake by POD 14", "Tube to external drainage/NG tube present/reinserted")))))
})


