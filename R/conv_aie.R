conv_aie_cols <- function(df) {
  df %>%
    dplyr::mutate(
      pufyear = tryCatch(conv_pufyear(caseid), error = function(e) return(NULL)),
      aie_proc = tryCatch(conv_aie_proc(aie_proc), error = function(e) return(NULL)),
      aie_sympt = tryCatch(conv_aie_sympt(aie_sympt), error = function(e) return(NULL)),
      aie_hrf_anat = tryCatch(conv_aie_hrf_anat(aie_hrf_anat), error = function(e) return(NULL)),
      aie_prehemo = tryCatch(conv_aie_preposthemo(aie_prehemo), error = function(e) return(NULL)),
      aie_posthemo = tryCatch(conv_aie_preposthemo(aie_posthemo), error = function(e) return(NULL)),
      aie_mostsevoutcome = tryCatch(conv_aie_mostsevoutcome(aie_mostsevoutcome), error = function(e) return(NULL)),
    )
}

conv_aie_mostsevoutcome <- function(vec) {
  val <- c("death" = 1L,
           "major amputation" = 2L,
           "new bypass in the treated arterial segment" = 3L,
           "patent treated arterial segment with stenosis" = 4L,
           "patent treated arterial segment, no stenosis" = 5L,
           "reintervened treated arterial segment with no stenosis" = 6L,
           "reintervened treated arterial segment with stenosis" = 7L,
           "thrombosis with no planned intervention" = 8L,
           "not documented" = NA
  )
  unname(val[vec])
}

conv_aie_preposthemo <- function(vec) {
  val <- c("abi  0.40 - 0.89" = 1L,
           "abi  0.90 - 1.29" = 2L,
           "abi <= 0.39" = 3L,
           'abi >= 1.30; arteries "noncompressible", no toe pressure taken' = 4L,
           'abi >= 1.30; arteries "noncompressible", toe pressure >= 30 mm Hg' = 5L,
           'abi >= 1.30; arteries "noncompressible", toe pressure < 30 mm Hg' = 6L,
           'abi not performed; "not palpable"' = 7L,
           'abi not performed; ipsilateral pedal pulse "palpable"' = 8L,
           "abi not performed w/in 30 days; evidence of patient clinically well" = 9L,
           "not documented" = NA,
           "none/not documented" = NA
          )
  unname(val[vec])
}

conv_aie_hrf_anat <- function(vec) {
  val <- c("prior ipsilateral bypass involving currently treated segment" = 1L,
           "prior ipsilateral percutaneous intervention involving currently treated segment" = 2L,
           "none/not documented" = NA)
  unname(val[vec])
}

conv_aie_sympt <- function(vec) {
  val <- c("critical limb ischemia: rest pain" = 1L,
           "critical limb ischemia: tissue loss" = 2L,
           "asymptomatic" = 3L,
           "claudication" = 4L,
           "not documented" = NA)
  unname(val[vec])
}

conv_aie_proc <- function(vec) {
  val <- c("aortic angioplasty/stenting" = 1L,
           "bilateral common iliac (kissing) angioplasty/stenting" = 2L,
           "common and external iliac angioplasty/stenting" = 3L,
           "common and internal iliac angioplasty/stenting" = 4L,
           "common iliac angioplasty/stenting" = 5L,
           "external iliac angioplasty/stenting" = 6L,
           "internal iliac angioplasty/stenting" = 7L,
           "not documented" = NA)
  unname(val[vec])
}

