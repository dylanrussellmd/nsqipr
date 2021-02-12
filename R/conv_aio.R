#' Convert targeted aortoiliac open columns
#'
#' @param df a data table to be cleaned
#' @param filename the name of the file from which the data table has been read in
#'
#' @details If the file being processed is a targeted aortoiliac open dataset,
#' it will be processed by this function. This function determines how data cleaning steps specific
#' to targeted aortoiliac open files should proceed.
#'
#' @keywords internal
#'

conv_aio_cols <- function(df, filename) {
  get_pufyear(df, filename)
}

#### ---- FACTOR LISTS (THESE DEFINE THE FACTOR LEVELS FOR VARIOUS COLUMNS) ----
aio_mostsevoutcome <- list(
  `Death` = "Death",
  `Major amputation` = "Major Amputation",
  `New bypass in the treated arterial segment` = "New bypass in the treated arterial segment",
  `Patent treated arterial segment with stenosis` = "Patent treated arterial segment with stenosis",
  `Patent treated arterial segment with no stenosis` = "Patent treated arterial segment, no stenosis",
  `Reintervened treated arterial segment with no stenosis` = "Reintervened treated arterial segment with no stenosis",
  `Reintervened treated arterial segment with stenosis` = "Reintervened treated arterial segment with stenosis",
  `Thrombosis with no planned intervention` = "Image-proven treated arterial segment thrombosis or clinically evident thrombosis with no planned intervention"
)
aio_posthemo <- list(
  `ABI  0.40 - 0.89` = "ABI 0.40 - 0.89",
  `ABI  0.90 - 1.29` = "ABI 0.90 - 1.29",
  `ABI <= 0.39` = "ABI <= 0.39",
  `ABI >= 1.30; arteries "noncompressible", no toe pressure taken` = 'ABI >= 1.30; arteries \"\"noncompressible\"\", no toe pressure taken',
  `ABI >= 1.30; arteries "noncompressible", toe pressure >= 30 mm Hg` = 'ABI >= 1.30; arteries \"\"noncompressible\"\", toe pressure >= 30 mm Hg',
  `ABI >= 1.30; arteries "noncompressible", toe pressure < 30 mm Hg` = 'ABI >= 1.30; arteries \"\"noncompressible\"\", toe pressure < 30 mm Hg',
  `ABI not performed; "not palpable"` = 'ABI not performed; \"\"not palpable\"\"',
  `ABI not performed; ipsilateral pedal pulse "palpable"` = 'ABI not performed; ipsilateral pedal pulse \"\"palpable\"\"',
  `ABI not performed w/in 30 days; evidence of patient clinically well` = "ABI not performed w/in 30 days; evidence of patient clinically well"
)
aio_prehemo <- aio_posthemo
aio_hrf_anat <- list(
  `Prior abdominal surgery` = "Prior abdominal surgery",
  `Prior ipsilateral bypass` = "Prior ipsilateral bypass involving currently treated segment",
  `Prior ipsilateral percutaneous intervention` = "Prior ipsilateral percutaneous intervention involving currently treated segment"
)
aio_sympt <- list(
  `Rest pain` = "Critical limb ischemia: rest pain",
  `Tissue loss` = "Critical limb ischemia: tissue loss",
  `Asymptomatic` = "Asymptomatic",
  `Claudication` = "Claudication"
)
aio_proc <- list(
  `Ilio-femoral or Femoral-femoral bypass` = "Ilio-femoral or Femoral-femoral bypass",
  `Aortoiliac endarterectomy` = "Aortoiliac endarterectomy",
  `Aortobifemoral bypass` = "Aortobifemoral bypass",
  `Aortoiliac bypass` = "Aortoiliac bypass",
  `Aortobiliac bypass` = "Aortobiliac bypass"
)
