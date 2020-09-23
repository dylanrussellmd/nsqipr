#' Convert targeted aortoiliac endo columns
#'
#' @param df a data table to be cleaned
#' @param filename the name of the file from which the data table has been read in
#'
#' @details If the file being processed is a targeted aortoiliac endo dataset,
#' it will be processed by this function. This function determines how data cleaning steps specific
#' to targeted aortoiliac endo dataset files should proceed.
#'
#' @keywords internal
#'
conv_aie_cols <- function(df, filename) {
  get_pufyear(df, filename)
}

#### ---- FACTOR LISTS (THESE DEFINE THE FACTOR LEVELS FOR VARIOUS COLUMNS) ----
aie_mostsevoutcome <- list(
  `Death` = "Death",
  `Major amputation` = "Major Amputation",
  `New bypass in the treated arterial segment` = "New bypass in the treated arterial segment",
  `Patent treated arterial segment with stenosis` = "Patent treated arterial segment with stenosis",
  `Patent treated arterial segment with no stenosis` = "Patent treated arterial segment, no stenosis",
  `Reintervened treated arterial segment with no stenosis` = "Reintervened treated arterial segment with no stenosis",
  `Reintervened treated arterial segment with stenosis` = "Reintervened treated arterial segment with stenosis",
  `Thrombosis with no planned intervention` = "Thrombosis with no planned intervention"
)
aie_posthemo <- list(
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
aie_prehemo <- aie_posthemo
aie_hrf_anat <- list(
  `Prior ipsilateral bypass` = "Prior ipsilateral bypass involving currently treated segment",
  `Prior ipsilateral percutaneous intervention` = "Prior ipsilateral percutaneous intervention involving currently treated segment"
)
aie_sympt <- list(
  `Rest pain` = "Critical limb ischemia: rest pain",
  `Tissue loss` = "Critical limb ischemia: tissue loss",
  `Asymptomatic` = "Asymptomatic",
  `Claudication` = "Claudication"
)
aie_proc <- list(
  `Aortic` = "Aortic angioplasty/stenting",
  `Bilateral common iliac (kissing)` = "Bilateral common iliac (kissing) angioplasty/stenting",
  `Common and external iliac` = "Common and external iliac angioplasty/stenting",
  `Common and internal iliac` = "Common and internal iliac angioplasty/stenting",
  `Common iliac` = "Common iliac angioplasty/stenting",
  `External iliac` = "External iliac angioplasty/stenting",
  `Internal iliac` = "Internal iliac angioplasty/stenting"
)
