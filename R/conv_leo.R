#' Convert targeted lower extremity open columns
#'
#' @param df a data table to be cleaned
#' @param filename the name of the file from which the data table has been read in
#'
#' @details If the file being processed is a targeted lower extremity open dataset,
#' it will be processed by this function. This function determines how data cleaning steps specific
#' to targeted lower extremity open dataset files should proceed.
#'
#' @keywords internal
#'
conv_leo_cols <- function(df, filename) {
  get_pufyear(df, filename)
}

#### ---- FACTOR LISTS (THESE DEFINE THE FACTOR LEVELS FOR VARIOUS COLUMNS) ----
leo_mostsevoutcome <- list(
  `Death` = "Death",
  `Major amputation` = "Major Amputation",
  `New bypass in the treated arterial segment` = "New bypass in the treated arterial segment",
  `Patent treated arterial segment with stenosis` = "Patent treated arterial segment with stenosis",
  `Patent treated arterial segment with stenosis` = "Patent graft with stenosis",
  `Patent treated arterial segment with no stenosis` = "Patent treated arterial segment, no stenosis",
  `Patent treated arterial segment determined clinically` = "Clinically Patent Graft",
  `Patent treated arterial segment with no stenosis` = "Patent graft, no stenosis",
  `Reintervened treated arterial segment with no stenosis` = "Reintervened treated arterial segment with no stenosis",
  `Reintervened treated arterial segment with no stenosis` = "Revised graft, no current stenosis",
  `Reintervened treated arterial segment with stenosis` = "Reintervened treated arterial segment with stenosis",
  `Reintervened treated arterial segment with stenosis` = "Revised graft with stenosis",
  `Thrombosis with no planned intervention` = "Thrombosis with no planned intervention",
  `Thrombosis with no planned intervention` = "Image-proven graft thrombosis or clinically evident thrombosis with no planned intervention"


)
leo_posthemo <- list(
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
leo_prehemo <- leo_posthemo
leo_hrf_anat <- list(
  `Prior ipsilateral bypass` = "Prior ipsilateral bypass involving currently treated segment",
  `Prior ipsilateral percutaneous intervention` = "Prior ipsilateral percutaneous intervention involving currently treated segment"
)
leo_sympt <- list(
  `Rest pain` = "Critical limb ischemia: rest pain",
  `Tissue loss` = "Critical limb ischemia: tissue loss",
  `Asymptomatic` = "Asymptomatic",
  `Claudication` = "Claudication"
)
leo_proc <- list(
  `Femoral endarterectomy` = "Femoral endarterectomy",
  `Profundoplasty` = "Profundoplasty",
  `Femoropopliteal bypass w/ single segment saphenous vein` = "Femoropopliteal bypass w/ single segment saphenous vein",
  `Femoropopliteal bypass w/prosthetic/spliced vein/composite` = "Femoropopliteal bypass w/prosthetic/spliced vein/composite",
  `Femoral distal bypass w/ single segment saphenous vein` = "Femoral distal bypass w/ single segment saphenous vein",
  `Femoral distal bypass w/ prosthetic/spliced vein/composite` = "Femoral distal bypass w/ prosthetic/spliced vein/composite",
  `Popliteal distal w/ single segment saphenous vein` = "Popliteal distal w/ single segment saphenous vein",
  `Popliteal distal bypass w/ prosthetic/spliced vein/composite or non-saphenous conduit` = "Popliteal distal bypass w/ prosthetic/spliced vein/composite or non-saphenous conduit"
)
