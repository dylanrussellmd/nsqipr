#' Check if a boolean is TRUE and not NA
#'
#' This represent a key assumption about the Dindo classification sorting. If a complication is missing (NA), it is considered FALSE
#' for the purposes of the Dindo classification.
#'
#' @param bool a logical vector
#'
#' @return a logical vector
#'
#' @keywords internal
#'
checkTrue <- function(bool) {
  (bool %in% TRUE) & !is.na(bool)
}

#' Check if any trues exist row-wise in a logical matrix.
#'
#' @param ... a list of logical vectors or a data frame
#'
#' @return a logical vector. TRUE if any trues, FALSE if no trues.
#'
#' @keywords internal
#'
checkAnyTrue <- function(...) {
  apply(cbind(...), 2, checkTrue) %>%
    apply(., 1, any)
}

#' Check if a patient has died within 30 days of the index procedure. Assumes that if a patient died, a date would be
#' recorded in `yrdeath`.
#'
#' @param col a vector of any type
#'
#' @return a logical vector. TRUE if not NA, FALSE if NA.
#'
#' @keywords internal
#'
isDead <- function(col) {
  !is.na(col)
}

#' Check if any trues exist row-wise in a logical matrix.
#'
#' @param ... a list of vectors of any type or a data frame
#'
#' @return a logical vector. TRUE if any NA values in row, FALSE if no NA values in row.
#'
#' @keywords internal
#'
checkAnyDead <- function(...) {
  apply(cbind(...), 2, isDead) %>%
    apply(., 1, any)
}


#' Classifies a patient according to the Dindo-Clavien surgical complication grading scale.
#'
#' @param df a dataframe including relevant columns containing information on specific post-operative complications.
#'
#' @return a numeric vector representing the Dindo-Clavien classification.
#'
#' @keywords internal
#'
dindo <- function(df) {
  e <- new.env()
  dindo_list <- list(dindo_1, dindo_2, dindo_3, dindo_4, dindo_5)
  dindo <- rep(0, nrow(df))

  dindo_cat <- function(x, y, df) {
    dindo_cols <- colnames(df)[which(colnames(df) %in% x)]
    if(length(dindo_cols) > 0) {
      if(y == 5) {
        dindo[which(checkAnyDead(df[dindo_cols]))] <<- y
      } else {
        dindo[which(checkAnyTrue(df[dindo_cols]))] <<- y
      }
    }
  }

  purrr::imap(dindo_list, ~dindo_cat(.x, .y, df))
  return(dindo)
}
