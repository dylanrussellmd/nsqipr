#' Calculate BMI
#'
#' @param height a numeric vector
#' @param weight a numeric vector
#'
#' @return a numeric vector
#'
#' @export
#'
#' @examples bmi(72, 180)
#'
bmi <- function(height, weight) {
  703 * weight/(height^2)
}
