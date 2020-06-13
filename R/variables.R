new_variable <- function(var = character()) {
  stopifnot(is_character(var))
  structure(var, class = "nsqiprVariable")
}

validate_variable <- function(x) {
  value <- unclass(x)
  if(is_null(match.arg(value, var_list))) {
    stop(
      "Variable name is not valid.",
      call. = FALSE
    )
  }
}
