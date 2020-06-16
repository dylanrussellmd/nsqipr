new_variable <- function(var = character()) {
  stopifnot(rlang::is_character(var))
  structure(var, class = "nsqiprVariable")
}

validate_variable <- function(x) {
  value <- unclass(x)
  if(rlang::is_null(match.arg(value, c("test1","test2")))) {
    stop(
      "Variable name is not valid.",
      call. = FALSE
    )
  }
}
