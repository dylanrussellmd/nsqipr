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






collect_fields <- function(dir) {
  files <- list.files(path = dir, pattern = "*.txt$",
                      full.names = TRUE, recursive = FALSE)
  headers <- unique(unlist(lapply(files, get_headers)))
}

collect_tablenames <- function(dir) {
  tablenames <- unique(unlist(lapply(files, parse_filename)))
}

get_five <- function(file) {
  column <- file %>% readr::read_lines(n_max = 1) %>%
    stringr::str_split(pattern = stringr::boundary("word")) %>%
    unlist() %>%
    stringr::str_to_lower()
  file %>% readr::read_lines(n_max = 1, skip = 1) %>%
    stringr::str_split(pattern = "\t") %>%
    unlist() %>%
    stringr::str_to_lower() %>% magrittr::set_names(., column)
}

get_headers <- function(file) {
  file %>% readr::read_lines(n_max = 1) %>%
    stringr::str_split(pattern = stringr::boundary("word")) %>%
    unlist() %>%
    stringr::str_to_lower()
}








conv_to_standard <- function(file) {
  readr::read_tsv(file, n_max = 500, na = c(" ", "")) %>%
    dplyr::rename_with(., tolower) %>%
    dplyr::mutate(dplyr::across(everything(), tolower)) %>%
    dplyr::mutate(dplyr::across(everything(), dplyr::na_if, "unknown")) %>%
    dplyr::mutate(dplyr::across(c("pufyear","admyr","operyr"), ~ lubridate::ymd(.x, truncated = 2))) %>%
    dplyr::mutate(
      caseid = caseid,
      sex = stringr::str_detect(sex, "^male$"),
      race_new = race_new,
      ethnicity_hispanic = stringr::str_detect(ethnicity_hispanic, "^yes$"),
      prncptx = prncptx,
      cpt = cpt,
      workrvu = as.numeric(workrvu),
      inout = stringr::str_detect(inout, "^inpatient$"),
      age = as.integer(ifelse(stringr::str_detect(age, "90+"), "90", age)),
      dischdest = dischdest,
      anesthes = anesthes,
      surgspec = surgspec,
      electsurg = stringr::str_detect(electsurg, "^yes$"),
      height = as.integer(height),
      weight = as.integer(weight)
    )
}

df <- conv_to_standard("../nsqipr-txt/acs_nsqip_puf18.txt")

lubridate::ymd(df$PUFYEAR, truncated = 2)
