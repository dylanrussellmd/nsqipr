#' Fetch NSQIP hospital count
#'
#' This function scrapes the NSQIP participant website and returns the
#' total number of participating hospital.
#'
#' @keywords internal
#'
get_nsqip_hospitals <- function() {
  rvest::read_html("https://www.facs.org/search/nsqip-participants?allresults=") %>%
    rvest::html_elements('h1') %>%
    rvest::html_text() %>%
    stringi::stri_extract_first(regex = "\\d+")
}

#' Fetch NSQIP PubMed search
#'
#' This function queries PubMed for the search term 'NSQIP' and returns all the
#' results in a data frame. Note, it can only return up to 5000 results.
#'
#' @keywords internal
#'
get_nsqip_pubmed <- function() {
  easyPubMed::get_pubmed_ids('NSQIP') %>%
    easyPubMed::fetch_pubmed_data(retmax = 5000) %>%
    easyPubMed::articles_to_list(abstracts) %>%
    purrr::map_dfr(easyPubMed::article_to_df,
                   max_chars = -1, getAuthors = FALSE) %>%
    dplyr::mutate(date = lubridate::make_date(as.integer(year), as.integer(month), as.integer(day)))
}

#' Calculate NSQIP search hits cumulative sum
#'
#' This function takes a data frame of search hits from a PubMed query and
#' calculates the cumulative sum of hits per year. Meant to be used in conjunction
#' with a data frame returned from the \code{get_nsqip_pubmed} function.
#'
#' @param pubmed a PubMed search data frame. Must have a year column.
#' @param interval filter for the years divisible by this interval.
#'
#' @keywords internal
#'
get_nsqip_pubmed_yearsums <- function(pubmed, interval = 5) {
  pubmed %>%
    dplyr::count(year) %>%
    dplyr::mutate(n = cumsum(n),
                  date = lubridate::make_date(as.integer(year), 12, 15),
                  mod = as.integer(year) %% interval) %>%
    dplyr::filter(mod == 0 | year == as.character(lubridate::year(Sys.Date())))
}

#' Make a graph of cumulative PubMed search hits
#'
#' This graph will chart the cumulative sum of PubMed search hits.
#' Meant to be used with the \code{get_nsqip_pubmed} and \code{get_nsqip_pubmed_yearsums}
#' functions.
#'
#' @param pubmed a PubMed search data frame. Must have a date and n column.
#' @param yearsums a data frame of cumulative sums. Used to add points at
#' specific intervals to the line.
#'
#' @keywords internal
#'
make_nsqip_hits_graph <- function(pubmed, yearsums) {
  pubmed %>%
    dplyr::count(date) %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = n)) +
    ggplot2::ylab("Pubmed 'NSQIP' Search Hits") +
    ggplot2::geom_line(ggplot2::aes(y = cumsum(n))) +
    ggplot2::geom_area(ggplot2::aes(x = date, y = cumsum(n)), fill = "midnightblue", alpha = 0.5) +
    ggplot2::geom_point(data = yearsums, mapping = ggplot2::aes(x = date, y = n)) +
    ggrepel::geom_text_repel(data = yearsums, mapping = ggplot2::aes(x = date, y = n, label = n), direction = "both", hjust = 2, vjust = -0.05) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank())
}

#' Prepare journal titles
#'
#' This function will strip extraneous characters and subtitles from a journal name.
#'
#' @param str a journal name
#'
#' @keywords internal
prep_title <- function(str) {
  stringr::str_remove(str, pattern =":.+|\\..+|\\(.+|=.+") %>%
    trimws() %>%
    tools::toTitleCase() %>%
    unescape_xml()
}

#' Translate XML
#'
#' PubMed searches are returned in XML format. This will convert special characters
#' (such as &amp;) into the correct characters for display.
#'
#' @param str an XML string
#'
#' @keywords internal
unescape_xml <- Vectorize(function(str){
  xml2::xml_text(xml2::read_xml(paste0("<x>", str, "</x>")))
})

#' Count and prepare journals
#'
#' This function groups by and counts journal names. It selects the top \code{num}
#' number of journals. It additionally calculates an angle and horizontal adjustment
#' necessary for use in a radial bar plot.
#'
#' @param pubmed A PubMed search data frame
#' @param num number of top journals to keep in data frame
#'
#' @keywords internal
#'
get_nsqip_pubmed_journals <- function(pubmed, num = 10) {
  pubmed %>%
    dplyr::count(journal) %>%
    dplyr::slice_max(n, n = num) %>%
    dplyr::mutate(journal = prep_title(journal),
                  rank = rank(-n),
                  angle = 90 -360 * (rank-0.5) / nrow(.),
                  hjust = ifelse(angle < -90, 1, 0),
                  angle = ifelse(angle < -90, angle + 180, angle))
}

#' Create circular bar plot of journal hits
#'
#' This function creates a circular bar plot that displays the number of hits
#' from a PubMed search that are published in each journal.
#'
#' @param journals a data frame of journals and number of hits. Should also have
#' parameters for angles and horizontal adjustments for display in a circular plot
#'
#' @keywords internal
#'
make_nsqip_journal_circular <- function(journals) {
  ggplot2::ggplot(journals, ggplot2::aes(x = reorder(journal, -n), y = n, fill = n)) +
    ggplot2::geom_hline(yintercept = seq(0, 200, by = 50), colour = "grey", size = 0.35) +
    ggplot2::geom_bar(stat = "identity",  width=1, color = "black") +
    ggplot2::scale_fill_gradient(low = "white", high = "midnightblue") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.ontop = FALSE,
      panel.grid = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.position = "none",
      plot.margin = ggplot2::unit(c(2.5,2.5,2.5,2.5), "cm")
    ) +
    ggplot2::coord_polar(start = 0, clip = "off") +
    ggplot2::geom_text(data = journals,
                       ggplot2::aes(x = rank, y = 215, label = stringr::str_wrap(journal, 25), hjust = hjust),
                       color="black", fontface="bold",
                       alpha=1, size=3, angle=journals$angle,
                       inherit.aes = FALSE) +
    ggplot2::geom_text(x = 9.5, y = 100, size = 2.5, label = "100", alpha = 0.1) +
    ggplot2::geom_text(x = 9.5, y = 150, size = 2.5, label = "150", alpha = 0.1) +
    ggplot2::geom_text(x = 9.5, y = 200, size = 2.5, label = "200", alpha = 0.1)
}
