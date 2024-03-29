% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-long.R
\name{extract_number_from_column}
\alias{extract_number_from_column}
\title{Extract number from value}
\usage{
extract_number_from_column(df, col)
}
\arguments{
\item{df}{a data.table}

\item{col}{column from which to extract a number}
}
\description{
This function takes a character vector of the form 'name1' where '1' may be any number.
It extracts the number and resets that character vector to just the number. It then
converts the number to an integer.
}
\keyword{internal}
