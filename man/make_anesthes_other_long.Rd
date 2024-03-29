% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conv_acs.R
\name{make_anesthes_other_long}
\alias{make_anesthes_other_long}
\title{Convert anesthes_other column from wide to long format}
\usage{
make_anesthes_other_long(df)
}
\arguments{
\item{df}{a data.table

If "anesthes_other" is a column in \code{df}, it will be broken into a long format with
\code{caseid} as the ID variable for joining back to the main table. This is because many of the NSQIP
PUF datasets input multiple values into a single "anesthes_other" column. For example,
"General, Spinal, MAC/IV Sedation" may be an entry in the raw data set. This makes
parsing for patients that received "Spinal" anesthesia at any point, for example, very difficult.

Note that this does not alter the "anesthes" column or include the anesthetic technique stored in the
"anesthes" column in the resulting "anesthes_other" data table.}
}
\value{
a data.table
}
\description{
Convert anesthes_other column from wide to long format
}
\examples{
x <- data.table::data.table(
anesthes_other = c("General","General, Spinal", "General, Spinal, MAC/IV Sedation", NA)
)
nsqipr:::make_anesthes_other_cols(x)

}
