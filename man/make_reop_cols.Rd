% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conv_acs.R
\name{make_reop_cols}
\alias{make_reop_cols}
\title{Create reoperation columns for long conversion}
\usage{
make_reop_cols(df)
}
\arguments{
\item{df}{a data table to add the columns to}
}
\description{
Create reoperation columns for long conversion
}
\details{
First checks if the data table contains any of the reoperation columns.
If so, the rest are created as needed. New columns are set to a value of NA.
}
\examples{
x <- data.table::data.table(reoperation1 = TRUE)
nsqipr:::make_reop_cols(x)
x

}
\keyword{internal}
