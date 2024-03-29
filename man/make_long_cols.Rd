% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-long.R
\name{make_long_cols}
\alias{make_long_cols}
\title{Create multiple columns from comma-separated column}
\usage{
make_long_cols(df, from, to)
}
\arguments{
\item{df}{the dataframe in which the current comma-separated column is stored.}

\item{from}{a character vector representing the name of the comma-separated column.}

\item{to}{a character vector of new column names into which the comma-separated column
will be split.}
}
\description{
Given a single column containing multiple comma-separated values,
this function will create new columns and split the comma-separated values.
}
\details{
This function will create new columns in the data frame but it will not remove
the original column (the \code{from} column).
}
\examples{
#' @examples
x <- data.table::data.table(
hep_con_ablation_140101 = c("Microwave ablation", "RFA ablation", "Other ablation",
"Microwave ablation,Other ablation", "RFA ablation,Microwave ablation",
"RFA ablation,Other ablation", "RFA ablation,Alcohol ablation", "Alcohol ablation",
"Microwave ablation,Alcohol ablation", "Cryoablation", NA, "RFA ablation,Cryoablation"))

hep_invasive_type_cols <- paste("hep_invasive_type", 1:5, sep = "")

make_long_cols(x, "hep_con_ablation_140101", hep_invasive_type_cols)

}
\keyword{internal}
