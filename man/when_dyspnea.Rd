% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conv_acs.R
\name{when_dyspnea}
\alias{when_dyspnea}
\title{Parse a column for type of dyspnea}
\usage{
when_dyspnea(vec)
}
\arguments{
\item{vec}{a character vector of values to convert}
}
\value{
a factor vector
}
\description{
Parse a column for type of dyspnea
}
\details{
NSQIP encodes the \code{dyspnea} column as either "no", "at rest", or "moderate exertion".
This function factors the vector for the levels "At rest" and "Moderate exertion".
}
\examples{
 nsqipr:::when_dyspnea(c("at rest","moderate exertion", NA))

}
\keyword{internal}
