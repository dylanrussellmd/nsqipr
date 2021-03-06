% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-conv.R
\name{conv_complication}
\alias{conv_complication}
\title{Convert complication columns to logicals}
\usage{
conv_complication(vec)
}
\arguments{
\item{vec}{a character vector to convert complication columns}
}
\value{
a logical vector
}
\description{
Convert complication columns to logicals
}
\details{
Matches the case-insensitive fixed string \code{"no complication"} and negates the result.
Any string that does not match will return TRUE. NA will return NA.
}
\examples{

x <- data.frame(
x = rep("no complication", 10),
y = rep("NO COMPLICATION", 10),
z = rep("complication", 10),
xx = rep("", 10),
yy = rep(NA, 10)
)

lapply(x , nsqipr:::conv_complication)

}
\keyword{internal}
