% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-conv.R
\name{coalesce}
\alias{coalesce}
\title{Coalesce two vector}
\usage{
coalesce(new, old)
}
\arguments{
\item{new}{the newer vector to be coalesced into. Will take priority if both
columns have values at the same position.}

\item{old}{the older vector to be coalesced.}
}
\value{
a vector of the type of \code{new}
}
\description{
A simple wrapper around \code{\link[data.table:fcoalesce]{fcoalesce}}.
}
\examples{
x = c(11L, NA, 13L, NA, 15L, NA)
y = c(NA, 12L, 5L, NA, NA, NA)
nsqipr:::coalesce(x, y)

}
\keyword{internal}
