% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-tar.R
\name{conv_unplanned_conversion}
\alias{conv_unplanned_conversion}
\title{Parse entries with unplanned conversion to open}
\usage{
conv_unplanned_conversion(vec)
}
\arguments{
\item{vec}{a character vector to parse}
}
\value{
a logical vector
}
\description{
Parse entries with unplanned conversion to open
}
\details{
returns TRUE if case-insensitive "w/ unplanned conversion to open"
is detected in the character vector. Cases with an "open" or "open (planned)"
approach will return NA.
}
\examples{
x <- c("Endoscopic","Endoscopic w/ open assist","Endoscopic w/ unplanned conversion to open",
"Hybrid","Hybrid w/ open assist","Hybrid w/ unplanned conversion to open",
"Laparoscopic","Laparoscopic w/ open assist","Laparoscopic w/ unplanned conversion to open",
"Laparoscopic hand assisted","NOTES","NOTES w/ open assist","NOTES w/ unplanned conversion to open",
"Open","Open (planned)","Other","Other MIS approach","Other MIS approach w/ open assist",
"Other MIS approach w/ unplanned conversion to open","Robotic","Robotic w/ open assist",
"Robotic w/ unplanned conversion to open","SILS","SILS w/ open assist",
"SILS w/ unplanned conversion to open", NA)

cbind(x, nsqipr:::conv_unplanned_conversion(x))

}
\keyword{internal}
