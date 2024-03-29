% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conv_app.R
\name{conv_app_img_mri}
\alias{conv_app_img_mri}
\title{Parse entries that indicate an MRI was performed}
\usage{
conv_app_img_mri(vec)
}
\arguments{
\item{vec}{a character vector to parse}
}
\value{
a logical vector
}
\description{
Parse entries that indicate an MRI was performed
}
\details{
returns TRUE if case-insensitive "result"
is detected in the character vector.
}
\examples{
x <- c("MRI / Other Definitive Imaging Modality Not Performed / Not Documented","Result Indeterminate / Uncertain",
"Result consistent w/ diagnosis of Appendicitis",'Result Not Consistent w/ Appendicitis; Appendix "Normal"', NA)

cbind(x, nsqipr:::conv_app_img_mri(x))

}
\keyword{internal}
