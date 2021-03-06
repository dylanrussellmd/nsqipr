% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conv_col.R
\name{conv_col_cols}
\alias{conv_col_cols}
\title{Convert targeted colectomy columns}
\usage{
conv_col_cols(df, filename)
}
\arguments{
\item{df}{a data table to be cleaned}

\item{filename}{the name of the file from which the data table has been read in}
}
\description{
Convert targeted colectomy columns
}
\details{
If the file being processed is a targeted colectomy dataset,
it will be processed by this function. This function determines how data cleaning steps specific
to targeted colectomy dataset files should proceed.
}
\keyword{internal}
