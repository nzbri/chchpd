% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_data.R
\name{import_HADS}
\alias{import_HADS}
\title{Import Hospital Anxiety and depression Scale (HADS) data.}
\usage{
import_HADS(concise = TRUE)
}
\arguments{
\item{concise}{If \code{TRUE}, return only the Anxiety and Depression sub-
totals. If \code{FALSE}, also return all individual item scores.}
}
\value{
A dataframe containing the HADS data.
}
\description{
\code{import_HADS} Return the subtotals for HADS anxiety and HADS depression.
}
\details{
This data is held in a fixed Google Sheet and so no parameters are
necessary for this function.
}
\examples{
\dontrun{
hads <- import_HADS()
}
}
