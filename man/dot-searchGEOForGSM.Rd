% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Accession_Search_Functions.R
\name{.searchGEOForGSM}
\alias{.searchGEOForGSM}
\title{Search for GSM in GEO}
\usage{
.searchGEOForGSM(acc_vector, geo_columns, gse_columns)
}
\arguments{
\item{acc_vector}{A character vector with accession numbers}

\item{geo_columns}{A character vector with columns to be returned 
(from within gsm table)}

\item{gse_columns}{A character vector with columns to be returned 
(from within gse table)}
}
\value{
A data frame with results
}
\description{
Search for GSM in GEO
}
\examples{

# startSpiderSeqRDemo()
# .searchGEOForGSM("GSM1173367", "*", "*")

}
\keyword{internal}
