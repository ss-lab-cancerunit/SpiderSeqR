% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/countAccessionMatches.R
\name{.countAccessionMatches}
\alias{.countAccessionMatches}
\title{Count accession matches within a data frame}
\usage{
.countAccessionMatches(acc_vector, df, output = "logical")
}
\arguments{
\item{acc_vector}{A character vector with the accessions 
to be found within data frame}

\item{df}{Data frame to be checked (needs to contain at least 
the column corresponding to accession type that is searched for)}

\item{output}{A character indicating the type of output, 
can be \code{"logical"} (returns \code{TRUE} if all 
(non-NA, non-empty, unique) acc_vector elements are present 
in the data frame) or \code{"numeric"} 
(returns a numerical vector with the count of matches and the length of 
(non-NA, non-empty, unique) acc_vector)}
}
\value{
Depends on the output type (see above)
}
\description{
Check whether all accessions are present within a data frame. 
Ignores empty strings NAs and repeated elements within the acc_vector.
}
\keyword{internal}
