% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Helper_Functions.R
\name{.verifySuperseries}
\alias{.verifySuperseries}
\title{Verify presence of superseries}
\usage{
.verifySuperseries(gse_list)
}
\arguments{
\item{gse_list}{List of GSEs (as they appear in the series_id column, 
i.e. comma-separated if multiple)}
}
\value{
A character vector with GSEs that existed 
in conjuction with other GSEs 
(i.e. suggesting some of them might be superseries); 
NULL if no potential superseries.
Also gives console information on the number of samples with mutliple GSEs 
and the first few GSEs which co-occur with other GSEs 
(i.e. displays beginning of the function output)
}
\description{
Verify presence of superseries
}
\keyword{internal}
