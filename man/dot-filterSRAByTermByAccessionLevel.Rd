% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Search_Anywhere.R
\name{.filterSRAByTermByAccessionLevel}
\alias{.filterSRAByTermByAccessionLevel}
\title{Filter df according to query matches 
only within accession levels of interest}
\usage{
.filterSRAByTermByAccessionLevel(
  query,
  df,
  acc_levels = c("run", "experiment", "sample")
)
}
\arguments{
\item{query}{Query to be passed to MATCH operator (for fts)}

\item{df}{Data frame to be filtered}

\item{acc_levels}{Accession levels to search within 
(choose from: run, experiment, sample, study; 
defaults to c("run", "experiment", "sample"))}
}
\value{
Filtered df (containing only rows matching query 
    within specified accession levels)
}
\description{
Performs fts search on the data frame according to the query, only searching
in the columns corresponding to specified accession levels of interest
}
\keyword{internal}
