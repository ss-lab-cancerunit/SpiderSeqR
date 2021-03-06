% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SpiderSeqR.R
\docType{package}
\name{SpiderSeqR-package}
\alias{SpiderSeqR}
\alias{SpiderSeqR-package}
\title{SpiderSeqR: a tool for integration of SRA and GEO metadata}
\description{
SpiderSeqR integrates two different sources of sequencing metadata 
and automates the search and retrieval of the information.
}
\section{SpiderSeqR functions}{

\itemize{
  \item \code{\link{startSpiderSeqR}} - 
      always run it before using SpiderSeqR
  \item \code{\link{searchForTerm}} - search for experiments of interest
  \item \code{\link{convertAccession}} - convert between accession types
  \item \code{\link{searchForAccession}} - search for specific experiments 
      (output similar to that of \code{\link{searchForTerm}})
  \item \code{\link{rerunSpiderSeqR}} - run the queries again
  \item \code{\link{getDatabaseInformation}} - get basic information 
      on databases (interactive function)
}
}

\author{
\strong{Maintainer}: A. Sozanska \email{amsozanska@gmail.com}

Authors:
\itemize{
  \item C. Fletcher
  \item S. Samarajiwa \email{ss861@cam.ac.uk}
}

}
