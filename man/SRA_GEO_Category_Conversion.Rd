% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{SRA_GEO_Category_Conversion}
\alias{SRA_GEO_Category_Conversion}
\title{SRA - GEO Category Conversion Table}
\format{
An object of class \code{data.frame} with 41 rows and 3 columns.
}
\usage{
data(SRA_GEO_Category_Conversion)
}
\description{
A manually curated set of conversions between SRA library_strategy 
and GEO (study) type. 
These categories can be used for convenience of searching for terms 
using SpiderSeqR functions.
}
\details{
The categories were created with the following principles in mind:
\itemize{
    \item Only the most common library_strategy and type elements are used
    \item Corresponding library_strategy and type elements are matched 
    whenever possible (but note that some have no equivalents)
    \item Microarray and Next Generation Sequencing methods are assigned 
    to separate categories, ending with MA and NGS respectively
    \item Methods investigating similar molecular aspect are grouped 
    together (e.g. different methods for investigating methylation)
    
}
}
\keyword{datasets}
