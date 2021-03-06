% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Search_Anywhere.R
\name{convertCategoriesToLibraryStrategyType}
\alias{convertCategoriesToLibraryStrategyType}
\title{Convert from SRA-GEO Categories}
\usage{
convertCategoriesToLibraryStrategyType(x)
}
\arguments{
\item{x}{Character with a category (can be a vector). 
NOTE: must match exactly (but matching is case insensitive)}
}
\value{
A list with a vector each for SRA_library_strategy and GEO_type
}
\description{
Converts from SRA-GEO Categories to corresponding SRA library_strategy 
and GEO (study) type.
For further details regarding available categories (and their 
corresponding elements), inspect the \code{SRA_GEO_Category_Conversion} 
object or see its documentation page: \code{?SRA_GEO_Category_Conversion}.
}
\examples{
convertCategoriesToLibraryStrategyType("Transcriptome MA")
convertCategoriesToLibraryStrategyType("ChIP-Seq")


}
