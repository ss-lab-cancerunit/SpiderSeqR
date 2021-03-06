% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Output_Functions.R
\name{.generateSampleSheet_Other}
\alias{.generateSampleSheet_Other}
\title{Generate 'other' sample sheet}
\usage{
.generateSampleSheet_Other(df)
}
\arguments{
\item{df}{Data frame for export}
}
\value{
A data frame with only relevant columns for export
}
\description{
Currently same format 
as \code{\link{.generateSampleSheet_RNA}}. 
In comparison to ChIP sample sheet, 
it does not have tissue, input and macsGroup columns
}
\keyword{internal}
