% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Output_Functions.R
\name{.generateSampleSheet_RNA}
\alias{.generateSampleSheet_RNA}
\title{Generate RNA sample sheet}
\usage{
.generateSampleSheet_RNA(df)
}
\arguments{
\item{df}{Data frame for export}
}
\value{
A data frame with only relevant columns for export
    (compatible with the pipeline)
}
\description{
In comparison to ChIP: does not have tissue, input and macsGroup columns
}
\keyword{internal}
