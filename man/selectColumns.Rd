% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Output_Functions.R
\name{selectColumns}
\alias{selectColumns}
\title{Select columns from a data frame}
\usage{
selectColumns(df, cols)
}
\arguments{
\item{df}{Data frame}

\item{cols}{Character vector with column names to be retained}
}
\value{
Original data frame containing only columns specified in cols
}
\description{
This is a universal function for extracting only columns of interest 
from a data frame. Related functions exist, 
with pre-defined sets of columns, 
see documentation for \code{\link{selectColumns_Accession}} 
and \code{\link{selectColumns_Overview}}.
}
\examples{

startSpiderSeqRDemo()
df <- searchForAccession("SRR2187611", file_output = FALSE)
df_sel <- selectColumns(df, c("run_accession", 
                              "study_accession", 
                              "SRA_experiment_title", 
                              "SRA_sample_attribute"))


}
