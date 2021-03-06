% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Output_Functions.R
\name{selectColumns_Accession}
\alias{selectColumns_Accession}
\title{Select columns with accession information}
\usage{
selectColumns_Accession(df)
}
\arguments{
\item{df}{Data frame}
}
\value{
Original data frame containing 
    only columns with accession information
}
\description{
This is a shortcut function 
for \code{selectColumns(df, cols = listColumnSets()$Accession)}
}
\examples{
startSpiderSeqRDemo()
df <- searchForAccession("SRR2187611", file_output = FALSE)
df_sel <- selectColumns_Accession(df)

}
