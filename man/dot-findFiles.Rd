% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/startSpiderSeqR.R
\name{.findFiles}
\alias{.findFiles}
\title{Find files (a wrapper around list.files)}
\usage{
.findFiles(path, pattern)
}
\arguments{
\item{path}{A path to be searched}

\item{pattern}{Regular expression pattern to search for (passed to dir())}
}
\value{
A full path with the matching file(s)
}
\description{
Find files (a wrapper around list.files)
}
\examples{
#.findFiles(getwd(), "*.sqlite")

}
\keyword{internal}
