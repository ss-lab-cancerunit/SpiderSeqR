% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-pipe.R
\name{\%>\%}
\alias{\%>\%}
\title{Pipe operator}
\usage{
lhs \%>\% rhs
}
\value{
Nothing (joins operations together)
}
\description{
See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
}
\examples{
data.frame(x=1:3, y=c(2,4,6)) \%>\% summary()
}
\keyword{internal}
