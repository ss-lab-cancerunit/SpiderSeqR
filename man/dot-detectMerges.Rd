% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Helper_Functions.R
\name{.detectMerges}
\alias{.detectMerges}
\title{Detect merges in a sample sheet}
\usage{
.detectMerges(df, do_nothing = FALSE)
}
\arguments{
\item{df}{Data frame (must have experiment_accession column; 
also, number of rows in df must correspond to number of SRRs)}

\item{do_nothing}{Logical indicating whether, 
after creating relevant columns, merge detection should be performed 
(if FALSE, fills these columns with NAs)}
}
\value{
Original data frame with added columns: \itemize{
    \item n - count of SRRs within that SRx
    \item lane - index of runs within a lane (1:n for each SRX)
    \item mer - indication on how to merge runs \itemize{
        \item "" - no merging required
        \item SRX... - merge runs with corresponding SRXs
    }
}
}
\description{
Detect merges in a sample sheet
}
\keyword{internal}
