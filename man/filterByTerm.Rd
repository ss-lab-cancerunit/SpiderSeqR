% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filterByTerm.R
\name{filterByTerm}
\alias{filterByTerm}
\title{Filter data frame by term}
\usage{
filterByTerm(df, query, filter_columns = NULL)
}
\arguments{
\item{df}{Data frame}

\item{query}{Character string for fts search}

\item{filter_columns}{Character vector with column names}
}
\value{
A data frame subset according to query conditions
}
\description{
Filter a data frame using full-text search within the data frame. 
If filter_columns are not specified, search will be performed 
within the whole data frame. 
If filter_columns are provided, 
search will only be limited to these columns.
Full-text search works akin to searching within 
web search engines like Google. 
You can find out more about it here: https://www.sqlite.org/fts3.html 
(fts3 is used for consistency with fts search within the SRAdb database).
}
\examples{
startSpiderSeqRDemo()
df <- searchForAccession("SRP026280")

# Filter by 'TRIMKD' anywhere within df
df <- filterByTerm(df, "TRIMKD")
# NOTE: here (as is often the case), the study-level columns
# contain information pertaining to all samples and make finding relevant
# samples impossible

# Filter by 'TRIMKD' only within SRA_experiment_title column
df <- filterByTerm(df, "SRA_experiment_title: TRIMKD")
# NOTE: you can explore further possibilities by filtering by columns
# corresponding to different accession levels, 
# see \code{\link{filterByTermByAccessionLevel}}

# For ease of viewing results
df_view <- selectColumns_Overview(df) 

}
\seealso{
Other Workflow functions: 
\code{\link{addMissingSamples}()},
\code{\link{convertAccession}()},
\code{\link{filterByTermByAccessionLevel}()},
\code{\link{searchForAccession}()}

Other Manipulation functions: 
\code{\link{filterByTermByAccessionLevel}()},
\code{\link{orderDFAccessions}()}
}
\concept{Manipulation functions}
\concept{Workflow functions}
