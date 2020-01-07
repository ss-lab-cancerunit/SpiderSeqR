#'SRAxGEOspideR: a tool for integration of SRA and GEO metadata
#'
#' SpideR integrates two different sources of sequencing metadata and automates the search and retrieval of the information.
#' 
#' 
#' 
#' 
#' 
#' @section SRAxGEOspideR functions:
#' \itemize{
#'   \item \code{\link{startSpideR}} - always run it before using SpideR
#'   \item \code{\link{searchForTerm}} - search for experiments of interest
#'   \item \code{\link{convertAccession}} - convert between accession types
#'   \item \code{\link{searchForAccession}} - search for specific experiments (output similar to that of \code{\link{searchForTerm}})
#'   \item \code{\link{rerunSpideR}} - run the queries again
#'   \item \code{\link{getDatabaseInformation}} - get basic information on databases (interactive function)
#' }
#' 
#' 
#' 
#' @section Things to include here ===*===:
#' 
#' \itemize{
#'   \item How to access vignettes -  Example: `vignette("roxygen2", package = "roxygen2")`, `vignette("rd", package = "roxygen2")`, `vignette("namespace", package = "roxygen2")`
#'   \item Explicit links to information on setup
#'   \item List of databases accessed by SpideR
#'   \item Main functions (under development)
#' }
#'
#' @docType package
"_PACKAGE"