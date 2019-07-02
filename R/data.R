

#' SRA - GEO Category Conversion Table
#' 
#' 
#' A manually curated set of conversions between SRA library_strategy and GEO (study) type. These categories can be used for convenience of searching for terms using SpideR functions. 
#' 
#' The categories were created with the following principles in mind:
#' \itemize{
#'     \item Only the most common library_strategy and type elements are used
#'     \item Corresponding library_strategy and type elements are matched whenever possible (but note that some have no equivalents)
#'     \item Microarray and Next Generation Sequencing methods are assigned to separate categories, ending with MA and NGS respectively
#'     \item Methods investigating similar molecular aspect are grouped together (e.g. different methods for investigating methylation)
#'     
#' }
#' 
#' 
#'
#'
"SRA_GEO_Category_Conversion"


