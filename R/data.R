

#' SRA - GEO Category Conversion Table
#' 
#' 
#' A manually curated set of conversions between SRA library_strategy 
#' and GEO (study) type. 
#' These categories can be used for convenience of searching for terms 
#' using SpiderSeqR functions. 
#' 
#' The categories were created with the following principles in mind:
#' \itemize{
#'     \item Only the most common library_strategy and type elements are used
#'     \item Corresponding library_strategy and type elements are matched 
#'     whenever possible (but note that some have no equivalents)
#'     \item Microarray and Next Generation Sequencing methods are assigned 
#'     to separate categories, ending with MA and NGS respectively
#'     \item Methods investigating similar molecular aspect are grouped 
#'     together (e.g. different methods for investigating methylation)
#'     
#' }
#' 
#' 
#' @docType data
#' @usage data(SRA_GEO_Category_Conversion)
#'
"SRA_GEO_Category_Conversion"



#' DEMO extract from sra table (from sqlite file of SRAdb package)
#' 
#' A data frame created as a DEMO for sra table from sqlite file 
#' of SRAdb package. 
#' Contains a few arbitrarily chosen complete study accessions.
#' 
#' @description 
#' Study accessions included in the data frame
#' \itemize{
#'  \item SRP134708
#'  \item DRP003157
#'  \item SRP061795
#'  \item SRP029758
#'  \item SRP076433
#'  \item SRP148363
#'  \item SRP062911
#'  
#' }
#' 
#' @docType data
#' @usage data(sra_demo)
#'
"sra_demo"


#' DEMO extract from srr_gsm table (from custom sqlite database)
#' 
#' A data frame created as a DEMO for srr_gsm table from custom sqlite database
#'  which stores conversions between SRA and GEO 
#'  (for samples which exist in both databases). 
#'  Samples include those study accessions/series that exist 
#'  within SRA's \code{sra_demo} 
#'  and GEO data frames (\code{gse_demo}, \code{gsm_demo}).
#' 
#' @docType data
#' @usage data(srr_demo)
#' 
"srr_demo"


#' DEMO extract from gse table (from sqlite file of GEOmetadb package)
#' 
#' 
#' A data frame created as a DEMO for gse table 
#' from sqlite file of GEOmetadb package. 
#' Contains a few arbitrarily chosen complete series 
#' (including superseries and subseries when appropriate).
#' 
#' @description 
#' Series included in the data frame:
#' \itemize{
#'  \item GSE48253
#'  \item GSE69001
#'  \item GSE27360
#'  \item GSE82246
#'  \item GSE36467
#'  \item GSE76553
#'  \item GSE10309
#'  \item GSE80767
#'  
#' }
#' @docType data
#' @usage data(gse_demo)
"gse_demo"



#' DEMO extract from gsm table (from sqlite file of GEOmetadb package)
#' 
#' A data frame created as a DEMO for gsm table 
#' from sqlite file of GEOmetadb package. 
#' Contains all samples from a few arbitrarily chosen series 
#' (including superseries and subseries when appropriate).
#'  
#' @description 
#' Series included in the data frame:
#' \itemize{
#'  \item GSE48253
#'  \item GSE69001
#'  \item GSE27360
#'  \item GSE82246
#'  \item GSE36467
#'  \item GSE76553
#'  \item GSE10309
#'  \item GSE80767
#'  
#' }
#'@docType data
#' @usage data(gsm_demo)
"gsm_demo"

