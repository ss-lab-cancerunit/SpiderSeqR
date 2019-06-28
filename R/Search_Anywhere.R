

#' Search Anywhere within SRA and GEO databases (under construction!)
#' 
#' @param query_both Search term for both SRA and GEO
#' @param query_sra Search term for SRA only
#' @param query_geo Search term for GEO only
#' @param acc_levels Accession levels at which the search is conducted
#' 
#' @examples 
#' searchAnywhere("*stat3*") #The broadest search
#' searchAnywhere("stat3")
#' searchAnywhere("tp53 OR p53") #Can list synonyms
#' 
#' searchAnywhere ("p53", acc_levels = c("gsm", "gse")) #Only search in GEO
#' 
#' @section Argument requirements:
#' Either query_both or \textbf{both} query_sra and query_geo need to be provided (this is to facilitate column-specific search in the databases; if you wish to search within specific columns, provide query_sra and query_geo with appropriate column names)
#' 
#' @section Accession levels:
#' Each accession level is associated with its own set of information. Sometimes the information is replicated across levels, sometimes it is unique to the level. Only information associated with the specified accession levels will be subject of the search. For example, it is common for study abstracts to mention a lot of gene names or proteins that were not a direct object of the study; by searching everywhere studies with a mere mention of a gene will be included. Restricting accession levels (\code{searchAnywhere(query_both = "p53", acc_levels = c("run", "experiment", "sample", "gsm"))}) will help avoid including these cases. However, always consider using a broader search and comparing the results to the more refined one.
#' 
#' Another use of accession levels is to restrict search to only one database. To do so, only list accession levels specific to one database: SRA (run, experiment, sample, study) or GEO (gsm, gse).
#' 
#' 
searchAnywhere <- function(query_both, query_sra, query_geo, acc_levels = c("run", "experiment", "sample", "study", "gsm", "gse")){
  
  #Checking arguments (either query_both or query_sra AND query_geo)
  if (!missing(query_both)){
    if (!missing(query_sra) | !missing(query_geo)){
      warning("Query_both already provided; query_sra/geo will be ignored")
    }
    query_sra <- query_both
    query_geo <- query_both
  } else if (missing(query_sra) | missing(query_geo)){
    stop("Either query_both or both 'query_sra & query_geo' need to be provided")
  }
  
  
  #------TBC
  # ===*===
  
}


