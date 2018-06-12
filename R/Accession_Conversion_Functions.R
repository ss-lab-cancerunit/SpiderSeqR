#
#
#NEW
#searchForAccessionAcrossDBs - initial accessionConverter with more function arguments (i.e. table columns for sra and geo)
#
#CHANGED
#convertAccession (previously accessionConverter) - a wrapper around searchForAccessionAcrossDBs
#Converts a list of accessions (of one type) into all possible accessions within SRA and GEO






#'
#'  Search for accession across databases
#' 
#' \code{searchForAccessionAcrossDBs} classifies accessions, searches for them in their original database (SRA or GEO), then, if conversion is possible, searches for corresponding accessions in the other database (SRA or GEO). If no conversion is possible, the columns from the corresponding database are returned empty (NAs).
#' 
#' 
#' @param acc_vector A vector of accessions \strong{(all must belong to the same type)}
#' @param sra_columns A character vector with names of the columns to be returned from SRA
#' @param geo_columns A character vector with names of the columns to be returned from GEO
#' @return A data frame with the results of the query
#' 
#' @examples
#' searchForAccessionAcrossDBs("GSE45530")
#' 
#' @keywords internal
#' 
searchForAccessionAcrossDBs <- function(acc_vector, sra_columns, geo_columns){

  accession_class <- classifyAccession(acc_vector)

  if (!(accession_class %in% c("gsm", "series_id", "run_accession", "experiment_accession", "sample_accession", "study_accession"))){
    stop("Accession needs to belong to one of the supported classes")
  }

  #=============================================================
  # GEO accession as input
  #=============================================================


  if (accession_class %in% c("gsm", "series_id")){

    #GEO data frame
    if (accession_class == "gsm"){
      geo_df <- searchGEOForGSM(acc_vector, geo_columns)
    }
    if (accession_class == "series_id"){
      geo_df <- searchGEOForGSE(acc_vector, geo_columns)
    }
    
    #TEMP
    .GlobalEnv$temp_geo_df <- geo_df
    #...

    #saveRDS(geo_df, "geo_df.Rda")

    #SRR_GSM data frame
    srr_gsm_df <- searchSRR_GSM(geo_df$gsm)
    #saveRDS(srr_gsm_df, "srr_gsm_df.Rda")
    
    #TEMP
    .GlobalEnv$temp_srr_gsm_df <- srr_gsm_df
    #...

    #SRA data frame
    if(length(srr_gsm_df$run_accession)!=0){ # Only search SRA if there is viable GEO/SRA conversion
      sra_df <- searchSRAForAccession(srr_gsm_df$run_accession, sra_columns)
    } else { # Generate an empty data frame if no results in SRR_GSM
      sra_df <- setNames(data.frame(matrix(ncol = length(sra_columns), nrow = 0)), sra_columns)
    }
    
    #saveRDS(sra_df, "sra_df.Rda")
    
    #TEMP
    .GlobalEnv$temp_sra_df <- sra_df
    #...


    #Merge
    #DOUBLE CHECK IF WANT ALL OR ALL.X ===*===
    geo_srr_gsm_df <- merge(geo_df, srr_gsm_df, by.x = "gsm", by.y = "gsm", all = TRUE)
    geo_srr_gsm_sra_df <- merge(geo_srr_gsm_df, sra_df, by.x = "run_accession", by.y = "run_accession", all = TRUE)
    
    #TEMP
    .GlobalEnv$temp_geo_srr_gsm_df <- geo_srr_gsm_df
    .GlobalEnv$temp_geo_srr_gsm_sra_df <- geo_srr_gsm_sra_df
    #...

    #Rename data frame
    output_df <- geo_srr_gsm_sra_df

  }
  #=============================================================





  #=============================================================
  # SRA accession as input
  #=============================================================
  if (accession_class %in% c("run_accession", "experiment_accession", "sample_accession", "study_accession")){

    #SRA data frame
    sra_df <- searchSRAForAccession(acc_vector, sra_columns)

    #TEMP
    .GlobalEnv$temp_sra_df <- sra_df
    #...

    #SRR_GSM data frame
    srr_gsm_df <- searchSRR_GSM(sra_df$run_accession)

    #TEMP
    .GlobalEnv$temp_srr_gsm_df <- srr_gsm_df
    #...

    #GEO data frame
    if(length(srr_gsm_df$gsm)!=0){ # Only search GEO if there is viable SRA/GEO conversion
      geo_df <- searchGEOForGSM(srr_gsm_df$gsm, geo_columns)
    } else { # Generate an empty data frame if no results in SRR_GSM
      geo_df <- setNames(data.frame(matrix(ncol = length(geo_columns), nrow = 0)), geo_columns)
    }

    #Create an empty data frame with appropriate column names
    #if(dim(geo_df)[1]==0){
      #geo_gsm_columns <- dbListFields(get("geo_con"), "gsm")
    #}

    #TEMP
    .GlobalEnv$temp_geo_df <- geo_df
    #...

    #Merge
    #DOUBLE CHECK IF WANT ALL OR ALL.X ===*===
    sra_srr_gsm_df <- merge(sra_df, srr_gsm_df, by.x = "run_accession", by.y = "run_accession", all = TRUE)

    #TEMP
    .GlobalEnv$temp_sra_srr_gsm_df <- sra_srr_gsm_df
    #...


    sra_srr_gsm_geo_df <- merge(sra_srr_gsm_df, geo_df, by.x = "gsm", by.y = "gsm", all = TRUE)

    #TEMP
    .GlobalEnv$temp_sra_srr_gsm_geo_df <- sra_srr_gsm_geo_df
    #...

    output_df <- sra_srr_gsm_geo_df

  }

  #=============================================================

  return(output_df)

}



#' 
#' Convert between accession types
#'
#' \code{convertAccession} converts a vector of accessions (all belonging into the same accession type) into all possible accession types within SRA and GEO. If no SRA/GEO conversion is possible, all the missing accession types are marked as NAs.
#' 
#' 
#' @param acc_vector A vector of accessions \strong{(all must belong to the same type)}
#' @return A data frame with conversion between all accession types
#' @examples
#' 
#' convertAccession(c("SRP010068", "SRP020088"))
#' 
#' convertAccession("GSE1") # Only in GEO. Takes a while because GSE1... is ubiquitous
#' convertAccession(c("GSE1", "GSE45530")) # Mixed GEO/SRA. Takes a while because GSE1... is ubiquitous
#' 
#' convertAccession("ERP016268") # Only in SRA
#' convertAccession(c("ERP016268", "SRP020088")) # Mixed SRA/GEO
#' 
#' 
#' 
#' 
#' 
#' @section Accepted Accession Types:
#' \code{convertAccession} accepts any of the 4 SRA or 2 GEO accession types (see section \emph{'Background Information on Accession Types')}. 
#' \code{convertAccession} accepts only one accession type at a time. 
#' 
#' For example, the following queries are NOT allowed: \code{convertAccession("SRR_____", "SRP_____")}, \code{convertAccession("GSE_____", "SRP_____")}. In order to obtain the above results, it is necessary to run separate queries for each accession type, and, if desirable, bind the data frames together (e.g. \code{rbind(convertAccession("SRR_____"), convertAccession("SRP_____"))}).
#' 
#' SRA accessions differing by the first letter belong to the same type, hence it is possible to run: \code{convertAccession("SRP_____", "ERP_____")}
#' 
#' 
#' 
#' @section Output format:
#' The function outputs a data frame with conversion of the input accessions into all possible types.
#' 
#' In the best case scenario, i.e. if an accession exists in both SRA and GEO databases, these would include all 6 accession types (SRR, SRX, SRS, SRP, GSM, GSE).
#' 
#' If an accession exists only in one of the databases, the conversion will be limited to that one database. For example, if an accession only exists in SRA, only SRA accessions will be provided, whilst the GEO columns will be populated with NAs.
#' 
#' 
#' 
#' @section Troubleshooting:
#' The conversion between SRA and GEO databases is based on a custom database generated by \code{startSpideR()} function. To ensure best results, make sure that the most up to date versions of the databases. To improve results, you can do the following:
#' \enumerate{
#'     \item Download the most up to date versions of SRAmetadb.sqlite and GEOmetadb.sqlite files - this is done by running \code{\link{startSpideR}}, specifying an appropriate argument for expiry period of database files (e.g. \code{startSpideR(dir = getwd(), general_expiry = 1)})
#'     \item Generate a fresh custom database for conversion between accessions (SRR_GSM.sqlite) - this is also done by running \code{\link{startSpideR}}, specifying an appropriate argument for expiry period of the database file
#'     \item As a last resort, manually search for the missing conversions online
#' }
#' 
#' NOTE: because the SRR_GSM.sqlite database is machine-generated, there is some risk that it might not include some conversions in case they have been recorded in the database in a non-standard way. If in doubt, it is worth checking the accession page online.
#' However, users should be aware that the overlap between SRA and GEO is only about 20% (at the time of writing), so most entries will not have corresponding accession numbers in the other database
#' 
#' 
#' 
#' @section Background Information on Accession Types:
#' The two lists below include accession types within SRA and GEO respectively. 
#' 
#' All of these are supported by the convertAccession function.
#' 
#' \strong{SRA}
#' \enumerate{
#'     \item SRP(/DRP/ERP) - project_accession
#'     \item SRS(/DRS/ERS) - sample_accession
#'     \item SRX(/DRX/ERX) - experiment_accession
#'     \item SRR(/DRR/ERR) - run_accession
#' }
#' 
#' NOTE: depending on the location of the database (NCBI, EBI or DDBJ), these accessions might begin with a different letter (S, E or D), so the accession levels can be either SRP/SRX/SRS/SRR or ERP/ERX/ERS/ERR or DRP/DRX/DRS/ERR. Accessions beginning with 'S' are by far the most common.
#' 
#' 
#' \strong{GEO}
#' \enumerate{
#'     \item GSE - series_id
#'     \item GSM - sample
#' }
#' 
#' NOTE: GEO accession system is further complicated by existence of 'superseries', which act as higher level series. In these cases a given GSM would belong to multiple (at least two) GSEs - its series_id and superseries.
#' 
#' @export
#' 
convertAccession <- function(acc_vector){

  geo_columns <- c("gsm", "series_id")
  #srr_gsm_columns <- c("gsm", "gsm_check", "run_accession") # Not needed (searchSRR_GSM has defaults)
  sra_columns <- c("run_accession", "experiment_accession", "sample_accession", "study_accession")

  output_df <- searchForAccessionAcrossDBs(acc_vector, geo_columns = geo_columns, sra_columns = sra_columns)

  # Reorder columns (order always the same, regardless of accession type)
  #output_df <- output_df[ , c("run_accession", "experiment_accession", "sample_accession", "study_accession", "gsm", "series_id", "gsm_check")]
  output_df <- output_df[ , c("run_accession", "experiment_accession", "sample_accession", "study_accession", "gsm", "series_id")] # Remove gsm_check
  
  order_columns <- list(output_df$study_accession, 
                        output_df$sample_accession,
                        output_df$experiment_accession,
                        output_df$run_accession,
                        output_df$series_id,
                        output_df$gsm)
  output_df <- output_df[orderAccessions(order_columns), ]
  
  return(output_df)

}




