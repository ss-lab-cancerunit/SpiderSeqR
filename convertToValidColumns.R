convertToValidColumns <- function(sra_columns=NULL, gsm_columns=NULL, gse_columns=NULL, other_columns=NULL){
  
  # Database connections ####
  sra_database_name <- "sra_con"
  geo_database_name <- "geo_con"
  database_env <- ".GlobalEnv"
  
  # Columns that are not prefixed ####
  sra_acc <- c("run_accession", "experiment_accession", "sample_accession", "study_accession", "submission_accession")
  geo_acc <- c("gsm", "series_id")
  
  # List of other columns ####
  oth_columns <- c("input", "control", 
                   "sa_tissue", "sa_antibody", "sa_gene", "sa_treatment", "sa_remainder", 
                   "ch1_tissue", "ch1_antibody", "ch1_gene", "ch1_treatment", "ch1_remainder",
                   "lane", "mer", "pairedEnd", "n")
  oth_columns <- paste0("OTH_", oth_columns)
  
  
  
  # Create lists of db columns ####
  sra_columns <- DBI::dbListFields(get(sra_database_name, envir = get(database_env)), "sra")
  gsm_columns <- DBI::dbListFields(get(geo_database_name, envir = get(database_env)), "gsm")
  gse_columns <- DBI::dbListFields(get(geo_database_name, envir = get(database_env)), "gse")
  
  sra_columns <- sra_columns[!sra_columns %in% "sra_ID"] # Remove sra_ID
  #sra_columns <- sra_columns[!sra_columns %in% "run_ID"] # Remove run_ID column ===*===
  gse_columns <- gse_columns[!gse_columns %in% "gse"] # Remove gse column
  
  sra_columns[!sra_columns %in% sra_acc] <- paste0("SRA_", sra_columns[!sra_columns %in% sra_acc])
  #sra_columns <- c(sra_acc, sra_columns) # NOTE: order not preserved
  
  gsm_columns[!gsm_columns %in% geo_acc] <- paste0("GSM_", gsm_columns[!gsm_columns %in% geo_acc])
  #gsm_columns <- c(geo_acc, gsm_columns) # NOTE: order not preserved
  
  gse_columns[!gse_columns %in% geo_acc] <- paste0("GSE_", gse_columns[!gse_columns %in% geo_acc])
  #gse_columns <- c(geo_acc, gse_columns) # NOTE: order not preserved
  
  db_columns <- list(SRA = sra_columns, GSM = gsm_columns, GSE = gse_columns, Other = oth_columns)
  return(db_columns)
  
}
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------

if (!is.null(sra_columns)){
  if (length(sra_columns)==1 & sra_columns[1]=="*"){
    sra_columns <- listValidColumns()$SRA
  }
  
  df_columns <- c(df_columns, as.character(unlist(listValidColumns()$SRA)))
}