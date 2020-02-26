# Accession_Search_Functions.R
#
#Functions for searching for accessions within the three databases: GEO, SRA and SRR_GSM
#These functions are used by higher level functions such as searchForAccessionAcrosDBs, convertAccession and searchForAccession
#
#CONTAINS:
# - searchGEOForGSM()
# - searchGEOForGSE()
#
# - searchSRAForAccession()
#
# - searchSRR_GSM()





#DATABASE CONNECTIONS ===*=== Come back and redo
#.GlobalEnv$sra_con <- dbConnect(SQLite(), dbname = 'SRAmetadb.sqlite')
#.GlobalEnv$geo_con <- dbConnect(SQLite(),'GEOmetadb.sqlite')
#.GlobalEnv$srr_gsm <- dbConnect(SQLite(),'SRR_GSM.sqlite')


#------------------------------------------------------
#------------------------------------------------------
searchGEOForGSM <- function(acc_vector, geo_columns, gse_columns){
  mm("Running searchGEOForGSM", "fn")
  
  database_name <- "geo_con"
  database_env <- ".GlobalEnv"
  
  #Remove duplicates and order
  acc_vector <- unique(acc_vector)
  acc_vector <- acc_vector[orderAccessions(acc_vector)]
  
  #Stop if list is not valid (i.e. non-gsm entries)
  if (classifyAccession(acc_vector)!="gsm"){
    stop("Only GSMs are allowed")
  }
  
  #Make sure that the query will not be empty
  if (length(acc_vector)==0){
    acc_vector <- "nth"
  }
  
  # Make sure that series_id and gsm are among the output columns
  if (!("series_id" %in% geo_columns) | !("gsm" %in% geo_columns)){
    if (!(length(geo_columns) == 1 & geo_columns[1] == "*")){
      geo_columns <- c("gsm", "series_id", geo_columns)
      geo_columns <- unique(geo_columns)
      message("Added gsm and series_id columns to facilitate merging with gse table")
    }
  }
  
  
  #geo_columns <- paste0(geo_columns, collapse = ", ") # This is dealt with by batchedAccSearch()
  search_count <- 0
  #df <- data.frame()
  
  #Search for GSMs
  df <- batchedAccSearch(acc_vector = acc_vector, database_name = database_name, table_name = "gsm", col_names = geo_columns)
  

  # Rename GSM columns
  df <- renameGSMColumns(df)
  
  #print(colnames(chunk))
  #if (dim(df)[1]==0){
  #  df <- chunk
  #}
  
  df <- appendGSEColumns(df, gse_columns)
  
  # Implement a solution for counting matches ===*===
  #print(paste0("Found results for ", search_count, " out of ", length(acc_vector), " accession search terms"))
    #if (search_count!=length(acc_vector)){
  #  warning("Some accessions were not found in the GEO database")
  #}
  
  df <- unique(df)
  
  
  mm("searchGEOForGSM completed", "fn")
  
  return(df)
}
#------------------------------------------------------
#------------------------------------------------------


#------------------------------------------------------
#------------------------------------------------------
searchGEOForGSE <- function(acc_vector, geo_columns, gse_columns){
  mm("Running searchGEOForGSE", "fn")
  
  database_name <- "geo_con"
  database_env <- ".GlobalEnv"
  
  #Remove duplicates and order
  acc_vector <- unique(acc_vector)
  acc_vector <- acc_vector[orderAccessions(acc_vector)]
  
  #Stop if list is not valid (i.e. non-gsm entries)
  if (classifyAccession(acc_vector)!="series_id"){
    stop("Only GSEs are allowed")
  }
  
  #Make sure that the query will not be empty
  if (length(acc_vector)==0){
    acc_vector <- "nth"
  }
  
  # Make sure that series_id and gsm are among the output columns
  if (!("series_id" %in% geo_columns) | !("gsm" %in% geo_columns)){
    if (!(length(geo_columns) == 1 & geo_columns[1] == "*")){
      geo_columns <- c("gsm", "series_id", geo_columns)
      geo_columns <- unique(geo_columns)
      message("Added gsm and series_id columns to facilitate merging with gse table")
    }
  }
  
  
  #geo_columns <- paste0(geo_columns, collapse = ", ") # This is dealt with by batchedAccSearch()
  search_count <- 0
  #df <- data.frame()
  
  #Search for GSEs
  df <- batchedAccSearch(acc_vector = acc_vector, database_name = database_name, table_name = "gsm", col_names = geo_columns)
  
  # Rename GSM columns
  df <- renameGSMColumns(df)
  
  df <- appendGSEColumns(df, gse_columns)
  
  # Implement a solution for counting matches ===*===
  #print(paste0("Found results for ", search_count, " out of ", length(acc_vector), " accession search terms"))
  
  #if (search_count!=length(acc_vector)){
  #  warning("Some accessions were not found in the GEO database")
  #}
  
  df <- unique(df)
  
  mm("searchGEOForGSE completed", "fn")
  
  return(df)
  
  
}
#------------------------------------------------------
#------------------------------------------------------



#------------------------------------------------------
#------------------------------------------------------
#Previously: searchForAccession. Adapted to only include SRA accessions
#simpleSearchSRAForAccession <- function(acc_vector, sra_columns){
searchSRAForAccession <- function(acc_vector, sra_columns){
  # Args: a character vector with accessions
  #       (needs to completely match to one accession class; no partial matches or mixed classes allowed)
  #
  # Returns: df from SRA with matches to the acc_vector
  #


  mm("Running searchSRAForAccession", "fn")

  #------------------------------------------------
  #------------------------------------------------
  #TECHNICALITIES (taken from searchSRA):
  #------------------------------------------------
  #------------------------------------------------
  database_name <- "sra_con"
  database_env <- ".GlobalEnv"
  sra_table <- "sra"

  #sra_columns <- c("experiment_name", "run_attribute", "experiment_accession", "experiment_url_link", "experiment_title", "library_strategy", "library_layout", "sample_alias", "taxon_id", "library_construction_protocol", "run_accession", "study_accession", "run_alias", "experiment_alias", "sample_name", "sample_attribute", "experiment_attribute")
  #sra_columns <- c("experiment_title")
  #sra_columns <- "*"

  #sra_columns <- paste(sra_columns, collapse = ", ") # This is dealt with by batchedAccSearch()

  #------------------------------------------------

  x <- unique(acc_vector)
  x <- x[orderAccessions(x)]

  #Make sure that the query will not be empty
  if (length(acc_vector)==0){
    acc_vector <- "nth"
  }

  accession_class <- classifyAccession(x)
  #search_count <- 0
  #accession_df <- data.frame()
  
  if (!(accession_class %in% c("study_accession", "sample_accession", "experiment_accession", "run_accession"))){
    stop("Only SRA accessions are allowed")
  } 
  
  # Search for SRA accessions
  df <- batchedAccSearch(acc_vector = acc_vector, database_name = database_name, table_name = "sra", col_names = sra_columns)
  

  #print(paste0("Found results for ", search_count, " out of ", length(x), " accession search terms"))
  #print(search_count)
  #print(length(x))

  #if (search_count!=length(x)){
  #  warning("Some accessions were not found in the SRA database")
  #}

  df <- unique(df)
  
  # Rename SRA columns
  df <- renameSRAColumns(df)


  mm("searchSRAForAccession completed", "fn")

  return(df)
}
#------------------------------------------------------
#------------------------------------------------------



#------------------------------------------------------
#------------------------------------------------------
#simpleSearchSRR_GSM <- function(acc_vector, srr_gsm_columns = c("run_accession", "gsm", "gsm_check")){
#searchSRR_GSM <- function(acc_vector, srr_gsm_columns = c("run_accession", "gsm", "gsm_check")){
searchSRR_GSM <- function(acc_vector, srr_gsm_columns = c("run_accession", "gsm")){
  mm("Running searchSRR_GSM", "fn")
  
  database_name <- "srr_gsm"
  database_env <- ".GlobalEnv"

  accession_class <- classifyAccession(acc_vector)

  #This is a safeguard in case incomplete SRA accessions had equivalents in GEO
  #Can only remove it if it is certain that this is not the case
  if (!(accession_class %in% c("run_accession", "gsm"))){
    stop("Only SRRs and GSMs are accepted")
  }

  #Make sure that the query will not be empty
  if (length(acc_vector)==0){
    acc_vector <- "nth"
  }

  acc_vector <- unique(acc_vector)
  acc_vector <- acc_vector[orderAccessions(acc_vector)]

  srr_gsm_columns <- paste0(srr_gsm_columns, collapse = ", ")

  search_count <- 0
  accession_df <- data.frame()

  
  df <- batchedAccSearch(acc_vector = acc_vector, database_name = "srr_gsm", table_name = "srr_gsm", col_names = srr_gsm_columns)
  
  
  #query_beg <- paste0("SELECT ", srr_gsm_columns, " FROM srr_gsm WHERE ", accession_class, " = '")

  #for (a in seq_along(acc_vector)){
  #  query <- paste0(query_beg, acc_vector[a], "'")
  #  print(query)
  #  chunk <- DBI::dbGetQuery(get(database_name, envir = get(database_env)), query)
  #  search_count <- search_count + as.integer(dim(chunk)[1]>=1)
  #  accession_df <- rbind(accession_df, chunk)
  #}


  #print(paste0("Found results for ", search_count, " out of ", length(acc_vector), " accession search terms"))

  #if (search_count!=length(acc_vector)){
  #  warning("Some accessions were not found in the SRR_GSM database. You may wish to re-check for SRA-GEO correspondence manually")
  #}

  df <- unique(df)


  mm("searchSRR_GSM completed", "fn")

  return(df)

}
#------------------------------------------------------
#------------------------------------------------------





#------------------------------------------------------
#------------------------------------------------------
#' List all column names of SRA table
#' 
#' @return Character vector containing column names of SRA table
#' 
#' 
#' @examples
#' listSRAFields()
#'  
#' @keywords internal
#' 
listSRAFields <- function(){
  database_name <- "sra_con"
  database_env <- ".GlobalEnv"
  sra_table <- "sra"
  
  y <- DBI::dbListFields(get(database_name, envir = get(database_env)), sra_table)
  
  return(y)
}

#------------------------------------------------------
#------------------------------------------------------

#------------------------------------------------------
#------------------------------------------------------
#' List all column names of the GSM table
#' 
#' @return Character vector containing column names of GSM table
#' 
#' 
#' @examples 
#' listGSMFields()
#'  
#' @keywords internal
#' 
listGSMFields <- function(){
  database_name <- "geo_con"
  database_env <- ".GlobalEnv"
  geo_table <- "gsm"
  
  y <- DBI::dbListFields(get(database_name, envir = get(database_env)), geo_table)
  
  return(y)
}

#------------------------------------------------------
#------------------------------------------------------



#------------------------------------------------------
#------------------------------------------------------
#' List all column names of the GSE table
#' @param omit_gse Logical indicating whether to omit 'gse' column name from the output
#' @return Character vector containing column names of GSE table (except gse)
#' 
#' 
#' @examples 
#' listGSEFields()
#'  
#' @keywords internal
#' 
listGSEFields <- function(omit_gse = TRUE){
  database_name <- "geo_con"
  database_env <- ".GlobalEnv"
  geo_table <- "gse"
  
  y <- DBI::dbListFields(get(database_name, envir = get(database_env)), geo_table)
  if (omit_gse){
    y <- y[!y %in% "gse"]
  }
  return(y)
}

#------------------------------------------------------
#------------------------------------------------------



#------------------------------------------------------
#------------------------------------------------------
#' Make batched queries to databases
#' 
#' @param acc_vector A character vector with accessions to search for (must belong to the same type)
#' @param database_name A character with the name of the database connection
#' @param table_name A character with the name of the database table
#' @param col_names A character vector with column names to be returned
#' @param c_size Number of items to search for in a batch (sqlite has a limit of 999 parameters within a single query)
#' @return Data frame with results of the query
#' 
#' @keywords internal
#' 
batchedAccSearch <- function(acc_vector, database_name, table_name, col_names, c_size = 500){
  
  database_env <- ".GlobalEnv"
  
  # Classify acc_vector
  acc_class <- classifyAccession(acc_vector)
  
  # Define the strings before and after accession ####
  # GSM
  if (acc_class == "gsm"){
    pre_el <- " gsm = '"
    post_el <- "' OR "
    end_char <- 4
  }
  
  # GSE
  if (acc_class == "series_id"){
    pre_el <- " (series_id LIKE '%"
    mid_el <- "' OR series_id LIKE '%"
    post_el <- ",%') OR "
    end_char <- 4
  }
  
  # SRA
  if (acc_class %in% c("run_accession", "experiment_accession", "sample_accession", "study_accession")){
    pre_el <- paste0(" ", acc_class, " = '")
    post_el <- "' OR "
    end_char <- 4
  }
  
  # Collapse column names for sqlite query
  col_names <- paste0(col_names, collapse = ", ")
  
  # Split the vector into chunks
  acc_vector_split <- split(acc_vector, ceiling(seq_along(acc_vector)/c_size))
  
  df <- data.frame()
  
  # Beginning of the query
  query_beg <- paste0("SELECT ", col_names, " FROM ", table_name, " WHERE (")
  
  
  
  # Query construction and search ####
  
  # Special case for srr_gsm ####
  if (database_name == "srr_gsm"){
    
    for (a in seq_along(acc_vector_split)){ # For each batch
      
      query_el <- character()
      for (i in seq_along(acc_vector_split[[a]])){ # For each element in batch
        query_el <- paste0(query_el, pre_el, acc_vector_split[[a]][i], post_el)
      }
      query_el <- substr(query_el, 1, nchar(query_el)-end_char)
      query <- paste0(query_beg, query_el, " )")
      mm(query, "query")
      chunk <- DBI::dbGetQuery(get(database_name, envir = get(database_env)), query)
      df <- rbind(df, chunk)
    }
    
    return(df)
  }
  
  
  if (acc_class %in% "series_id"){
    
    # GSE - special treatment (acc_vector elements used twice) ####
    for (a in seq_along(acc_vector_split)){ # For each batch
      
      query_el <- character()
      for (i in seq_along(acc_vector_split[[a]])){ # For each element in batch
        query_el <- paste0(query_el, pre_el, acc_vector_split[[a]][i], mid_el, acc_vector_split[[a]][i], post_el)
      }
      query_el <- substr(query_el, 1, nchar(query_el)-end_char)
      query <- paste0(query_beg, query_el, " )")
      mm(query, "query")
      chunk <- DBI::dbGetQuery(get(database_name, envir = get(database_env)), query)
      df <- rbind(df, chunk)
    }
    
  } else if (acc_class %in% c("run_accession", "experiment_accession", "sample_accession", "study_accession", "gsm")){
    
    # GSM, SRA - standard treatment (acc_vector elements used once) ####
    for (a in seq_along(acc_vector_split)){ # For each batch
      
      query_el <- character()
      for (i in seq_along(acc_vector_split[[a]])){ # For each element in batch
        query_el <- paste0(query_el, pre_el, acc_vector_split[[a]][i], post_el)
      }
      query_el <- substr(query_el, 1, nchar(query_el)-end_char)
      query <- paste0(query_beg, query_el, " )")
      mm(query, "query")
      chunk <- DBI::dbGetQuery(get(database_name, envir = get(database_env)), query)
      df <- rbind(df, chunk)
    }
  }
  
  return(df)
  
}
#------------------------------------------------------
#------------------------------------------------------






