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
searchGEOForGSM <- function(acc_list, geo_columns){

  print("Running searchGEOForGSM")

  #Remove duplicates and order
  acc_list <- unique(acc_list)
  acc_list <- acc_list[digitSort(acc_list)]

  #Stop if list is not valid (i.e. non-gsm entries)
  if (accessionClassifier(acc_list)!="gsm"){
    stop("Only GSMs are allowed")
  }

  #Make sure that the query will not be empty
  if (length(acc_list)==0){
    acc_list <- "nth"
  }

  geo_columns <- paste0(geo_columns, collapse = ", ")
  search_count <- 0
  df <- data.frame()

  #Search for GSMs
  for (a in seq_along(acc_list)){
    query <- paste0("SELECT ", geo_columns, " FROM gsm WHERE gsm = '", acc_list[a], "'")
    print(query)
    chunk <- DBI::dbGetQuery(get("geo_con"), query)
    search_count <- search_count + as.integer(dim(chunk)[1]>=1)
    df <- rbind(df, chunk)
  }

  #print(colnames(chunk))
  #if (dim(df)[1]==0){
  #  df <- chunk
  #}

  print(paste0("Found results for ", search_count, " out of ", length(acc_list), " accession search terms"))

  if (search_count!=length(acc_list)){
    warning("Some accessions were not found in the database")
  }

  df <- unique(df)


  print("searchGEOForGSM completed")

  return(df)
}
#------------------------------------------------------
#------------------------------------------------------



#------------------------------------------------------
#------------------------------------------------------
searchGEOForGSE <- function(acc_list, geo_columns){

  print("Running searchGEOForGSE")

  #Remove duplicates and order
  acc_list <- unique(acc_list)
  acc_list <- acc_list[digitSort(acc_list)]

  #Stop if list is not valid (i.e. non-gsm entries)
  if (accessionClassifier(acc_list)!="series_id"){
    stop("Only GSEs are allowed")
  }

  #Make sure that the query will not be empty
  if (length(acc_list)==0){
    acc_list <- "nth"
  }

  geo_columns <- paste0(geo_columns, collapse = ", ")
  search_count <- 0
  df <- data.frame()

  #Search for GSMs
  for (a in seq_along(acc_list)){
    query <- paste0("SELECT ", geo_columns, " FROM gsm WHERE series_id LIKE '%", acc_list[a], "' OR series_id LIKE '%", acc_list[a], ",%'")
    print(query)
    chunk <- DBI::dbGetQuery(get("geo_con"), query)
    search_count <- search_count + as.integer(dim(chunk)[1]>=1)
    df <- rbind(df, chunk)
  }

  print(paste0("Found results for ", search_count, " out of ", length(acc_list), " accession search terms"))

  if (search_count!=length(acc_list)){
    warning("Some accessions were not found in the database")
  }

  df <- unique(df)

  print("searchGEOForGSE completed")

  return(df)


}
#------------------------------------------------------
#------------------------------------------------------



#------------------------------------------------------
#------------------------------------------------------
#Previously: searchForAccession. Adapted to only include SRA accessions
searchSRAForAccession <- function(acc_list, sra_columns){
  # Args: a character vector with accessions
  #       (needs to completely match to one accession class; no partial matches or mixed classes allowed)
  #
  # Returns: df from SRA with matches to the acc_list
  #


  print("Running searchSRAForAccession")

  #------------------------------------------------
  #------------------------------------------------
  #TECHNICALITIES (taken from searchSRA):
  #------------------------------------------------
  #------------------------------------------------
  database_name <- "sra_con"
  sra_table <- "sra"

  #sra_columns <- c("experiment_name", "run_attribute", "experiment_accession", "experiment_url_link", "experiment_title", "library_strategy", "library_layout", "sample_alias", "taxon_id", "library_construction_protocol", "run_accession", "study_accession", "run_alias", "experiment_alias", "sample_name", "sample_attribute", "experiment_attribute")
  #sra_columns <- c("experiment_title")
  #sra_columns <- "*"

  sra_columns <- paste(sra_columns, collapse = ", ")

  #------------------------------------------------

  x <- unique(acc_list)
  x <- x[digitSort(x)]

  #Make sure that the query will not be empty
  if (length(acc_list)==0){
    acc_list <- "nth"
  }

  accession_class <- accessionClassifier(x)
  search_count <- 0
  accession_df <- data.frame()

  if (accession_class %in% c("study_accession", "sample_accession", "experiment_accession", "run_accession")){

    query_beg <- paste0("SELECT ", sra_columns, " FROM ", sra_table, " WHERE ", accession_class, " = '")

    for (a in seq_along(x)){
      query <- paste0(query_beg, x[a], "'")
      print(query)
      chunk <- DBI::dbGetQuery(get(database_name), query)
      search_count <- search_count + as.integer(dim(chunk)[1]>=1)
      accession_df <- rbind(accession_df, chunk)
    }
  } else {
    stop("Only SRA accessions are allowed")
  }

  print(paste0("Found results for ", search_count, " out of ", length(x), " accession search terms"))
  #print(search_count)
  #print(length(x))

  if (search_count!=length(x)){
    warning("Some accessions were not found in the database")
  }

  accession_df <- unique(accession_df)


  print("searchSRAForAccession completed")

  return(accession_df)
}
#------------------------------------------------------
#------------------------------------------------------


#------------------------------------------------------
#------------------------------------------------------
searchSRR_GSM <- function(acc_list, srr_gsm_columns = c("run_accession", "gsm", "gsm_check")){


  print("Running searchSRR_GSM")

  accession_class <- accessionClassifier(acc_list)

  #This is a safeguard in case incomplete SRA accessions had equivalents in GEO
  #Can only remove it if it is certain that this is not the case
  if (!(accession_class %in% c("run_accession", "gsm"))){
    stop("Only SRRs and GSMs are accepted")
  }

  #Make sure that the query will not be empty
  if (length(acc_list)==0){
    acc_list <- "nth"
  }

  acc_list <- unique(acc_list)
  acc_list <- acc_list[digitSort(acc_list)]

  srr_gsm_columns <- paste0(srr_gsm_columns, collapse = ", ")

  search_count <- 0
  accession_df <- data.frame()

  query_beg <- paste0("SELECT ", srr_gsm_columns, " FROM srr_gsm WHERE ", accession_class, " = '")

  for (a in seq_along(acc_list)){
    query <- paste0(query_beg, acc_list[a], "'")
    print(query)
    chunk <- DBI::dbGetQuery(get("srr_gsm"), query)
    search_count <- search_count + as.integer(dim(chunk)[1]>=1)
    accession_df <- rbind(accession_df, chunk)
  }


  print(paste0("Found results for ", search_count, " out of ", length(acc_list), " accession search terms"))

  if (search_count!=length(acc_list)){
    warning("Some accessions were not found in the database")
  }

  accession_df <- unique(accession_df)


  print("searchSRR_GSM completed")

  return(accession_df)

}
#------------------------------------------------------
#------------------------------------------------------





