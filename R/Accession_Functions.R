# Accession_Functions.R
# Functions related to searching for accessions
#
# Currently includes:
# - searchForAccession - main function that returns df from SRA
# - accessionClassifier - given an accession vector, classifies it into correct class or returns an error if not all elements are matched to the same class
# - vConditionVerifier - grepl'es on the input vector according to any of the regular expressions (an OR operation)

# USES:
# - digitSort - sorting by digits, disregarding preceding letters (NOT INCLUDED!)


#USAGE
#searchForAccession(spider_combined$run_accession[c(1,5,22,35)])

#searchForAccession("GSM522267")

searchForAccession <- function(accession_vector){
  # Args: a character vector with accessions
  #       (needs to completely match to one accession class; no partial matches or mixed classes allowed)
  #
  # Returns: df from SRA with matches to the accession_vector
  #
  
  #------------------------------------------------
  #------------------------------------------------
  #TECHNICALITIES (taken from searchSRA):
  #------------------------------------------------
  #------------------------------------------------
  database_name <- "sra_con"
  sra_table <- "sra"
  
  #sra_columns <- c("experiment_name", "run_attribute", "experiment_accession", "experiment_url_link", "experiment_title", "library_strategy", "library_layout", "sample_alias", "taxon_id", "library_construction_protocol", "run_accession", "study_accession", "run_alias", "experiment_alias", "sample_name", "sample_attribute", "experiment_attribute")
  #sra_columns <- c("experiment_title")
  
  sra_columns <- "*"
  
  sra_columns <- paste(sra_columns, collapse = ", ")
  
  #------------------------------------------------
  
  x <- unique(accession_vector)
  x <- x[digitSort(x)]
  
  accession_class <- accessionClassifier(x)
  search_count <- 0
  accession_df <- data.frame()
  
  if (accession_class %in% c("study_accession", "sample_accession", "experiment_accession", "run_accession")){
    for (a in seq_along(x)){
      query <- paste0("SELECT ", sra_columns, " FROM ", sra_table, " WHERE ", accession_class, " = '", x[a], "'")
      print(query)
      chunk <- dbGetQuery(get(database_name), query)
      search_count <- search_count + as.integer(dim(chunk)[1]>=1)
      accession_df <- rbind(accession_df, chunk)
    }
  } else if (accession_class == "series_id"){
    #GSE
    for (a in seq_along(x)){
      query <- paste0("SELECT ", sra_columns, " FROM ", sra_table, " WHERE study_alias = '", x[a], "'") #Needs to be an exact search, otherwise will get accessions with additional digits
      print(query)
      chunk <- dbGetQuery(get(database_name), query)
      search_count <- search_count + as.integer(dim(chunk)[1]>=1)
      accession_df <- rbind(accession_df, chunk)
    }
  } else if (accession_class == "gsm"){
    #GSM
    for (a in seq_along(x)){
      
      #LOGIC: 'GSM###_...' in run_alias OR 'GSM###' in run_alias OR '...GEO Accession: GSM###' in experiment_attribute OR '...GEO Accession: GSM### ...' in experiment_attribute 
      #BEST QUERY (but slow)
      #query <- paste0("SELECT ", sra_columns, " FROM ", sra_table, " WHERE (run_alias LIKE '", x[a], "' OR run_alias LIKE '", x[a], "_%') OR (experiment_attribute LIKE '%GEO Accession: ", x[a], "' OR experiment_attribute LIKE '%GEO Accession: ", x[a], " %')")
      
      #SIMPLER QUERY (misses about 200 entries atm)
      query <- paste0("SELECT ", sra_columns, " FROM ", sra_table, " WHERE (run_alias LIKE '", x[a], "_%') OR (experiment_attribute LIKE '%GEO Accession: ", x[a], "' OR experiment_attribute LIKE '%GEO Accession: ", x[a], " %')")
      
      print(query)
      chunk <- dbGetQuery(get(database_name), query)
      search_count <- search_count + as.integer(dim(chunk)[1]>=1)
      accession_df <- rbind(accession_df, chunk)
    }
  }
  
  
  print(paste0("Found results for ", search_count, " out of ", length(x), " accession search terms"))
  #print(search_count)
  #print(length(x))
  
  if (search_count!=length(x)){
    warning("Some accessions were not found in the database")
  }
  
  accession_df <- unique(accession_df)
  return(accession_df)
}





accessionClassifier <- function(x){
  accession_regexp <- list(c("[DES]RP"),
                           c("[DES]RS"),
                           c("[DES]RX"),
                           c("[DES]RR"),
                           c("GSE"),
                           c("GSM"))
  
  accession_name <- c("study_accession",
                      "sample_accession", 
                      "experiment_accession",
                      "run_accession",
                      "series_id", #A little tricky to deal with
                      "gsm") #A little tricky to deal with
  
  accession_class <- NA
  
  for (a in seq_along(accession_regexp)){
    if (sum(vConditionVerifier(accession_regexp[[a]], x))==length(x))
      accession_class <- accession_name[a]
  }
  
  if (is.na(accession_class)){
    stop("Input needs to completely match one of the accession classes")
  }
  return(accession_class)
}





vConditionVerifier <- function(regexpr_vector, x){
  rv <- regexpr_vector
  
  out <- rep(FALSE, length(x))
  for (r in seq_along(rv)){
    out <- grepl(rv[r], x) | out
  }
  return(out)
  
}
