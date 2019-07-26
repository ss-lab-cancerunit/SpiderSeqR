#' Batched query
#' 
#' inputs: vector of accessions, table/query
#' 
#' Split vector into chunks of a given size (n)
#' 
#' Search chunk by chunk
#' 
#' 
#' NOTE: MOVED TO ACCESSION SEARCH FUNCTIONS
database_name <- "geo_con"
table_name <- "gsm"
#acc_vector <- temp_geo_df$gsm
acc_vector <- temp_random1$gsm
col_names <- listGSMFields() # Will have to provide a clause for * outside of the function
#c_size <- 5
#c_size <- 500

acc_vector <- temp_random1$run_accession
acc_vector <- temp_random1$series_id

acc_vector <- acc_vector[!is.na(acc_vector)]

acc_vector <- getGSECounts(temp_random1)$gses


# Arguments
#------------------------
batchedAccSearch(acc_vector, "geo_con", "gsm", col_names, 10)
#------------------------
# Function
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
  
  if (acc_class %in% "series_id"){
    
    # GSE - special treatment (acc_vector elements used twice) ####
    for (a in seq_along(acc_vector_split)){ # For each batch
      
      query_el <- character()
      for (i in seq_along(acc_vector_split[[a]])){ # For each element in batch
        query_el <- paste0(query_el, pre_el, acc_vector_split[[a]][i], mid_el, acc_vector_split[[a]][i], post_el)
      }
      query_el <- substr(query_el, 1, nchar(query_el)-end_char)
      query <- paste0(query_beg, query_el, " )")
      print(query)
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
      print(query)
      chunk <- DBI::dbGetQuery(get(database_name, envir = get(database_env)), query)
      df <- rbind(df, chunk)
    }
  }
  
  return(df)
  
}
