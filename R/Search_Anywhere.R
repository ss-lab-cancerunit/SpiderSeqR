

#' Search Anywhere within SRA and GEO databases (under construction!)
#' 
#' @param query_both Search term for both SRA and GEO
#' @param sra_query Search term for SRA only
#' @param geo_query Search term for GEO only
#' @param acc_levels Accession levels at which the search is conducted
#' 
#' @examples 
#' searchAnywhere("*stat3*") #The broadest search
#' searchAnywhere("stat3")
#' searchAnywhere("tp53 OR p53") #Can list synonyms
#' 
#' 
#' searchAnywhere ("p53", acc_levels = c("gsm", "gse")) #Only search in GEO
#' 
#' @section Argument requirements:
#' Either query_both or \strong{both} sra_query and geo_query need to be provided (this is to facilitate column-specific search in the databases; if you wish to search within specific columns, provide sra_query and geo_query with appropriate column names)
#' 
#' @section Accession levels:
#' Each accession level is associated with its own set of information. Sometimes the information is replicated across levels, sometimes it is unique to the level. Only information associated with the specified accession levels will be subject of the search. For example, it is common for study abstracts to mention a lot of gene names or proteins that were not a direct object of the study; by searching everywhere studies with a mere mention of a gene will be included. 
#' 
#' Restricting accession levels, e.g.  
#' 
#' \code{searchAnywhere(query_both = "p53", acc_levels = c("run", "experiment", "sample", "gsm"))}  
#' 
#' will help avoid including these cases. However, always consider using a broader search and comparing the results to the more refined one.
#' 
#' Another use of accession levels is to restrict search to only one database. To do so, only list accession levels specific to one database: SRA (run, experiment, sample, study) or GEO (gsm, gse).
#' 
#' 
#' @section Examples of usage:
#' 
#' 
#' Under construction ===*===
#' 
#' \enumerate{
#'     \item Search for rare types of experiments ('library_strategy: HiC'; 'hic library_strategy: OTHER')
#' }
#' 
searchAnywhere <- function(query_both, category_both=NULL, acc_levels = c("run", "experiment", "sample", "study", "gsm", "gse"), sra_library_strategy=NULL, sra_other_library_strategy = c("OTHER", "NA", "NULL"), geo_type=NULL, sra_query, geo_query, ...){
  
  
  #Checking arguments (either query_both or sra_query AND geo_query)
  if (!missing(query_both)){
    if (!missing(sra_query) | !missing(geo_query)){
      warning("query_both already provided; sra_query/geo will be ignored")
    }
    sra_query <- query_both
    geo_query <- query_both
  } else if (missing(sra_query) | missing(geo_query)){
    stop("Either query_both or both 'sra_query & geo_query' need to be provided")
    # ===*=== Add clause that checks for acc_levels
  }
  
  
  
  
  # Convert sra_library_strategy from a list of synonyms to a canonical form
  if (!is.null(sra_library_strategy)){
    x <- character()
    for (s in seq_along(sra_library_strategy)){
      x[s] <- manageLibraryStrategy(sra_library_strategy[s], input = "syn", output = "can")
    }
    sra_library_strategy <- x
  }
  
  
  # Populate sra_library_strategy and geo_type with converted categories
  if (!is.null(category_both)){
    
    if ( !is.null(sra_library_strategy) | !is.null(geo_type)){
      warning("category_both already provided; sra_library_strategy/geo_type will be ignored")
    }
    
    sra_library_strategy <- convertCategoriesToLibraryStrategyType(category_both)$sra_library_strategy
    geo_type <- convertCategoriesToLibraryStrategyType(category_both)$geo_type
    print(sra_library_strategy)
    print(geo_type)
    
  }
  
  sra_arg_check <- list(...)$sra_arg_check
  if (!is.null(sra_arg_check)){
    searchAnywhereSRA(sra_query, sra_library_strategy, sra_other_library_strategy, acc_levels = acc_levels, sra_arg_check)
  }
  
  
  # Search in SRA if any of the acc_levels are from SRA
  # ===*===
  if (sum(acc_levels %in% c("run", "experiment", "sample", "study"))>0){
    if (!(!is.null(category_both) & length(sra_library_strategy)==0)){ # Don't search if category_both doesn't include SRA
      sra_df <- searchAnywhereSRA(sra_query, acc_levels = acc_levels, sra_library_strategy = sra_library_strategy, sra_other_library_strategy = sra_other_library_strategy) # NOT PASSING ANY OTHER ARGUMENTS HERE ===*===
    }
  }
  
  
  if (sum(acc_levels %in% c("gse", "gsm"))>0){
    if (!(!is.null(category_both) & length(geo_type)==0)){ # Don't search if category_both doesn't include GEO
      geo_df <- searchAnywhereGEO(geo_query, acc_levels = acc_levels, geo_type = geo_type)
    }  
  }

  
  
  #------TBC
  # ===*===
  
  df <- sra_df
  
  
  return(df)
}





searchAnywhereGEO <- function(geo_query, acc_levels = c("gse", "gsm"), geo_type){
  l <- list()
  l$geo_query <- geo_query
  l$acc_levels <- acc_levels
  l$geo_type <- geo_type
  print(l)
}

#------------------------------------------
# ------------------DONE-------------------
#------------------------------------------



#' 
#' Fulltext search in SRA
#' 
#' @param query Query passed to fts MATCH operator
#' @param library_strategy Character vector denoting library_strategy/ies of choice (OPTIONAL)
#' @param sra_other_library_strategy A character vector indicating whether (and which) uncategorised library strategies are accepted (choose between one and all elements of c("OTHER", "NA", "NULL")); if not desired, set equal to FALSE. NOTE: only evaluated if library strategy is provided
#' @param acc_levels Character vector denoting which accession levels will be searched. Choose from some/all of c("run", "experiment", "sample", "study")
#' 
#' @examples 
#' searchAnywhereSRA("stat3") # stat3
#' searchAnywhereSRA("stat3 taxon_id: 9606") # stat3 in human samples
#' searchAnywhereSRA("stat3", library_strategy = "ChIP-Seq")
#' searchAnywhereSRA("stat3", library_strategy = "ChIP-Seq", sra_other_library_strategy = FALSE) # stat3 chip-seq not including unclassified library strategies
#' searchAnywhereSRA("stat3", acc_levels = c("run", "experiment", "sample")) #stat3 ignoring matches at study level
#' 
#' 
#' 
searchAnywhereSRA <- function(query, acc_levels = c("run", "experiment", "sample", "study"), sra_library_strategy=NULL, sra_other_library_strategy = c("OTHER", "NA", "NULL"),  ...){
  
  sra_arg_check <- list(...)$sra_arg_check
  if(!is.null(sra_arg_check)){
    if(isTRUE(sra_arg_check)){
      
      #return(as.list(match.call(def=sys.function(-1), call = sys.call(-1))))
      
      # Previously
      
      #print(as.list(match.call(expand.dots = TRUE)))
      print(as.list(match.call(def=sys.function(-1), call = sys.call(-1))))
      
      l <- list()
      l$query <- query
      l$sra_library_strategy <- sra_library_strategy
      l$sra_other_library_strategy <- sra_other_library_strategy
      l$acc_levels <- acc_levels
      l$dots <- list(...)
      
      return(l)
      
      
      print(paste0("query: ", query))
      print(paste0("sra_library_strategy: ", sra_library_strategy))
      print(paste0("sra_other_library_strategy: ", sra_other_library_strategy))
      print(paste0("acc_levels: ", acc_levels))
      print(paste0("other: ", unlist(list(...))))
      
      #return(as.list(match.call(expand.dots = TRUE)))
      
      
      #argg <- c(as.list(environment()), list(...))
      #return(argg)
      
      
      #l <- as.list(match.call(expand.dots = TRUE))
      #print(l)
      #return(get(as.character((l[3]))))
      
      
      #Didn't work
      #return(as.list(get(as.character(unlist((match.call(expand.dots = TRUE)))))))

    }
  }
  
  database_name <- "sra_con"
  database_env <- ".GlobalEnv"

  query_full <- paste0("SELECT * FROM sra_ft WHERE sra_ft MATCH '", query, "'")
  
  if (!is.null(sra_library_strategy)){
    
    # other library strategy clause
    ls_query <- paste0("library_strategy = '", sra_library_strategy, sep = "' OR ", collapse = "")
    
    
    if( sum(c("OTHER", "NA", "NULL") %in% sra_other_library_strategy) > 0 ){
      ols_clause <- character()
      ols_clause[1] <- "library_strategy = 'OTHER'"
      ols_clause[2] <- "library_strategy = 'NA'"
      ols_clause[3] <- "library_strategy IS NULL"
      
      ols_check <- logical()
      ols_check[1] <- "OTHER" %in% sra_other_library_strategy
      ols_check[2] <- "NA" %in% sra_other_library_strategy
      ols_check[3] <- "NULL" %in% sra_other_library_strategy
      
      # sra_other_library_strategy clause
      ols_query <- paste0(ols_clause[ols_check], sep = " OR ", collapse = "")
      
      ls_query <- paste0(ls_query, ols_query)
      
      print(ls_query)
      
    }
    
    ls_query <- substr(ls_query, 1, nchar(ls_query)-4)
    query_full <- paste0(query_full, " AND ( ", ls_query, ")")
    
  }
  
  print(query_full)
  
  

  
  
  query_check <- list(...)$query_check
  if(!is.null(query_check)){
    if (isTRUE(query_check)){
      return(query_full)
    }
  }

  
  df <- DBI::dbGetQuery(get(database_name, envir = get(database_env)), query_full)
  
  
  df <- filterSRAQueryByAccessionLevel(query, df, acc_levels)
  
  return(df)
  #------TBC
  # ===*===
}








#' Filter df according to query matches only within accession levels of interest
#' 
#' Performs fts search on the data frame according to the query, only searching in the columns corresponding to specified accession levels of interest
#' 
#' 
#' @param query Query to be passed to MATCH operator (for fts)
#' @param df Data frame to be filtered
#' @param acc_levels Accession levels to search within
#' @return Filtered df (containing only rows matching query within specified accession levels)
#' 
#' 
#' 
filterSRAQueryByAccessionLevel <- function(query, df, acc_levels){
  # Select columns within df
  df_filt <- subsetSRAByAccessionLevel(df, acc_levels, add_run_accession = TRUE)
  
  
  # Create db
  filter_db_file <- "filter_db.sqlite"
  
  if (file.exists(filter_db_file)) file.remove(filter_db_file)
  
  .GlobalEnv$filter_con <- DBI::dbConnect(SQLite(), dbname = filter_db_file)
  DBI::dbWriteTable(conn = filter_con, name="filt_sra", value = df_filt)
  
  
  # Create fts table of the db
  createFtsTable("filter_con", "filt_sra", "filt_sra_ft")
  query <- paste0("SELECT run_accession FROM filt_sra_ft WHERE filt_sra_ft MATCH '", query, "'")
  
  out_runs <- DBI::dbGetQuery(.GlobalEnv$filter_con, query)$run_accession
  
  file.remove(filter_db_file)
  rm(filter_con, envir = .GlobalEnv)
  
  df <- df %>% dplyr::filter(run_accession %in% out_runs)
  
  return(df)
}





#' Subset (by column) of a df based on SRA accession levels of interest
#' 
#' @param df Data frame to be subset
#' @param acc_levels Accession levels of interest
#' @return Data frame only with columns corresponding to accession levels of interest
#'
#'
#'
subsetSRAByAccessionLevel <- function(df, acc_levels, add_run_accession = TRUE){
  
  sel_cols <- findSRAAccessionLevelColumnNames(acc_levels, add_run_accession = add_run_accession)
  col_ind <- NULL
  
  for (i in seq_along(sel_cols)){
    x <- grep(paste0("^", sel_cols[i], "$"), colnames(df))
    col_ind <- c(col_ind, x)
  }
  
  return(df[,col_ind])
}







#' Convert from SRA-GEO Categories
#' 
#' Converts from SRA-GEO Categories to corresponding SRA library_strategy and GEO (study) type.
#' For further details regarding available categories (and their corresponding elements), inspect the \code{SRA_GEO_Category_Conversion} object or see its documentation page: \code{?SRA_GEO_Category_Conversion}.
#'
#' @param x Character with a category (can be a vector). NOTE: must match exactly (but matching is case insensitive)
#' @return A list with a vector each for sra_library_strategy and geo_type
#'
#'
convertCategoriesToLibraryStrategyType <- function(x){
  
  df <- SRA_GEO_Category_Conversion # Retrieve category conversion data frame
  
  # Make matching case insensitive
  x <- tolower(x)
  df_lower <- df
  df_lower$Category <- tolower(df_lower$Category)
  
  
  if (sum(df_lower$Category %in% x)==0) stop("Provide a valid category")
  
  df <- df[df_lower$Category %in% x,] # Filter by matching category/categories
  
  y <- list()
  
  y$sra_library_strategy <- dplyr::filter(df, DB == "SRA")$Name
  y$geo_type <- dplyr::filter(df, DB == "GEO")$Name
  
  #if (length(y$sra_library_strategy)==0){
  #  y$sra_library_strategy <- NULL
  #}
  
  #if (length(y$geo_type)==0){
  #  y$geo_type <- NULL
  #}
  
  #y$sra_library_strategy <- df$Name[df$DB=="SRA"]
  #y$geo_type <- df$Name <- df$Name[df$DB=="GEO"]
  
  return(y)
  
}


#' Find SRA column names corresponding to accession levels
#' 
#' @param acc_levels Accession levels
#' @param add_run_accession Logical indicating whether to add run_accession column name
#' @return Vector with column names
#' 
#' @examples 
#' findSRAAccessionLevelColumnNames("run")
#' 
#' 
findSRAAccessionLevelColumnNames <- function(acc_levels = c("run", "experiment", "sample", "study"), add_run_accession = TRUE, table_name = "sra_ft"){
  
  database_name <- "sra_con"
  database_env <- ".GlobalEnv"
  col_list <- DBI::dbListFields(get(database_name, envir = get(database_env)), table_name)
  
  # Store index of first column relevant for an accession level
  run_beg <- grep("^run_ID$", col_list)
  exp_beg <- grep("^experiment_ID$", col_list)
  sample_beg <- grep("^sample_ID$", col_list)
  study_beg <- grep("^study_ID$", col_list)
  
  
  run_cols <- col_list[run_beg:(exp_beg-1)]
  exp_cols <- col_list[exp_beg:(sample_beg-1)]
  sample_cols <- col_list[sample_beg:(study_beg-1)]
  study_cols <- col_list[study_beg:length(col_list)]
  
  
  all_levels <- c("run", "experiment", "sample", "study", "gsm", "gse")
  sra_levels <- c("run", "experiment", "sample", "study")
  
  # Check that there is at least one valid SRA level provided
  if (sum(acc_levels %in% sra_levels)==0){
    stop("Provide at least one valid SRA accession level")
  }
  
  # Check that all accession levels belong to the set of acceptable levels
  if (sum(acc_levels %in% all_levels)!=length(acc_levels)){
    warning("Some accession levels do not belong to SRA/GEO type")
  }
  
  
  
  # Create a vector with column names of interest
  sel_cols <- NULL
  
  # Add run_accession if run is not one of the levels
  if (add_run_accession & !("run" %in% acc_levels)){
    sel_cols <- c(sel_cols, "run_accession")
  }
  
  if ("run" %in% acc_levels){
    sel_cols <- c(sel_cols, run_cols)
  }
  if ("experiment" %in% acc_levels){
    sel_cols <- c(sel_cols, exp_cols)
  }
  if ("sample" %in% acc_levels){
    sel_cols <- c(sel_cols, sample_cols)
  }
  if ("study" %in% acc_levels){
    sel_cols <- c(sel_cols, study_cols)
  }
  
  if (is.null(sel_cols)) stop("Provide at least one accession level to search within")
  
  return(sel_cols)
}



