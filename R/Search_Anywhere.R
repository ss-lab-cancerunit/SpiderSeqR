

#' Search Anywhere within SRA and GEO databases (under construction!)
#' 
#' @param query_all Search term for both SRA and GEO (gse and gsm tables)
#' @param category_both
#' @param acc_levels Accession levels at which the search is conducted
#' @param sra_library_strategy
#' @param sra_other_library_strategy
#' @param geo_type
#' @param sra_query Search term for SRA only
#' @param geo_query Search term for GEO only
#' @param gsm_query
#' @param gse_query
#' 
#' 
#' 
#' @examples 
#' searchAnywhere("*stat3*") #The broadest search
#' searchAnywhere("stat3")
#' searchAnywhere("tp53 OR p53") #Can list synonyms
#' 
#' 
#' searchAnywhere ("p53", acc_levels = c("gsm", "gse")) #Only search in GEO
#' 
#' 
#' @section Delete some things below:
#' TO BE DELETED ===*====
#' 
#' @section Argument requirements:
#' Either query_all or \strong{both} sra_query and geo_query need to be provided (this is to facilitate column-specific search in the databases; if you wish to search within specific columns, provide sra_query and geo_query with appropriate column names)
#' 
#' 
#' @section Query arguments:
#' 
#' Query arguments include query_both, sra_query, geo_query, gsm_query and gse_query.
#' In the simplest case, it is recommended to just use query_both, which will apply to all the searches across databases. However, for user in need of more fine-tuning, other query arguements can be used (e.g. when you wish to search within specific columns of each database table; this is mostly appropriate for use in fts search).
#' Only the highest level query arguments will be considered. Hence the following combinations of arguments are accepted (any extra query arguments will be ignored): 
#' \itemize{
#'     \item query_both
#'     \item sra_query and geo_query
#'     \item sra_query and gsm_query and gse_query
#' 
#' }
#' @section Category_both, sra_library_strategy and geo_type:
#' 
#' SRA and GEO have distinct ways of specifying the type of their data (such as e.g. RNA-Seq, ChIP-Seq or microarray expression experiments). SRA stores that information as *library_strategy*, GEO records *types*. For users' convenience, a data frame with the conversion between the commmonest *library_strategies* and *types* is provided in \code{SRA_GEO_Category_Conversion} (for more details, please examine \code{SRA_GEO_Category_Conversion} or its documentation, \code{?SRA_GEO_Category_Conversion}). Hence, it is possible to specify *category*, which refers to either one or both SRA and GEO (some categories exist within both SRA and GEO, some only in one of the databases; e.g. only GEO stores microarray data).
#' 
#' Similarly to query arguments, the highest level argument will be taken into account and if lower-level arguments exist, they will be ignored. Hence, the user can provide the following combinations of arguments:
#' \itemize{
#'     \item NONE of category_both, sra_library_strategy and geo_type
#'     \item category_both ONLY
#'     \item sra_library_strategy AND geo_type
#'     \item sra_library_strategy ONLY*
#'     \item geo_type ONLY* 
#' }
#' * If only one of the sra_library_strategy and geo_type is provided, no search will be undertaken in the database corresponding to the missing argument. The same is the case if the supplied category_both refers only to one of the databases (e.g. search in SRA only if category_both = "DNA NGS" (DNA sequencing))
#' 
#' 
#' 
#' 
#' @section Accession levels:
#' Each accession level is associated with its own set of information. Sometimes the information is replicated across levels, sometimes it is unique to the level. Only information associated with the specified accession levels will be subject of the search. For example, it is common for study abstracts to mention a lot of gene names or proteins that were not a direct object of the study; by searching everywhere studies with a mere mention of a gene will be included. 
#' 
#' Restricting accession levels, e.g.  
#' 
#' \code{searchAnywhere(query_all = "p53", acc_levels = c("run", "experiment", "sample", "gsm"))}  
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
searchAnywhere <- function(query_all, category_both=NULL, acc_levels = c("run", "experiment", "sample", "study", "gsm", "gse"), sra_library_strategy=NULL, sra_other_library_strategy = c("OTHER", "NA", "NULL"), geo_type=NULL, sra_query, geo_query, gsm_query, gse_query, ...){
  
  
  # Query arguments ####
  # Checking arguments (either query_all or sra_query AND geo_query (OR gsm_query AND gse_query))
  
  if (!missing(query_all)){ # QUERY_ALL PRESENT
    
    if (!missing(sra_query) | !missing(geo_query) | !missing(gsm_query) | !missing(gse_query)){
      warning("query_all already provided; sra/geo/gsm/gse_query will be ignored")
    }
    
    sra_query <- query_all
    gsm_query <- query_all
    gse_query <- query_all
    
  } else { # QUERY_ALL ABSENT
    
    #sra_query as provided (sra_query <- sra_query)
    if (missing(sra_query)){
      stop("sra_query is required")
    }
    
    if (!missing(geo_query)){ ## QUERY_ALL ABSENT; GEO_QUERY PRESENT
      
      if( !missing(gsm_query) | !missing(gse_query)){
        warning("geo_query already provided; gsm/gse_query will be ignored")
      }
      
      gsm_query <- geo_query
      gse_query <- geo_query
      
    } else { ## QUERY_ALL ABSENT; GEO_QUERY ABSENT
      
      # gsm_query, gse_query as provided (gsm_query <- gsm_query; gse_query <- gse_query)
      if ( missing(gsm_query) | missing(gse_query)){
        stop("gsm_query and gse_query are both required in the absence of query_all or geo_query")
      }
    }
    
  }

  
  
  # category_both, sra_library_strategy and geo_type ####
  # Convert sra_library_strategy from a list of synonyms to a canonical form (will be disregarded if category_both is provided)
  if (!is.null(sra_library_strategy)){
    x <- character()
    for (s in seq_along(sra_library_strategy)){
      x[s] <- manageLibraryStrategy(sra_library_strategy[s], input = "syn", output = "can")
    }
    sra_library_strategy <- x
  }
  
  # category_both PRESENT ####
  # Populate sra_library_strategy and geo_type with converted categories
  if (!is.null(category_both)){
    
    if ( !is.null(sra_library_strategy) | !is.null(geo_type)){
      #warning("category_both already provided; sra_library_strategy/geo_type will be ignored")
      message("category_both already provided; sra_library_strategy/geo_type will be ignored")
    }
    
    sra_library_strategy <- convertCategoriesToLibraryStrategyType(category_both)$sra_library_strategy
    geo_type <- convertCategoriesToLibraryStrategyType(category_both)$geo_type
    #print(sra_library_strategy)
    #print(geo_type)
    
    # If catagory_both has no corresponding library_strategy in SRA, don't search there
    if (is.null(sra_library_strategy)){
      length_pre <- length(acc_levels)
      acc_levels <- acc_levels[!acc_levels %in% c("run", "experiment", "sample", "study")]
      if (length(acc_levels) < length_pre){
        #warning("Category_both does not exist in SRA - will not search there")
        message("Category_both does not exist in SRA - will not search there")
      }
    }
    
    # If catagory_both has no corresponding type in GEO, don't search there
    if (is.null(geo_type)){
      length_pre <- length(acc_levels)
      acc_levels <- acc_levels[!acc_levels %in% c("gsm", "gse")]
      if (length(acc_levels) < length_pre){
        #warning("Category_both does not exist in GEO - will not search there")
        message("Category_both does not exist in GEO - will not search there")
      }
    }
    
    
  } else {
    
    # category_both ABSENT ####
    
    # Omit search within SRA if geo_type exists, but sra_library_strategy is null
    if (!is.null(geo_type) & is.null(sra_library_strategy)){
      length_pre <- length(acc_levels)
      acc_levels <- acc_levels[!acc_levels %in% c("run", "experiment", "sample", "study")]
      if (length(acc_levels) < length_pre){
        #warning("SRA library strategy not provided. Will only search in GEO")
        message("SRA library strategy not provided. Will only search in GEO")
      }
    }
    
    # Omit search within GEO if sra_library_strategy exists, but geo_type is null
    if (is.null(geo_type) & !is.null(sra_library_strategy)){
      length_pre <- length(acc_levels)
      acc_levels <- acc_levels[!acc_levels %in% c("gsm", "gse")]
      if (length(acc_levels) < length_pre){
        #warning("GEO type not provided. Will onlly search in SRA")
        message("GEO type not provided. Will onlly search in SRA")
      }
    }
    
  }
  
  message("===SEARCH DETAILS===")
  message("---QUERY---")
  message("sra_query: ", sra_query)
  message("gsm_query: ", gsm_query)
  message("gse_query: ", gse_query)
  message("---LIBRARY_STRATEGY/TYPE---")
  message("sra_library_strategy: ", sra_library_strategy)
  message("geo_type: ", geo_type)
  message("---ACCESSION LEVELS FOR SEARCHING---")
  message("acc_levels: ", paste0(acc_levels, collapse = ", "))
  message("=====================")
  
  
  # Developer check ===*===
  #sra_arg_check <- list(...)$sra_arg_check
  #if (!is.null(sra_arg_check)){
  #  searchAnywhereSRA(sra_query, sra_library_strategy, sra_other_library_strategy, acc_levels = acc_levels, sra_arg_check)
  #}
  
  
  if (sum(acc_levels %in% c("run", "experiment", "sample", "study"))>0){
    print("Search SRA")
    sra_df <- searchAnywhereSRA(sra_query = sra_query, acc_levels = acc_levels, sra_library_strategy = sra_library_strategy, sra_other_library_strategy = sra_other_library_strategy)
    sra_out <- searchForAccessionAcrossDBsDF(sra_df$run_accession, "*", "*", "*", sra_df)
    
    
  }
  
  if (sum(acc_levels %in% c("gse", "gsm"))>0){
    print("Search GEO")
    geo_df <- searchAnywhereGEO(gsm_query = gsm_query, gse_query = gse_query, acc_levels = acc_levels, geo_type = geo_type)
    geo_out <- searchForAccessionAcrossDBsDF(geo_df$gsm, "*", "*", "*", geo_df)
  }
  
  
  # TBD ####
  # Search in SRA if any of the acc_levels are from SRA
  # ===*===
  if (sum(acc_levels %in% c("run", "experiment", "sample", "study"))>0){
    if (!(!is.null(category_both) & length(sra_library_strategy)==0)){ # Don't search if category_both doesn't include SRA
      #print("Search SRA")
      #sra_df <- searchAnywhereSRA(sra_query, acc_levels = acc_levels, sra_library_strategy = sra_library_strategy, sra_other_library_strategy = sra_other_library_strategy) # NOT PASSING ANY OTHER ARGUMENTS HERE ===*===
    }
  }
  
  
  if (sum(acc_levels %in% c("gse", "gsm"))>0){
    if (!(!is.null(category_both) & length(geo_type)==0)){ # Don't search if category_both doesn't include GEO
      #print("Search GEO")
      #geo_df <- searchAnywhereGEO(gsm_query = gsm_query, gse_query = gse_query, acc_levels = acc_levels, geo_type = geo_type)
    }  
  }

  
  
  #------TBC
  # ===*===
  
  df <- geo_out
  
  
  return(df)
}




#' Search anywhere within gse and gsm tables of GEO database
#' 
#' @param gsm_query String to search for within gsm table
#' @param gse_query String to search for within gse table
#' @param acc_levels Character vector indicating where to conduct the search (only "gse" and "gsm" are considered)
#' @param geo_type Study type for filtering results (optional)
#' @return Data frame with result data from whole GEO (gsm and gse tables)
#' 
#' 
searchAnywhereGEO <- function(gsm_query, gse_query, acc_levels = c("gse", "gsm"), geo_type=NULL){
  
  if ("gsm" %in% acc_levels){
    df_gsm <- searchAnywhereGSM(gsm_query, geo_type)
    df_out <- df_gsm
  }
  
  if ("gse" %in% acc_levels){
    df_gse <- searchAnywhereGSE(gse_query, geo_type)
    df_out <- df_gse
  }
  
  if (sum(c("gsm", "gse") %in% acc_levels)==2){
    df_out <- unique((rbind(df_gsm, df_gse)))
    
  }

  return(df_out)
}



#' Search anywhere within gsm table of GEO database
#' 
#' @param gsm_query String to search for 
#' @param geo_type Study type for filtering results (optional)
#' @return Data frame with result data from whole GEO (gsm and gse tables)
#' 
#' 
searchAnywhereGSM <- function(gsm_query, geo_type){ # No acc_levels needed in this case
  
  database_name <- "geo_con"
  database_env <- ".GlobalEnv"
  
  # List all tables within geo_con
  geo_tables <- DBI::dbListTables(get(database_name, envir = get(database_env)))
  
  # Construct a query within gsm ####
  if (!("gsm_ft" %in% geo_tables)){
    # Standard search ####
    gsm_columns <- DBI::dbListFields(get(database_name, envir = get(database_env)), "gsm")
    full_query <- "SELECT * FROM gsm WHERE ( " # initial bracket
    for (g in gsm_columns){
      chunk <- paste0("( ", g, " LIKE '%", gsm_query, "%') OR ")
      full_query <- paste0(full_query, chunk)
    }
    full_query <- substr(full_query, 1, nchar(full_query)-4)
    full_query <- paste0(full_query, " )") # final bracket
    print(full_query)
    df <- DBI::dbGetQuery(get(database_name, envir = get(database_env)), full_query)
  } else {
    # Fts search ####
    stop("No fts support yet")
    #df <- 1 # To be added ===*===
  }
  
  # Append gse columns ####
  df <- appendGSEColumns(df, "*")
  
  # Filter by geo_type if provided ####
  if (!is.null(geo_type)){
    filt_ind <- grepl(geo_type, df$GSE_type)
    df <- df[filt_ind,]
  }
  
  return(df)
}


#' Search anywhere within gse table of GEO database
#' 
#' @param gse_query String to search for 
#' @param geo_type Study type for filtering results (optional)
#' @return Data frame with result data from whole GEO (gsm and gse tables)
#' 
#' 
searchAnywhereGSE <- function(gse_query, geo_type){
  
  database_name <- "geo_con"
  database_env <- ".GlobalEnv"
  
  # List all tables within geo_con
  geo_tables <- DBI::dbListTables(get(database_name, envir = get(database_env)))
  
  # Construct a query within gse ####
  if (!("gse_ft" %in% geo_tables)){
    # Standard search ####
    gse_columns <- DBI::dbListFields(get(database_name, envir = get(database_env)), "gse")
    full_query <- "SELECT * FROM gse WHERE ( " # initial bracket
    for (g in gse_columns){
      chunk <- paste0("( ", g, " LIKE '%", gse_query, "%') OR ")
      full_query <- paste0(full_query, chunk)
    }
    full_query <- substr(full_query, 1, nchar(full_query)-4)
    full_query <- paste0(full_query, " )") # final bracket
    print(full_query)
    df <- DBI::dbGetQuery(get(database_name, envir = get(database_env)), full_query)
  } else {
    # Fts search ####
    stop("No fts support yet")
    #df <- 1 # To be added ===*===
  }
  
  # Filter by geo_type if provided ####
  if (!is.null(geo_type)){
    filt_ind <- rep(FALSE, dim(df)[1])
    for (t in geo_type){
      filt_ind <- filt_ind | grepl(geo_type, df$type) # Note that column name here is type, not GSE_type
    }
    df <- df[filt_ind,]
  }
  
  
  # Search across GEO for GSEs ####
  df <- searchGEOForGSE(df$gse, "*", "*")
  

  return(df)
  
  
}




#------------------------------------------
# ------------------DONE-------------------
#------------------------------------------



#' 
#' Fulltext search in SRA
#' 
#' @param sra_query Query passed to fts MATCH operator (cannot be a vector)
#' @param library_strategy Character vector denoting library_strategy/ies of choice (OPTIONAL)
#' @param sra_other_library_strategy A character vector indicating whether (and which) uncategorised library strategies are accepted (choose between one and all elements of c("OTHER", "NA", "NULL")); if not desired, set equal to FALSE. NOTE: only evaluated if library strategy is provided
#' @param acc_levels Character vector denoting which accession levels will be searched. Choose from some/all of c("run", "experiment", "sample", "study")
#' 
#' @examples 
#' # stat3
#' searchAnywhereSRA("stat3") 
#' 
#' # stat3 in human samples
#' searchAnywhereSRA("stat3 taxon_id: 9606") 
#' 
#' # stat3 chip-seq (including unclassified library strategies)
#' searchAnywhereSRA("stat3", library_strategy = "ChIP-Seq")
#' # stat3 chip-seq not including unclassified library strategies
#' searchAnywhereSRA("stat3", library_strategy = "ChIP-Seq", sra_other_library_strategy = FALSE) 
#' 
#' #stat3 ignoring matches at study level
#' searchAnywhereSRA("stat3", acc_levels = c("run", "experiment", "sample")) 
#' 
#' 
#' 
searchAnywhereSRA <- function(sra_query, acc_levels = c("run", "experiment", "sample", "study"), sra_library_strategy=NULL, sra_other_library_strategy = c("OTHER", "NA", "NULL"),  ...){
  
  sra_arg_check <- list(...)$sra_arg_check
  if(!is.null(sra_arg_check)){
    if(isTRUE(sra_arg_check)){
      
      #return(as.list(match.call(def=sys.function(-1), call = sys.call(-1))))
      
      # Previously
      
      #print(as.list(match.call(expand.dots = TRUE)))
      print(as.list(match.call(def=sys.function(-1), call = sys.call(-1))))
      
      l <- list()
      l$sra_query <- sra_query
      l$sra_library_strategy <- sra_library_strategy
      l$sra_other_library_strategy <- sra_other_library_strategy
      l$acc_levels <- acc_levels
      l$dots <- list(...)
      
      return(l)
      
      
      print(paste0("sra_query: ", sra_query))
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

  query_full <- paste0("SELECT * FROM sra_ft WHERE sra_ft MATCH '", sra_query, "'")
  
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
  
  .GlobalEnv$temp_searchAnywhereSRA <- df
  
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
  
  if (sum(unique(acc_levels) %in% c("study", "sample", "experiment", "run"))==4){
    print("Nothing to filter - returning original df")
    return(df)
  }
  
  # Select columns within df
  df_filt <- subsetSRAByAccessionLevel(df, acc_levels, add_run_accession = TRUE)
  
  
  # Create db
  filter_db_file <- "filter_db.sqlite"
  
  if (file.exists(filter_db_file)) file.remove(filter_db_file)
  
  .GlobalEnv$filter_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = filter_db_file)
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
  
  # devtools::check()
  DB <- NULL
  SRA_GEO_Category_Conversion <- NULL
  
  utils::data("SRA_GEO_Category_Conversion", envir = environment())
  df <- SRA_GEO_Category_Conversion # Retrieve category conversion data frame
  
  # Make matching case insensitive
  x <- tolower(x)
  df_lower <- df
  df_lower$Category <- tolower(df_lower$Category)
  
  
  if (sum(df_lower$Category %in% x)==0) stop("Provide a valid category")
  
  df <- df[df_lower$Category %in% x,] # Filter by matching category/categories
  
  y <- list()
  
  #.GlobalEnv$temp_convertCat_y_ori <- df
  
  
  y$sra_library_strategy <- dplyr::filter(df, DB == "SRA")$Name
  y$geo_type <- dplyr::filter(df, DB == "GEO")$Name
  
  if (length(y$sra_library_strategy)==0){
    y[1] <- list(NULL) # This is a slight trick to avoid losing that list element altogether
  }
  if (length(y$geo_type)==0){
    y[2] <- list(NULL) # This is a slight trick to avoid losing that list element altogether
  }
  
  #.GlobalEnv$temp_convertCat_y_later <- y
  
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



