

#' Search Anywhere within SRA and GEO databases
#' 
#' @param query_all Search term for both SRA and GEO (gse and gsm tables)
#' 
#' @param acc_levels Accession levels at which the search is conducted.
#'     Possible options include run, sample, experiment, study, gsm, gse. 
#'     Defaults to c("run", "experiment", "sample", "gsm")
#' @param category_both A character with category 
#'     for SRA library strategy and GEO type
#' @param SRA_library_strategy A character with SRA library strategy
#' @param SRA_other_library_strategy A logical whether to include 
#'     unclassified entries
#' @param GEO_type A character with GEO type
#' @param SRA_query Search term for SRA only
#' @param GEO_query Search term for GEO only
#' @param GSM_query Search term for gsm table only (GEO)
#' @param GSE_query Search term for gse table only (GEO)
#' @param call_output A logical indicating whether to produce a call record
#' @return A data frame with results of the search
#' 
#' 
#' 
#' @examples 
#' startSpiderSeqRDemo()
#' searchAnywhere("*sir3*") # The broadest search
#' searchAnywhere("sir3") # omits entries with characters before/after sir3
#' searchAnywhere("sir3 OR sir3p") # Can list synonyms
#' 
#' ## Only search for matches in SRA
#' searchAnywhere ("sir3", acc_levels = c("run", 
#'            "sample", "experiment", "study"))
#' 
#' ## Only search for matches in GEO
#' searchAnywhere ("sir3", acc_levels = c("gsm", "gse"))
#' 
#' 
#' 
#' 
#' @section Argument requirements:
#' Either query_all or \strong{both} SRA_query and GEO_query 
#' need to be provided (this is to facilitate column-specific search 
#' in the databases; if you wish to search within specific columns, 
#' provide SRA_query and GEO_query with appropriate column names)
#' 
#' 
#' @section Query arguments:
#' 
#' Query arguments include query_both, SRA_query, GEO_query,
#' GSM_query and GSE_query. 
#' 
#' In the simplest case, it is recommended to just use query_both, 
#' which will apply to all the searches across databases. 
#' However, for user in need of more fine-tuning, other query arguements 
#' can be used (e.g. when you wish to search within specific columns 
#' of each database table; this is mostly appropriate for use in fts search).
#' Only the highest level query arguments will be considered. 
#' Hence the following combinations of arguments are accepted 
#' (any extra query arguments will be ignored): 
#' \itemize{
#'     \item query_both
#'     \item SRA_query and GEO_query
#'     \item SRA_query and GSM_query and GSE_query
#' 
#' }
#' 
#' @section Accession levels:
#' Each accession level is associated with its own set of information. 
#' Sometimes the information is replicated across levels, 
#' sometimes it is unique to the level. 
#' Only information associated with the specified accession levels 
#' will be subject of the search. 
#' For example, it is common for study abstracts to mention a lot 
#' of gene names or proteins that were not a direct object of the study; 
#' by searching everywhere studies with a mere mention 
#' of a gene will be included. 
#' 
#' Restricting accession levels, e.g.  
#' 
#' \code{searchAnywhere(query_all = "p53", 
#' acc_levels = c("run", "experiment", "sample", "gsm"))}  
#' 
#' will help avoid including these cases. 
#' However, always consider using a broader search 
#' and comparing the results to the more refined one.
#' 
#' Another use of accession levels is to restrict search to only one database. 
#' To do so, only list accession levels specific to one database: 
#' SRA (run, experiment, sample, study) or GEO (gsm, gse).
#' 
#' 
#' @section Category_both, SRA_library_strategy and GEO_type:
#' 
#' SRA and GEO have distinct ways of specifying the type of their data 
#' (such as e.g. RNA-Seq, ChIP-Seq or microarray expression experiments). 
#' SRA stores that information as *library_strategy*, GEO records *types*. 
#' For users' convenience, a data frame with the conversion 
#' between the commmonest *library_strategies* and *types* is provided 
#' in \code{SRA_GEO_Category_Conversion} (for more details, please examine 
#' \code{SRA_GEO_Category_Conversion} or its documentation, 
#' \code{?SRA_GEO_Category_Conversion}). 
#' 
#' Hence, it is possible to specify *category*, which refers to either 
#' one or both SRA and GEO (some categories exist within both SRA and GEO, 
#' some only in one of the databases; e.g. only GEO stores microarray data).
#' 
#' Similarly to query arguments, the highest level argument 
#' will be taken into account and if lower-level arguments exist, 
#' they will be ignored. 
#' 
#' Hence, the user can provide the following combinations of arguments:
#' \itemize{
#'     \item NONE of category_both, SRA_library_strategy and GEO_type
#'     \item category_both ONLY
#'     \item SRA_library_strategy AND GEO_type
#'     \item SRA_library_strategy ONLY*
#'     \item GEO_type ONLY* 
#' }
#' * If only one of the SRA_library_strategy and GEO_type is provided, 
#' no search will be undertaken in the database corresponding 
#' to the missing argument. 
#' The same is the case if the supplied category_both refers only 
#' to one of the databases 
#' (e.g. search in SRA only if category_both = "DNA NGS" (DNA sequencing))
#' 
#' 
#' 
#' 
#' @export
#' 
searchAnywhere <- function(query_all, 
                        acc_levels = c("run", "experiment", "sample", "gsm"), 
                        category_both=NULL, 
                        SRA_library_strategy=NULL, 
                        SRA_other_library_strategy = c("OTHER", "NA", "NULL"), 
                        GEO_type=NULL, 
                        SRA_query, 
                        GEO_query, 
                        GSM_query, 
                        GSE_query, 
                        call_output = FALSE){
    
    
    ## Query arguments ####
    ## Checking arguments (either query_all or SRA_query AND GEO_query 
    ## (OR GSM_query AND GSE_query))
    
    if (!missing(query_all)){ # QUERY_ALL PRESENT
        
        if (!missing(SRA_query) | 
                                    !missing(GEO_query) | 
                                    !missing(GSM_query) | 
                                    !missing(GSE_query)){
            warning(paste0("query_all already provided; ",
                                "sra/geo/gsm/GSE_query will be ignored"))
        }
        
        SRA_query <- query_all
        GSM_query <- query_all
        GSE_query <- query_all
        
    } else { # QUERY_ALL ABSENT
        
        ## SRA_query as provided (SRA_query <- SRA_query)
        if (missing(SRA_query)){
            stop("SRA_query is required")
        }
        
        if (!missing(GEO_query)){ ## QUERY_ALL ABSENT; GEO_query PRESENT
            
            if( !missing(GSM_query) | !missing(GSE_query)){
                warning(paste0("GEO_query already provided; ",
                                "gsm/GSE_query will be ignored"))
            }
            
            GSM_query <- GEO_query
            GSE_query <- GEO_query
            
        } else { ## QUERY_ALL ABSENT; GEO_query ABSENT
            
            # GSM_query, GSE_query as provided (GSM_query <- 
            # GSM_query; GSE_query <- GSE_query)
            if ( missing(GSM_query) | missing(GSE_query)){
                stop(paste0("GSM_query and GSE_query are both required ",
                                "in the absence of query_all or GEO_query"))
            }
        }
        
    }
    
    
    
    # category_both, SRA_library_strategy and GEO_type ####
    # Convert SRA_library_strategy from a list of synonyms 
    # to a canonical form (will be disregarded if category_both is provided)
    if (!is.null(SRA_library_strategy)){
        x <- character()
        for (s in seq_along(SRA_library_strategy)){
            x[s] <- .manageLibraryStrategy(SRA_library_strategy[s], 
                                            input = "syn", output = "can")
        }
        SRA_library_strategy <- x
    }
    
    # category_both PRESENT ####
    # Populate SRA_library_strategy and GEO_type with converted categories
    if (!is.null(category_both)){
        
        if ( !is.null(SRA_library_strategy) | !is.null(GEO_type)){
            #warning("category_both already provided; 
            #SRA_library_strategy/GEO_type will be ignored")
            message(paste0("category_both already provided; ",
                        "SRA_library_strategy/GEO_type will be ignored"))
        }
        
        SRA_library_strategy <- 
            convertCategoriesToLibraryStrategyType(category_both)$
                                                        SRA_library_strategy
        GEO_type <- 
            convertCategoriesToLibraryStrategyType(category_both)$GEO_type
        #print(SRA_library_strategy)
        #print(GEO_type)
        
        # If catagory_both has no corresponding library_strategy in SRA, 
        # don't search there
        if (is.null(SRA_library_strategy)){
            length_pre <- length(acc_levels)
            acc_levels <- acc_levels[!acc_levels %in% c("run", 
                                                        "experiment", 
                                                        "sample", 
                                                        "study")]
            if (length(acc_levels) < length_pre){
                #warning("Category_both does not exist in SRA 
                #- will not search there")
                message(paste0("Category_both does not exist in SRA ",
                "- will not search there"))
            }
        }
        
        # If catagory_both has no corresponding type in GEO, don't search there
        if (is.null(GEO_type)){
            length_pre <- length(acc_levels)
            acc_levels <- acc_levels[!acc_levels %in% c("gsm", "gse")]
            if (length(acc_levels) < length_pre){
                #warning("Category_both does not exist in GEO 
                # - will not search there")
                message(paste0("Category_both does not exist in GEO ",
                "- will not search there"))
            }
        }
        
        
    } else {
        
        # category_both ABSENT ####
        
        # Omit search within SRA if GEO_type exists, 
        # but SRA_library_strategy is null
        if (!is.null(GEO_type) & is.null(SRA_library_strategy)){
            length_pre <- length(acc_levels)
            acc_levels <- acc_levels[!acc_levels %in% c("run", 
                                                        "experiment", 
                                                        "sample", 
                                                        "study")]
            if (length(acc_levels) < length_pre){
                #warning("SRA library strategy not provided. 
                # Will only search in GEO")
                message(paste0("SRA library strategy not provided. ",
                "Will only search in GEO"))
            }
        }
        
        # Omit search within GEO if SRA_library_strategy exists, 
        #but GEO_type is null
        if (is.null(GEO_type) & !is.null(SRA_library_strategy)){
            length_pre <- length(acc_levels)
            acc_levels <- acc_levels[!acc_levels %in% c("gsm", "gse")]
            if (length(acc_levels) < length_pre){
                #warning("GEO type not provided. Will onlly search in SRA")
                message("GEO type not provided. Will onlly search in SRA")
            }
        }
        
    }
    .mm(cli::rule(), "search")
    .mm(cli::rule(left = "SEARCH DETAILS:"), "search")
    #.mm(cli::rule(), "search")
    .mm(cli::rule(left = "QUERY"), "search")
    #.mm(cli::rule(), "search")
    .mm(paste0("SRA_query: ", SRA_query), "search")
    .mm(paste0("GSM_query: ", GSM_query), "search")
    .mm(paste0("GSE_query: ", GSE_query), "search")
    .mm(cli::rule(left = "LIBRARY_STRATEGY/TYPE"), "search")
    .mm(paste0("SRA_library_strategy: ", SRA_library_strategy), "search")
    .mm(paste0("GEO_type: ", GEO_type), "search")
    .mm(cli::rule(left = "ACCESSION LEVELS FOR SEARCHING"), 
        "search")
    .mm(paste0("acc_levels: ", paste0(acc_levels, collapse = ", ")), "search")
    .mm(cli::rule(), "search")
    
    
    #message("========================================================")
    #message("===SEARCH DETAILS=======================================")
    #message("---QUERY------------------------------------------------")
    #message("SRA_query: ", SRA_query)
    #message("GSM_query: ", GSM_query)
    #message("GSE_query: ", GSE_query)
    #message("---LIBRARY_STRATEGY/TYPE--------------------------------")
    #message("SRA_library_strategy: ", SRA_library_strategy)
    #message("GEO_type: ", GEO_type)
    #message("---ACCESSION LEVELS FOR SEARCHING-----------------------")
    #message("acc_levels: ", paste0(acc_levels, collapse = ", "))
    #message("========================================================")
    
    if (call_output){
        sa_argument_list <- list(SRA_query=SRA_query,
                                GSM_query=GSM_query,
                                GSE_query=GSE_query,
                                SRA_library_strategy=SRA_library_strategy,
                                GEO_type=GEO_type,
                                acc_levels=acc_levels)
        
        
        sa_file <- .generateFileName_CALL_SA(sa_argument_list)
        .generateCallRecord(file = sa_file)
    }

    
    # Developer check ===*===
    #sra_arg_check <- list(...)$sra_arg_check
    #if (!is.null(sra_arg_check)){
    #  .searchAnywhereSRA(SRA_query, SRA_library_strategy, 
    #SRA_other_library_strategy, acc_levels = acc_levels, sra_arg_check)
    #}
    
    # Search within SRA ####
    if (sum(acc_levels %in% c("run", "experiment", "sample", "study"))>0){
        .mm("Searching for matches in SRA...", "prog")
        sra_df <- .searchAnywhereSRA(SRA_query = SRA_query, 
                    acc_levels = acc_levels, 
                    SRA_library_strategy = SRA_library_strategy, 
                    SRA_other_library_strategy = SRA_other_library_strategy)
        
        .vex("temp_anywhere_sra_df", sra_df)
        
        if (dim(sra_df)[1]!=0){
            sra_out <- .searchForAccessionAcrossDBsDF(sra_df$run_accession, 
                                                        "*", "*", "*", sra_df)
        } else {
            sra_out <- .generateEmptyDF() # Warning already generated by SRA
        }
        
        .vex("temp_anywhere_sra_out_sfa", sra_out)
        sra_out <- .unifyDFFormat(sra_out)
        .vex("temp_anywhere_sra_out_udf", sra_out)
        
    } else {
        sra_out <- data.frame() # Create an empty data frame for rbind
    }
    
    # Search within GEO ####
    if (sum(acc_levels %in% c("gse", "gsm"))>0){
        .mm("Searching for matches in GEO...", "prog")
        geo_df <- .searchAnywhereGEO(GSM_query = GSM_query, 
                                    GSE_query = GSE_query, 
                                    acc_levels = acc_levels, 
                                    GEO_type = GEO_type)
        
        .vex("temp_anywhere_geo_df", geo_df)
        
        if (dim(geo_df)[1]!=0){
            geo_out <- .searchForAccessionAcrossDBsDF(geo_df$gsm, 
                                                        "*", "*", "*", geo_df)
        } else {
            # Warning already generated by GSM, GSE
            geo_out <- .generateEmptyDF() 
        }
        
        .vex("temp_anywhere_geo_out_sfa", geo_out)
        geo_out <- .unifyDFFormat(geo_out)
        .vex("temp_anywhere_geo_out_udf", geo_out)
    } else {
        geo_out <- data.frame() # Create an empty data frame for rbind
    }
    
    
    .vex("temp_anywhere_sra_out", sra_out)
    .vex("temp_anywhere_geo_out", geo_out)
    
    # Combine results from GEO and SRA ####
    df_out <- rbind(sra_out, geo_out)
    #df_out <- .unifyDFFormat(df_out)
    
    
    
    #---------------------------------------------------
    # TBD ####
    #---------------------------------------------------
    # Search in SRA if any of the acc_levels are from SRA
    # ===*===
    if (sum(acc_levels %in% c("run", "experiment", "sample", "study"))>0){
        
        # Don't search if category_both doesn't include SRA
        if (!(!is.null(category_both) & length(SRA_library_strategy)==0)){ 
            #print("Search SRA")
            #sra_df <- .searchAnywhereSRA(SRA_query, 
                #acc_levels = acc_levels, 
                #SRA_library_strategy = SRA_library_strategy, 
                #SRA_other_library_strategy = SRA_other_library_strategy) 
            # NOT PASSING ANY OTHER ARGUMENTS HERE ===*===
        }
    }
    
    
    if (sum(acc_levels %in% c("gse", "gsm"))>0){
        
        # Don't search if category_both doesn't include GEO
        if (!(!is.null(category_both) & length(GEO_type)==0)){ 
            #print("Search GEO")
            #geo_df <- .searchAnywhereGEO(GSM_query = GSM_query, 
            #GSE_query = GSE_query, acc_levels = acc_levels, 
            #GEO_type = GEO_type)
        }  
    }
    #---------------------------------------------------
    #---------------------------------------------------
    
    
    
    
    # Process results and add unifying columns ####
    
    #Don't create sample column
    df_out <- .extractGSM(df_out, sampleColumn = FALSE) 
    
    df_out <- .saExtractor(df_out)
    .vex("temp_TEN_df_out", df_out)
    df_out <- .chExtractor(df_out)
    
    # No .detectInputs/Controls used
    df_out <- createEmptyColumns(df_out, c("input", "control"))
    
    df_out <- .detectMerges(df_out, do_nothing = TRUE)
    
    #No .verifyMissingRuns used
    
    df_out <- .convertPairedEnds(df_out)
    
    
    if (dim(df_out)[1]!=0){
        df_out <- .unifyNAs(df_out)
    }
    
    
    df_out <- .renameOTHColumns(df_out)
    
    .vex("temp_df_out", df_out)
    df_out <- .unifyDFFormat(df_out)
    
    df_dim <- dim(df_out)[1]
    
    if (df_dim == 1){
        .mm(paste0("Found ", df_dim, " entry matching search terms"), 
            "res")
    } else {
        .mm(paste0("Found ", df_dim, " entries matching search terms"), 
            "res")
    }
    
    
    
    
    return(df_out)
    
}




#' Search anywhere within gse and gsm tables of GEO database
#' 
#' @param GSM_query String to search for within gsm table
#' @param GSE_query String to search for within gse table
#' @param acc_levels Character vector indicating where to conduct the search 
#'     (only "gse" and "gsm" are considered)
#' @param GEO_type Study type for filtering results (optional)
#' @return Data frame with result data from whole GEO (gsm and gse tables)
#' 
#' @keywords internal
#' 
.searchAnywhereGEO <- function(GSM_query, 
                                GSE_query, 
                                acc_levels = c("gse", "gsm"), 
                                GEO_type=NULL){
    
    if ("gsm" %in% acc_levels){
        df_gsm <- .searchAnywhereGSM(GSM_query, GEO_type)
        df_out <- df_gsm
    }
    
    if ("gse" %in% acc_levels){
        df_gse <- .searchAnywhereGSE(GSE_query, GEO_type)
        df_out <- df_gse
    }
    
    if (sum(c("gsm", "gse") %in% acc_levels)==2){
        df_out <- unique((rbind(df_gsm, df_gse)))
        
    }
    
    return(df_out)
}



#' Search anywhere within gsm table of GEO database
#' 
#' @param GSM_query String to search for 
#' @param GEO_type Study type for filtering results (optional)
#' @return Data frame with result data from whole GEO (gsm and gse tables)
#' 
#' @keywords internal
#' 
.searchAnywhereGSM <- function(GSM_query, GEO_type){
    # No acc_levels needed in this case
    
    
    database_name <- "geo_con"
    database_env <- ".GlobalEnv"
    
    # List all tables within geo_con
    geo_tables <- DBI::dbListTables(get(database_name, 
                                        envir = get(database_env)))
    
    # Construct a query within gsm ####
    if (!("gsm_ft" %in% geo_tables)){
        # Standard search ####
        gsm_columns <- DBI::dbListFields(get(database_name, 
                                            envir = get(database_env)), "gsm")
        full_query <- "SELECT * FROM gsm WHERE ( " # initial bracket
        
        for (g in gsm_columns){
            chunk <- paste0("( ", g, " LIKE '%", GSM_query, "%') OR ")
            full_query <- paste0(full_query, chunk)
        }
        
        full_query <- substr(full_query, 1, nchar(full_query)-4)
        full_query <- paste0(full_query, " )") # final bracket
        .mm(full_query, "query")
        df <- DBI::dbGetQuery(get(database_name, 
                                    envir = get(database_env)), full_query)
    } else {
        # Fts search ####
        stop("No fts support yet")
        #df <- 1 # To be added ===*===
    }
    
    
    if (dim(df)[1]==0){
        warning(paste0("No results found in GSM. ",
                "Try refining your search terms or acc_levels"), call. = FALSE)
    }
    
    
    
    df <- .renameGSMColumns(df)
    .vex("temp_df_diag", df)
    
    
    # Append gse columns ####
    df <- .appendGSEColumns(df, "*")
    
    # Filter by GEO_type if provided ####
    if (!is.null(GEO_type)){
        filt_ind <- grepl(GEO_type, df$GSE_type)
        df <- df[filt_ind,]
    }
    
    return(df)
}




#' Search anywhere within gse table of GEO database
#' 
#' @param GSE_query String to search for 
#' @param GEO_type Study type for filtering results (optional)
#' @return Data frame with result data from whole GEO (gsm and gse tables)
#' 
#' 
#' @keywords internal
#' 
.searchAnywhereGSE <- function(GSE_query, GEO_type){
    
    database_name <- "geo_con"
    database_env <- ".GlobalEnv"
    
    # List all tables within geo_con
    geo_tables <- DBI::dbListTables(get(database_name, 
                                        envir = get(database_env)))
    
    # Construct a query within gse ####
    if (!("gse_ft" %in% geo_tables)){
        # Standard search ####
        gse_columns <- DBI::dbListFields(get(database_name, 
                                        envir = get(database_env)), "gse")
        
        full_query <- "SELECT * FROM gse WHERE ( " # initial bracket
        for (g in gse_columns){
            chunk <- paste0("( ", g, " LIKE '%", GSE_query, "%') OR ")
            full_query <- paste0(full_query, chunk)
        }
        full_query <- substr(full_query, 1, nchar(full_query)-4)
        full_query <- paste0(full_query, " )") # final bracket
        .mm(full_query, "query")
        df <- DBI::dbGetQuery(get(database_name, 
                                        envir = get(database_env)), full_query)
    } else {
        # Fts search ####
        stop("No fts support yet")
        #df <- 1 # To be added ===*===
    }
    
    # Filter by GEO_type if provided ####
    if (!is.null(GEO_type)){
        filt_ind <- rep(FALSE, dim(df)[1])
        for (t in GEO_type){
            # Note that column name here is type, not GSE_type
            filt_ind <- filt_ind | grepl(GEO_type, df$type) 
        }
        df <- df[filt_ind,]
    }
    
    
    # Search across GEO for GSEs ####
    if (dim(df)[1]!=0){
        df <- .searchGEOForGSE(df$gse, "*", "*")
    } else {
        warning(paste0("No results found in GSE. ",
            "Try refining your search terms or acc_levels"), call. = FALSE)
        df <- .generateEmptyDF(c("gsm", "gse"))
    }
    
    
    
    return(df)
    
    
}




#------------------------------------------
# ------------------DONE-------------------
#------------------------------------------



#' 
#' Fulltext search in SRA
#' 
#' @param SRA_query Query passed to fts MATCH operator (cannot be a vector)
#' @param SRA_library_strategy Character vector denoting 
#'     library_strategy/ies of choice (OPTIONAL)
#' @param SRA_other_library_strategy A character vector indicating whether 
#'     (and which) uncategorised library strategies are accepted 
#'     (choose between one and all elements of c("OTHER", "NA", "NULL")); 
#'     if not desired, set equal to FALSE. 
#'     NOTE: only evaluated if library strategy is provided
#' @param acc_levels Character vector denoting which accession levels 
#'     will be searched. 
#'     Choose from some/all of c("run", "experiment", "sample", "study")
#' @param ... Other options
#' @return Data frame with results
#' 
#' @examples 
#' ## stat3
#' # .searchAnywhereSRA("stat3") 
#' 
#' ## stat3 in human samples
#' # .searchAnywhereSRA("stat3 taxon_id: 9606") 
#' 
#' ## stat3 chip-seq (including unclassified library strategies)
#' # .searchAnywhereSRA("stat3", library_strategy = "ChIP-Seq")
#' ## stat3 chip-seq not including unclassified library strategies
#' # .searchAnywhereSRA("stat3", 
#' #    library_strategy = "ChIP-Seq", 
#' #    SRA_other_library_strategy = FALSE) 
#' 
#' ## stat3 ignoring matches at study level
#' #.searchAnywhereSRA("stat3", acc_levels = c("run", "experiment", "sample")) 
#' 
#' 
#' @keywords internal
.searchAnywhereSRA <- function(SRA_query, 
                        acc_levels = c("run", "experiment", "sample", "study"),
                        SRA_library_strategy=NULL, 
                        SRA_other_library_strategy = c("OTHER", "NA", "NULL"), 
                        ...){
    
    sra_arg_check <- list(...)$sra_arg_check
    if(!is.null(sra_arg_check)){
        if(isTRUE(sra_arg_check)){
            
            #return(as.list(match.call(def=sys.function(-1), 
            # call = sys.call(-1))))
            
            # Previously
            
            #print(as.list(match.call(expand.dots = TRUE)))
            print(as.list(match.call(definition = sys.function(-1), 
                                        call = sys.call(-1))))
            
            l <- list()
            l$SRA_query <- SRA_query
            l$SRA_library_strategy <- SRA_library_strategy
            l$SRA_other_library_strategy <- SRA_other_library_strategy
            l$acc_levels <- acc_levels
            l$dots <- list(...)
            
            return(l)
            
            
            .mm(paste0("SRA_query: ", SRA_query), "search")
            .mm(paste0("SRA_library_strategy: ", SRA_library_strategy),"search")
            .mm(paste0("SRA_other_library_strategy: ", 
                                        SRA_other_library_strategy), "search")
            .mm(paste0("acc_levels: ", acc_levels), "search")
            .mm(paste0("other: ", unlist(list(...))), "search")
            
            #return(as.list(match.call(expand.dots = TRUE)))
            
            
            #argg <- c(as.list(environment()), list(...))
            #return(argg)
            
            
            #l <- as.list(match.call(expand.dots = TRUE))
            #print(l)
            #return(get(as.character((l[3]))))
            
            
            #Didn't work
            #return(as.list(get(
            #    as.character(unlist((match.call(expand.dots = TRUE)))))))
            
        }
    }
    
    database_name <- "sra_con"
    database_env <- ".GlobalEnv"
    
    query_full <- paste0("SELECT * FROM sra_ft WHERE sra_ft MATCH '", 
                                                            SRA_query, "'")
    
    if (!is.null(SRA_library_strategy)){
        
        # other library strategy clause
        ls_query <- paste0("library_strategy = '", 
                            SRA_library_strategy, sep = "' OR ", collapse = "")
        
        
        if(sum(c("OTHER", "NA", "NULL") %in% SRA_other_library_strategy) > 0 ){
            ols_clause <- character()
            ols_clause[1] <- "library_strategy = 'OTHER'"
            ols_clause[2] <- "library_strategy = 'NA'"
            ols_clause[3] <- "library_strategy IS NULL"
            
            ols_check <- logical()
            ols_check[1] <- "OTHER" %in% SRA_other_library_strategy
            ols_check[2] <- "NA" %in% SRA_other_library_strategy
            ols_check[3] <- "NULL" %in% SRA_other_library_strategy
            
            # SRA_other_library_strategy clause
            ols_query <- 
                    paste0(ols_clause[ols_check], sep = " OR ", collapse = "")
            
            ls_query <- paste0(ls_query, ols_query)
            
            .mm(ls_query, "query")
            
        }
        
        ls_query <- substr(ls_query, 1, nchar(ls_query)-4)
        query_full <- paste0(query_full, " AND ( ", ls_query, ")")
        
    }
    
    .mm(query_full, "query")
    
    
    
    
    
    query_check <- list(...)$query_check
    if(!is.null(query_check)){
        if (isTRUE(query_check)){
            return(query_full)
        }
    }
    
    
    df <- DBI::dbGetQuery(get(database_name, 
                                    envir = get(database_env)), query_full)
    
    
    
    
    
    .vex("temp_searchAnywhereSRA", df)
    
    if (dim(df)[1]!=0){
        .mm("Filtering results according to accession levels...", "prog")
        df <- .filterSRAByTermByAccessionLevel(SRA_query, df, acc_levels)
    }
    
    
    
    # Must come after filtering, otherwise it would not work
    df <- .renameSRAColumns(df)
    
    if (dim(df)[1]==0){
        warning(paste0("No results found in SRA. ",
            "Try refining your search terms or acc_levels"), call. = FALSE)
    }
    
    return(df)
    #------TBC
    # ===*===
}




#' Filter df according to query matches 
#' only within accession levels of interest
#' 
#' Performs fts search on the data frame according to the query, only searching
#' in the columns corresponding to specified accession levels of interest
#' 
#' 
#' @param query Query to be passed to MATCH operator (for fts)
#' @param df Data frame to be filtered
#' @param acc_levels Accession levels to search within 
#'     (choose from: run, experiment, sample, study; 
#'     defaults to c("run", "experiment", "sample"))
#' @return Filtered df (containing only rows matching query 
#'     within specified accession levels)
#' 
#' 
#' @keywords internal
#' 
.filterSRAByTermByAccessionLevel <- function(query, 
                            df, 
                            acc_levels = c("run", "experiment", "sample")){
    
    # =====================================================================
    #warning("Only works for SRA") # ===*===
    # =====================================================================
    
    acc_possible <- c("run", "experiment", "sample", "study")
    acc_levels <- match.arg(acc_levels, acc_possible, several.ok = TRUE)
    
    .mm(paste0("Filtering for the following acc_levels: ", 
                            paste0(acc_levels, collapse = ", ")), "search")
    if (sum(unique(acc_levels) %in% c("study", 
                                        "sample", 
                                        "experiment", 
                                        "run"))==4){
        .mm("Nothing to filter - returning original df", "adverse")
        return(df)
    }
    
    df_out <- 
        filterByTerm(df = df, 
                    query = query, 
                    filter_columns = 
                        .findSRAAccessionLevelColumnNames(acc_levels = 
                                                                acc_levels))
    
    return(df_out)
    
}



#' Filter df according to query matches 
#' only within accession levels of interest
#' 
#' Performs fts search on the data frame according to the query, only searching
#' in the columns corresponding to specified accession levels of interest
#' 
#' 
#' @param query Query to be passed to MATCH operator (for fts)
#' @param df Data frame to be filtered
#' @param acc_levels Accession levels to search within 
#'     (choose from: run, experiment, sample, study, gsm, gse); 
#'     defaults to c("run", "experiment", "sample", "gsm")
#' @return Filtered df (containing only rows matching query 
#'     within specified accession levels)
#' 
#' @family Workflow functions
#' @family Manipulation functions
#' 
#' 
#' @export
#' 
filterByTermByAccessionLevel <- function(
                        query, 
                        df, 
                        acc_levels = c("run", "experiment", "sample", "gsm")){
    
    # =====================================================================
    # warning("Only works for SRA") # ===*===
    # =====================================================================
    
    acc_possible <- c("run", "experiment", "sample", "study", "gsm", "gse")
    acc_levels <- match.arg(acc_levels, acc_possible, several.ok = TRUE)
    
    .mm(paste0("Filtering for the following acc_levels: ", 
                            paste0(acc_levels, collapse = ", ")), "search")
    
    if (sum(unique(acc_levels) %in% c("study", 
                                        "sample", 
                                        "experiment", 
                                        "run", "gsm", "gse"))==6){
        .mm("Nothing to filter - returning original df", "adverse")
        return(df)
    }
    
    acc_columns <- .findAccessionLevelColumnNames(acc_levels = acc_levels)
    
    
    df_out <- filterByTerm(df = df,query = query, filter_columns = acc_columns)
    
    return(df_out)
    
}







#' Convert from SRA-GEO Categories
#' 
#' Converts from SRA-GEO Categories to corresponding SRA library_strategy 
#' and GEO (study) type.
#' For further details regarding available categories (and their 
#' corresponding elements), inspect the \code{SRA_GEO_Category_Conversion} 
#' object or see its documentation page: \code{?SRA_GEO_Category_Conversion}.
#'
#' @param x Character with a category (can be a vector). 
#'     NOTE: must match exactly (but matching is case insensitive)
#' @return A list with a vector each for SRA_library_strategy and GEO_type
#' 
#' 
#' @examples 
#' convertCategoriesToLibraryStrategyType("Transcriptome MA")
#' convertCategoriesToLibraryStrategyType("ChIP-Seq")
#' 
#' 
#' @export
convertCategoriesToLibraryStrategyType <- function(x){
    
    # devtools::check()
    DB <- NULL
    
    
    #utils::data("SRA_GEO_Category_Conversion", envir = environment())
    
    # Retrieve category conversion data frame
    df <- SpiderSeqR::SRA_GEO_Category_Conversion 
    
    # Make matching case insensitive
    x <- tolower(x)
    df_lower <- df
    df_lower$Category <- tolower(df_lower$Category)
    
    
    if (sum(df_lower$Category %in% x)==0) stop("Provide a valid category")
    
    df <- df[df_lower$Category %in% x,] #Filter by matching category/categories
    
    y <- list()
    
    #.vex("temp_convertCat_y_ori", df)
    
    
    y$SRA_library_strategy <- dplyr::filter(df, DB == "SRA")$Name
    y$GEO_type <- dplyr::filter(df, DB == "GEO")$Name
    
    if (length(y$SRA_library_strategy)==0){
        # This is a slight trick to avoid losing that list element altogether
        y[1] <- list(NULL) 
    }
    if (length(y$GEO_type)==0){
        # This is a slight trick to avoid losing that list element altogether
        y[2] <- list(NULL) 
    }
    
    #.vex("temp_convertCat_y_later", y)
    
    #if (length(y$SRA_library_strategy)==0){
    #  y$SRA_library_strategy <- NULL
    #}
    
    #if (length(y$GEO_type)==0){
    #  y$GEO_type <- NULL
    #}
    
    #y$SRA_library_strategy <- df$Name[df$DB=="SRA"]
    #y$GEO_type <- df$Name <- df$Name[df$DB=="GEO"]
    
    return(y)
    
}


#' Find SRA column names corresponding to accession levels (ORI)
#' 
#' @param acc_levels Accession levels
#' @param add_run_accession Logical indicating whether to add 
#'     run_accession column name
#' @param table_name A character with table name
#' @return Vector with column names
#' 
#' NOTE: works on the original column names (from the database)
#' 
#' @examples 
#' # .findSRAAccessionLevelColumnNames("run")
#' 
#' @keywords internal
#' 
.findSRAAccessionLevelColumnNames <- 
    function(acc_levels = c("run", "experiment", "sample", "study"), 
                add_run_accession = TRUE, table_name = "sra_ft"){
    
    database_name <- "sra_con"
    database_env <- ".GlobalEnv"
    col_list <- DBI::dbListFields(get(database_name, 
                                        envir = get(database_env)), table_name)
    
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
    
    if (is.null(sel_cols)){
        stop("Provide at least one accession level to search within")
    } 
    
    return(sel_cols)
    
}




#' Find column names corresponding to accession levels in SRA and GEO (SP)
#' 
#' @param acc_levels Accession levels
#' @return Vector with column names
#' 
#' NOTE: works on the SpiderSeqR column names (i.e. with prefixes)
#' 
#' @examples 
#' # .findAccessionLevelColumnNames("run")
#' 
#' @keywords internal
#' 
.findAccessionLevelColumnNames <- 
    function(acc_levels = c("run", "experiment", "sample", "gsm")){
        
        sra_accessions <- c("run", "experiment", "sample", "study")
        acc_possible <- c(sra_accessions, "gsm", "gse")
        
        
        acc_levels <- match.arg(acc_levels, acc_possible, several.ok = TRUE)
        
        
        acc_columns <- character()
        if (sum(acc_levels %in% sra_accessions)>0){
            sra_columns <- 
                .findSRAAccessionLevelColumnNames(acc_levels = 
                                    acc_levels[acc_levels %in% sra_accessions])
            
            df <- stats::setNames(data.frame(matrix(ncol = length(sra_columns),
                                                    nrow = 0)), sra_columns)
            df <- .renameSRAColumns(df)
            
            sra_columns <- colnames(df)
            
            acc_columns <- c(acc_columns, sra_columns)
        }
        
        
        if ("gsm" %in% acc_levels){
            acc_columns <- c(acc_columns, listValidColumns()$GSM)
        }
        
        if ("gse" %in% acc_levels){
            acc_columns <- c(acc_columns, listValidColumns()$GSE)
        }
        
        return(acc_columns)
    }


