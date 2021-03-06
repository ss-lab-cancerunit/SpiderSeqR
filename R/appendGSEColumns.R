


#-------

# INPUTS

# GSE44563 <- searchForAccession("GSE44563", file_output = FALSE) #single gse
# GSM277339 <- searchForAccession("GSM277339", file_output = FALSE) #triple gse
# GSM640576 <- searchForAccession("GSM640576", file_output = FALSE) #double gse
# df_whole <- rbind(GSE44563, GSM277339, GSM640576)
# df <- df_whole[ ,c(2, 5,4, 11)]


# df <- dbGetQuery(geo_con, "SELECT gsm, series_id, 
#                 title AS gsm_title, characteristics_ch1 FROM gsm WHERE 
#                 series_id LIKE '%,%' GROUP BY RANDOM() LIMIT 20")


##gse_columns <- c("gse", "title", "type", "pubmed_id")
##gse_columns <- dbListFields(geo_con, "gse")

#--------


#' Append information from GSE columns to a data frame
#' 
#' Performs search for GSEs in the gse table and fetches columns of interest, 
#' which get appended to the existing data frame. In case a sample belongs 
#' to multiple GSEs, the information appears in the same field separated 
#' with a special character.
#' 
#' 
#' 
#' @param df Data frame with series_id column containing GSEs; 
#' gsm column is also required for join purposes
#' @param gse_columns Character vector with names of GSE columns to be appended
#' @return Data frame with appended gse columns 
#' (they will be named with a prepended 'GSE_')
#' 
#' @examples
#' # .appendGSEColumns(df, "pubmed_id") # Append only pubmed_id column
#' # .appendGSEColumns(df, "*") # Append all available columns from gse
#' 
#' 
#' @details 
#' NOTE: pubmed_id column (in gse table from the database generated 
#' by GEOmetadb) appears to only contain approximately 20% of entries 
#' from the online version of GEO (the rest has missing pubmed_ids)
#' 
#' @keywords internal
#' 
#'
#' 
.appendGSEColumns <- function(df, gse_columns){
    
    .mm("Running .appendGSEColumns", "fn")
    .mm("Adding information about GSEs...", "prog")
    gsm <- NULL
    series_id <- NULL
    
    
    database_name <- "geo_con"
    database_env <- ".GlobalEnv"
    
    
    # Checks and housekeeping (arguments) ####
    if (missing(gse_columns)){
        message("No gse_columns provided")
        #warning("No gse_columns provided")
        return(df)
    }
    
    if (is.null(gse_columns)){
        message("No gse_columns provided")
        #warning("No gse_columns provided")
        return(df)
    }
    
    # Check for gsm and series_id columns
    if ( !("gsm" %in% colnames(df) & "series_id" %in% colnames(df))){
        stop("Columns needed for appending GSE data are missing")
        
        # ===*=== is it really appropriate if no gsm/series_id?
        # df_out <- df[gse_columns] <- NA 
        # return(df_out)
    }
    
    all_gse_columns <- DBI::dbListFields(get(database_name, 
                                            envir = get(database_env)), "gse")
    
    # Change * to full column list
    if (length(gse_columns) == 1 & gse_columns[1] == "*"){
        gse_columns <- all_gse_columns
    }
    
    # Check if all gse_columns are within gse
    if (sum(gse_columns %in% all_gse_columns)!=length(gse_columns)){
        stop("All specified columns must be in gse table")
    }
    
    
    # If series_id is not there, append it
    if (!("gse" %in% gse_columns)){
        gse_columns <- c("gse", gse_columns)
    }
    
    # Create a vector with final gse_columns for checking
    final_gse_columns <- gse_columns[!gse_columns %in% "gse"]
    final_gse_columns <- paste0("GSE_", final_gse_columns)
    
    
    # If gse_columns already present, stop
    if (sum(final_gse_columns %in% colnames(df)) == length(final_gse_columns)){
        stop("All GSE columns already exist within the data frame")
    }
    
    # If some gse_columns already present, give warning
    if (sum(final_gse_columns %in% colnames(df)) > 0){
        warning("Some GSE columns already exist within the data frame")
    }
    
    
    # Collapse column list to comma separated string for SQLite
    gse_columns_sql <- paste0(gse_columns, sep = ", ", collapse = "")
    gse_columns_sql <- substr(gse_columns_sql, 1, nchar(gse_columns_sql)-2)
    
    # Convert df to long format (gsm, series_id), 
    # where multiple series_id is in separate rows
    df_ids_long <- df %>% dplyr::select(gsm, series_id)
    df_ids_long <- df_ids_long %>% tidyr::separate_rows(series_id, sep = ",")
    
    # Find a list of gses to search for
    gse_list <- unique(df_ids_long$series_id)
    gse_list <- gse_list[grepl("^GSE\\d\\d\\d+$", gse_list)]
    
    #print("THIS IS THE GSE LIST ===================")
    #print(gse_list)
    
    # Check whether at least one valid GSE is present
    if (length(gse_list)==0){
        #print(gse_columns)
        warning("No valid GSEs present")
        
        # Don't include gsm
        gse_columns <- gse_columns[!gse_columns %in% "gse"] 
        
        # Matrix gets filled with NAs
        df_columns <- 
            stats::setNames(data.frame(matrix(ncol = length(gse_columns), 
                                            nrow = dim(df)[1])), gse_columns) 
        
        df_out <- cbind(df, df_columns)
        df_out <- .renameGSEColumns(df_out)
        #df_out[gse_columns] <- NA # Didn't work
        return(df_out)
    }
    
    
    # Search for information in GSE ####
    gse_df <- data.frame()
    for (s in seq_along(gse_list)){
        
        query <- paste0("SELECT ", gse_columns_sql, 
                        " FROM gse WHERE gse = '", gse_list[s], "'")
        chunk <- DBI::dbGetQuery(get(database_name, 
                                    envir = get(database_env)), query)
        gse_df <- rbind(gse_df, chunk)
    }
    
    colnames(gse_df)[grepl("^gse$", colnames(gse_df))] <- "series_id"
    
    if(dim(gse_df)[2]==0){
        warning("No matching GSEs found")
        df_out <- df[gse_columns] <- NA
        return(df_out)
    }
    
    # Rename columns
    # colnames(gse_df)[colnames(gse_df)!="series_id"] <- 
    #    paste0("GSE_", colnames(gse_df)[colnames(gse_df)!="series_id"])
    gse_df <- .renameGSEColumns(gse_df)
    
    # Join (and collapse) gse_df and df_ids_long dfs ####
    
    df_ids_long <- df_ids_long %>% dplyr::left_join(gse_df, by = "series_id")
    
    collapsible_columns <- colnames(df_ids_long)
    collapsible_columns <- collapsible_columns[!grepl("gsm", 
                                                    collapsible_columns)]
    
    # collapsible_columns <- collapsible_columns[!grepl("series_id", 
    #                                                collapsible_columns)]
    
    
    collapse_str <- ";;"
    x <- character()
    for (c in seq_along(collapsible_columns)){
        
        # for (c in 1){
        # x[c] <- paste0(collapsible_columns[c], " = paste(", 
        #                collapsible_columns[c], ", collapse = ';;' )" )
        # x[c] <- paste0(collapsible_columns[c], " = paste(unique(", 
        #                collapsible_columns[c], "), collapse = '\\\\\\\\' )" )
        
        x[c] <- paste0(collapsible_columns[c], " = paste(unique(", 
                    collapsible_columns[c], "), collapse = collapse_str )" )
    }
    
    collapse_expression <- paste0(x, collapse = "", sep = ", ")
    collapse_expression <- substr(collapse_expression, 1, 
                                    nchar(collapse_expression)-2)
    
    
    collapse_expression <- 
        paste0("df_ids_long %>% dplyr::group_by(gsm) %>% dplyr::summarise(", 
                collapse_expression, ") %>%dplyr::ungroup()")
    
    # Perform above (stitched) commands
    df_ids_long <- eval(parse(text = collapse_expression)) 
    
    
    # Convert back to normal df format
    df_ids_long <- as.data.frame(df_ids_long)
    
    
    # Cosmetic changes ####
    # Remove NAs mixed in with other patterns
    # Logic: gsub(“(NA,|,NA)”, “”, x) if "," is collapse_str
    na_pattern <- paste0("(NA", collapse_str, "|", collapse_str, "NA)")
    for (x in collapsible_columns){
        df_ids_long[ , x] <- gsub(na_pattern, "", df_ids_long[, x]) 
        df_ids_long[ (df_ids_long[, x]=="NA") , x] <- NA
    }
    
    
    
    # Replace ";;" with "," for series_id and GSE_pubmed_id (if exists)
    df_ids_long$series_id <- gsub(collapse_str, ",", df_ids_long$series_id)
    if (sum(grepl("^GSE_pubmed_id$", colnames(df_ids_long)))>0) {
        df_ids_long$GSE_pubmed_id <- gsub(collapse_str, ",", 
                                            df_ids_long$GSE_pubmed_id)
    }
    
    # Join with the original df ####
    df_out <- df %>% dplyr::left_join(df_ids_long, 
                                        by = c("gsm", "series_id"))
    
    ## This order is better for display purposes
    # df_out <- df_ids_long %>% dplyr::left_join(df, 
    #                                              by = c("gsm", "series_id")) 
    
    df_out <- as.data.frame(df_out)
    .vex("temp_df_out", df_out)
    
    .mm(".appendGSEColumns completed", "fn")
    return(df_out)
    
}
