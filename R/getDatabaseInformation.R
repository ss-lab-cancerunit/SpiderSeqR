

#' Get summaries of database contents
#' 
#' \code{getDatabaseInformation} allows users to get insights 
#' into the contents of certain parts of the databases. 
#' Run the function and select the menu option with the summary of interest.
#' 
#' @return Data frame with requested information
#' 
#' @section Available information:
#' In order to obtain the most up to date list of available information, 
#' please run the function
#' Some of the information available includes:
#' \itemize{
#'   \item \emph{Library_strategy counts in SRA database} - a dataframe 
#'   with all the library strategies featuring in SRA and their counts
#'   \item \emph{taxon_id's and their counts}
#'   \item \emph{source_name_ch1 and their counts}
#'   \item \emph{label_ch and their counts}
#'   \item \emph{molecule_ch1 and their counts}
#'   \item \emph{Number of accessions} in the SRA and GEO databases
#'   \item ...
#' }
#' 
#' @examples 
#' # getDatabaseInformation()
#' 
#' 
#' @export
getDatabaseInformation <- function(){
    
    database_env <- ".GlobalEnv"
    sra_database_name <- "sra_con"
    geo_database_name <- "geo_con"
    
    
    
    col_desc_table_info <- paste0("NOTE: the following data frame ", 
            "is a convenient way of accessing the column description tables ",
            "from databases of SRAdb and GEOmetadb packages. ",
            "Currently GEO provides much better documented descriptions, ",
            "whereas SRA column descriptions are only partially complete. ",
            "Please note that SpiderSeqR only utilises information from gsm ",
            "and gse tables of GEO and sra table for GEO (which includes ",
            "collated information from most of the listed tables, i.e. run, ",
            "experiment, sample, study, submission; however, some columns ",
            "are renamed in sra and not all of the columns from the original ",
            "accession-level tables are used in the sra table).")
    
    
    cat(crayon::magenta(crayon::bold("getDatabaseInformation:")))
    cat(crayon::magenta(" summaries and handy extracts from the databases\n")) 
    cat(crayon::magenta(paste0("PLEASE NOTE: the order and content of the ",
    "options below may change in the future.\n\n")))
    
    #cat(crayon::magenta("What kind of database 
    #information are you interested in?\n"))
    
    cat(cli::rule(col = "magenta", 
                    left = crayon::bold(paste0("What kind of database ",
                    "information are you interested in?"))))
    
    menu_options <- 
        utils::menu(c("SRA: Available library_strategy types (& their counts)",
            "SRA: Available taxon_id's (& their counts)", #2
            "GEO: Available source_name_ch1 (& their counts)", #3
            "GEO: Available label_ch1 (& their counts)", #4
            "GEO: Available molecule_ch1 (& their counts)", #5
            "SRA: Number of run accessions (SRR)", #6
            "SRA: Number of experiment accessions (SRX)", #7
            "SRA: Number of sample accessions (SRS)", #8
            "SRA: Number of study accessions (SRP)", #9
            "SRA: Random sample of the database (size 20)", #10
            "GEO: Random sample of the GSM database (size 20)", #11
            "GEO: Random sample of the GSE database (size 20)", #12
            "GEO: Number of studies per study type", #13
            "SRA: Column descriptions", #14
            "GEO: Column descriptions", #15
            "None (exit)"))
    
    
    
    if (menu_options == 1){ # library_strategy
        
        df <- 
            DBI::dbGetQuery(get(sra_database_name, envir = get(database_env)),
                            paste0("SELECT library_strategy, count(*) ",
                            "AS count FROM sra GROUP BY library_strategy ",
                            "ORDER BY count DESC"))
        
        #df <- df[order(df$count, decreasing = TRUE),]
        #rownames(df) <- NULL
        return(df)
        
    } else if (menu_options == 2 ){
        df <- 
            DBI::dbGetQuery(get(sra_database_name, envir = get(database_env)), 
                            paste0("SELECT taxon_id, count(*) AS count ",
                            "FROM sra GROUP BY taxon_id ORDER BY count DESC"))
        return(df)
        
    } else if (menu_options == 3){
        df <- 
            DBI::dbGetQuery(get(geo_database_name, envir = get(database_env)), 
                            paste0("SELECT source_name_ch1, count(*) ",
                            "AS count FROM gsm GROUP BY source_name_ch1 ",
                            "ORDER BY count DESC"))
        return(df)
        
    } else if (menu_options == 4){
        df <- 
            DBI::dbGetQuery(get(geo_database_name, envir = get(database_env)), 
                            paste0("SELECT label_ch1, count(*) AS count ",
                            "FROM gsm GROUP BY label_ch1 ORDER BY count DESC"))
        return(df)
        
    } else if (menu_options == 5){
        df <- 
            DBI::dbGetQuery(get(geo_database_name, envir = get(database_env)), 
                            paste0("SELECT molecule_ch1, count(*) AS count ",
                            "FROM gsm GROUP BY molecule_ch1 ",
                            "ORDER BY count DESC"))
        return(df)
        
    } else if (menu_options == 6){
        df <- 
            DBI::dbGetQuery(get(sra_database_name, envir = get(database_env)), 
                            paste0("SELECT count (DISTINCT run_accession) ",
                            "FROM sra"))
        return(df)
        
    } else if (menu_options == 7){
        df <- 
            DBI::dbGetQuery(get(sra_database_name, envir = get(database_env)), 
                            paste0("SELECT count (DISTINCT ",
                            "experiment_accession) FROM sra"))
        return(df)
        
    } else if (menu_options == 8){
        
        df <- 
            DBI::dbGetQuery(get(sra_database_name, envir = get(database_env)), 
                            paste0("SELECT count (DISTINCT sample_accession) ",
                            "FROM sra"))
        return(df)
        
    } else if (menu_options == 9){
        
        df <- 
            DBI::dbGetQuery(get(sra_database_name, envir = get(database_env)), 
                            paste0("SELECT count (DISTINCT study_accession) ",
                            "FROM sra"))
        return(df)
        
    } else if (menu_options == 10){
        
        df <- 
            DBI::dbGetQuery(get(sra_database_name, envir = get(database_env)), 
                            paste0("SELECT * FROM sra ORDER BY ",
                            "RANDOM() LIMIT 20"))
        
    } else if (menu_options == 11){
        
        df <- 
            DBI::dbGetQuery(get(geo_database_name, envir = get(database_env)), 
                            paste0("SELECT * FROM gsm ORDER BY RANDOM() ",
                            "LIMIT 20"))
        
    } else if (menu_options == 12){
        
        df <- 
            DBI::dbGetQuery(get(geo_database_name, envir = get(database_env)), 
                            paste0("SELECT * FROM gse ORDER BY RANDOM() ",
                            "LIMIT 20"))
        
    } else if (menu_options == 13){
        
        # Devtools::check()
        type <- NULL
        total <- NULL
        
        df <- 
            DBI::dbGetQuery(get(geo_database_name, envir = get(database_env)), 
                            paste0("SELECT type, count(*) AS total FROM gse ",
                            "GROUP BY type")) 
                            #Frequency table of study types in GSE table
        
        df <- df %>% 
            tidyr::separate_rows(type, sep = ";\t") %>% 
            dplyr::group_by(type) %>% 
            dplyr::summarise_all(sum) %>% 
            dplyr::arrange(dplyr::desc(total))
        
        df <- as.data.frame(df)
        return(df)
        
    } else if (menu_options == 14){
        
        cat(crayon::magenta(col_desc_table_info))
        df <- 
            DBI::dbGetQuery(get(sra_database_name, envir = get(database_env)),
                            "SELECT * FROM col_desc")
        return(df)
        
        
    } else if (menu_options == 15){
        
        cat(crayon::magenta(col_desc_table_info))
        df <- 
            DBI::dbGetQuery(get(geo_database_name, envir = get(database_env)),
                            "SELECT * FROM geodb_column_desc")
        return(df)
        
    } else if (menu_options == 16){
        
        cat(crayon::magenta("Nothing to investigate"))
        invisible()
        
    }
    
}





#' Get Descriptions of Columns within Databases
#' 
#' @return Data frame with column descriptions
#' 
#' This is a function for convenience of accessing column description tables 
#' for databases from SRAdb and GEOmetadb packages.
#' Currently GEO provides much better documented descriptions, 
#' whereas SRA column descriptions are only partially complete. 
#' Please note that SpiderSeqR only utilises information 
#' from gsm and gse tables of GEO and sra table for GEO 
#' (which includes collated information from most of the listed tables, 
#' i.e. run, experiment, sample, study, submission).
#' 
#' 
#' Currently a duplicate of functionality from \code{getDatabaseInformation()}
#'
getColumnDescriptions <- function(){
    
    database_env <- ".GlobalEnv"
    sra_database_name <- "sra_con"
    geo_database_name <- "geo_con"
    
    .mm("Which database are you interested in?", "qn")
    db_choices <- utils::menu(c("SRA database", "GEO database"))
    if (db_choices == 1){
        df <- 
            DBI::dbGetQuery(get(sra_database_name, envir = get(database_env)),
                            "SELECT * FROM col_desc")
        return(df)
    } else if (db_choices ==2){
        df <- 
            DBI::dbGetQuery(get(geo_database_name, envir = get(database_env)),
                            "SELECT * FROM geodb_column_desc")
        return(df)
    }
    
}