

#' Get summaries of database contents
#' 
#' \code{getDatabaseInformation} allows users to get insights into the contents of certain parts of the databases. Run the function and select the menu option with the summary of interest.
#' 
#' @return Data frame with requested information
#' 
#' @section Available information:
#' In order to obtain the most up to date list of available information, run the function
#' Currently, it includes:
#' \itemize{
#'   \item \emph{Library_strategy counts in SRA database} - a dataframe with all the library strategies featuring in SRA and their counts
#'   \item \emph{taxon_id's and their counts}
#'   \item \emph{source_name_ch1 and their counts}
#'   \item \emph{label_ch and their counts}
#'   \item \emph{molecule_ch1 and their counts}
#'   \item \emph{Number of accessions} in the SRA and GEO databases
#'   \item (more in progress... ===*===)
#' }
#' 
#' @export
getDatabaseInformation <- function(){
  
  database_env <- ".GlobalEnv"
  sra_database_name <- "sra_con"
  geo_database_name <- "geo_con"
  
  
  print("What kind of database information are you interested in?")
  menu_options <- menu(c("SRA: Available library_strategy types (and their counts)", #1
                         "SRA: Available taxon_id's (and their counts)", #2
                         "GEO: Available source_name_ch1 (and their counts)", #3
                         "GEO: Available label_ch1 (and their counts)", #4
                         "GEO: Available molecule_ch1 (and their counts)", #5
                         "SRA: Number of run accessions (SRR)", #6
                         "SRA: Number of experiment accessions (SRX)", #7
                         "SRA: Number of sample accessions (SRS)", #8
                         "SRA: Number of study accessions (SRP)", #9
                         "None (exit)"))
  
  
  
  if (menu_options == 1){ # library_strategy
    
    df <- DBI::dbGetQuery(get(sra_database_name, envir = get(database_env)), "SELECT library_strategy, count(*) AS count FROM sra GROUP BY library_strategy ORDER BY count DESC")
    #df <- df[order(df$count, decreasing = TRUE),]
    #rownames(df) <- NULL
    return(df)
    
  } else if (menu_options == 2 ){
    df <- DBI::dbGetQuery(get(sra_database_name, envir = get(database_env)), "SELECT taxon_id, count(*) AS count FROM sra GROUP BY taxon_id ORDER BY count DESC")
    return(df)
    
  } else if (menu_options == 3){
    df <- DBI::dbGetQuery(get(geo_database_name, envir = get(database_env)), "SELECT source_name_ch1, count(*) AS count FROM gsm GROUP BY source_name_ch1 ORDER BY count DESC")
    return(df)
    
  } else if (menu_options == 4){
    df <- DBI::dbGetQuery(get(geo_database_name, envir = get(database_env)), "SELECT label_ch1, count(*) AS count FROM gsm GROUP BY label_ch1 ORDER BY count DESC")
    return(df)
    
  } else if (menu_options == 5){
    df <- DBI::dbGetQuery(get(geo_database_name, envir = get(database_env)), "SELECT molecule_ch1, count(*) AS count FROM gsm GROUP BY molecule_ch1 ORDER BY count DESC")
    return(df)
    
  } else if (menu_options == 6){
    
    df <- DBI::dbGetQuery(get(sra_database_name, envir = get(database_env)), "SELECT count (DISTINCT run_accession) FROM sra")
    return(df)
    
  } else if (menu_options == 7){
    
    df <- DBI::dbGetQuery(get(sra_database_name, envir = get(database_env)), "SELECT count (DISTINCT experiment_accession) FROM sra")
    return(df)
    
  } else if (menu_options == 8){
    
    df <- DBI::dbGetQuery(get(sra_database_name, envir = get(database_env)), "SELECT count (DISTINCT sample_accession) FROM sra")
    return(df)
    
  } else if (menu_options == 9){
    
    df <- DBI::dbGetQuery(get(sra_database_name, envir = get(database_env)), "SELECT count (DISTINCT study_accession) FROM sra")
    return(df)
    
  } else if (menu_options == 10){
    
    print("Nothing to investigate")
    return(NULL)
    
  }
  
}