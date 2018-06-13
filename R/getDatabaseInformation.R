

#' Get summarised information on database contents
#' 
#' \code{getDatabaseInformation} allows users to get insights into the contents of the database. Run the function and select the menu option with the summary of interest.
#' 
#' @return Data frame with requested information
#' 
#' @section Available information:
#' \itemize{
#'   \item \emph{Library_strategy counts in SRA database} - a dataframe with all the library strategies featuring in SRA and their counts
#' }
#' 
#' @export
getDatabaseInformation <- function(){
  
  database_env <- ".GlobalEnv"
  sra_database_name <- "sra_con"
  
  
  print("What kind of database information are you interested in?")
  menu_options <- menu(c("Available library_strategy types (and their counts)", 
                         "Available taxon_id's (and their counts)",
                         "None (exit)"))
  
  if (menu_options == 1){ # library_strategy
    
    df <- DBI::dbGetQuery(get(sra_database_name, envir = get(database_env)), "SELECT library_strategy, count(*) AS count FROM sra GROUP BY library_strategy ORDER BY count DESC")
    #df <- df[order(df$count, decreasing = TRUE),]
    #rownames(df) <- NULL
    return(df)
    
  } else if (menu_options ==2 ){
    df <- DBI::dbGetQuery(get(sra_database_name, envir = get(database_env)), "SELECT taxon_id, count(*) AS count FROM sra GROUP BY taxon_id ORDER BY count DESC")
    return(df)
    
  } else if (menu_options == 3){
    
    print("Nothing to investigate")
    return(NULL)
    
  }
  
}