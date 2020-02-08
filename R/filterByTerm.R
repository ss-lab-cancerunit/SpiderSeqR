


#' Filter data frame by term
#'  
#' @param df Data frame
#' @param query Character string for fts search
#' @param filter_columns Character vector with column names
#' @return A data frame subset according to query conditions
#' 
#' @description
#' Filter a data frame using full-text search within the data frame. If filter_columns are not specified, search will be performed within the whole data frame. If filter_columns are provided, search will only be limited to these columns.
#' Full-text search works akin to searching within web search engines like Google. You can find out more about it here: https://www.sqlite.org/fts3.html (fts3 is used for consistency with fts search within the SRAdb database).
#' 
#' @examples 
#' startSpiderSeqRDemo()
#' df <- searchForAccession("SRP026280")
#' df <- filterByTerm(df, "TRIMKD") # Filter by 'TRIMKD' anywhere within df
#' 
#' @export
#' 
filterByTerm <- function(df, query, filter_columns=NULL){
  # - Check all columns within df
  # - add ord column with an index (in case duplicates exist etc.)
  # - subset df according to columns
  # (- make sure sqlite file does not exist, if it does, delete it)
  # - create a database, with fts table
  # - fts query to database
  # - filter the original df based on the query result
  # - remove additional ord column
  
  filter_con <- NULL
  ord <- NULL
  

  if (!("data.frame" %in% class(df))){
    stop("Object needs to be a data frame")
  }
  
  if ("ord" %in% colnames(df)){
    warning("Data frame contains 'ord' column, which will be overwritten")
  }
  
  # Create a new column with an index
  df$ord <- seq(dim(df)[1])
  
  
  if (!is.null(filter_columns)){
    if (!all(filter_columns %in% colnames(df))){
      stop("All columns need to be within the data frame")
    }
    col_ind <- colnames(df) %in% c("ord", filter_columns) # Get logical of column indices to search within, including ord
  } else {
    col_ind <- rep(TRUE, dim(df)[2])
  }


  df_subset <- df[, col_ind]

  
  # Create db
  filter_db_file <- "filter_db.sqlite"
  
  if (file.exists(filter_db_file)) file.remove(filter_db_file)
  
  
  .GlobalEnv$filter_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = filter_db_file)
  DBI::dbWriteTable(conn = filter_con, name="df", value = df_subset)
  
  # Create fts table of the db
  createFtsTable("filter_con", "df", "df_ft")
  query <- paste0("SELECT ord FROM df_ft WHERE df_ft MATCH '", query, "'")
  
  filt_ord <- DBI::dbGetQuery(.GlobalEnv$filter_con, query)$ord # This will be a df
  
  DBI::dbDisconnect(filter_con)
  file.remove(filter_db_file)
  rm(filter_con, envir = .GlobalEnv)
  
  df <- df %>% dplyr::filter(ord %in% filt_ord)
  df <- df[, !(colnames(df) %in% "ord")]
  
  return(df)
  
}


# subsetSRAByAccessionLevel - will not be needed in the current scenario



