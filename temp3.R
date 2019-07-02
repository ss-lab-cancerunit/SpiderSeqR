
# A few functions for filtering based on accession-level information in SRA

search_term <- "stat3"
acc_levels <- c("run", "sample")
# List of sra columms




#' Create a query at a subsection of the table
#' 
#' @param search_term Search term for MATCH function
#' @acc_levels Character vector denoting accession levels of interest
#' @return String with a query
#' 
createLevelSpecificQuery <- function(search_term, acc_levels){
  
  sel_cols <- findSRAAccessionLevelColumnNames(acc_levels)
  
  query <- paste0(sel_cols, ": ", search_term, " OR ", collapse = "")
  query <- substr(query, 1, nchar(query)-4)
  query <- paste0("SELECT * FROM sra_ft WHERE sra_ft MATCH '", query, "'")
  
  return(query)
}




#' Find rows of df which match a pattern within columns of levels of interest
#' @param pattern Pattern to check for (can be a vector, in which case elements are treated as alternatives)
#' @param df Data frame
#' @param acc_levels Accession levels
#' @return Logical vector denoting which rows match criteria
#' 
#' NOTE: this function is case-insensitive
#' 
#' 
findSRAIndicesBasedOnMatchesInLevels <-function(pattern, df, acc_levels){
  
  sel_cols <- findSRAAccessionLevelColumnNames(acc_levels)
  col_ind <- NULL
  
  for (i in seq_along(sel_cols)){
    x <- grep(paste0("^", sel_cols[i], "$"), colnames(df))
    col_ind <- c(col_ind, x)
  }
  
  sel_number <- length(sel_cols)
  row_total <- rep(0, dim(df)[1])
  
  for (i in seq_along(col_ind)){
    #i <- 5
    curr_match <- rep(FALSE, dim(df)[1])
    
    for(j in seq_along(pattern)){
      curr_match <- curr_match | grepl(pattern[j], df[,col_ind[i]], ignore.case = TRUE)
    }

    #sum(curr_match)
    #print(df[curr_match,col_ind[i]])
    row_total <- row_total + curr_match
  }
  
  if (sum(row_total > 0)==0) warning("No matches within the specified accession levels")
  
  return(row_total > 0)

}





