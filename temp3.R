










search_term <- "stat3"
acc_levels <- c("run", "sample")
# List of sra columms





#' Find SRA column names corresponding to accession levels
#' 
#' @param acc_levels Accession levels
#' @return Vector with column names
#' 
#' @examples 
#' findSRAAccessionLevelColumnNames("run")
#' 
#' 
findSRAAccessionLevelColumnNames <- function(acc_levels = c("run", "experiment", "sample", "study")){
  col_list <- dbListFields(sra_con, "sra_ft")
  
  # Store index of first column relevant for an accession level
  run_beg <- grep("^run_ID$", col_list)
  exp_beg <- grep("^experiment_ID$", col_list)
  sample_beg <- grep("^sample_ID$", col_list)
  study_beg <- grep("^study_ID$", col_list)
  
  
  run_cols <- col_list[run_beg:(exp_beg-1)]
  exp_cols <- col_list[exp_beg:(sample_beg-1)]
  sample_cols <- col_list[sample_beg:(study_beg-1)]
  study_cols <- col_list[study_beg:length(col_list)]
  
  # Create a vector with column names of interest
  sel_cols <- NULL
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
#' @param pattern Pattern to check for
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
    curr_match <- grepl(pattern, df[,col_ind[i]], ignore.case = TRUE)
    #sum(curr_match)
    #print(df[curr_match,col_ind[i]])
    row_total <- row_total + curr_match
  }
  
  return(row_total > 0)

}





