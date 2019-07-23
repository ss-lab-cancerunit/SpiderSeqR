
#' Prepare the environment to run SpideR DEMO
#' 
#'
#' This is a minimalist version of \code{\link{startSpideR()}} function for use in package documentation examples and testing. It uses small extracts from the relevant databases to set up database files, just as \code{\link{startSpideR()}} would do. However, in contrast to \code{\link{startSpideR()}}, no files are created, because the database extracts are stored in memory.
#' 
#' To examine the data frames, which form the content of the databases, use them directly or search for extra documentation on these data files (\code{\link{?sra_demo}}, \code{\link{?srr_demo}}, \code{\link{?gse_demo}}, \code{\link{?gsm_demo)}}.
#' 
#' @return Nothing. Create relevant connections in the Global Environment
#' 
#' @examples 
#' startSpideRDemo() # The function does not take any arguments
#' 
startSpideRDemo <- function(){
  
  database_env <- ".GlobalEnv"
  

  # TBD (not necessary)
  #utils::data("gsm_demo", envir = environment())
  #utils::data("gse_demo", envir = environment())
  #utils::data("srr_demo", envir = environment())
  #utils::data("sra_demo", envir = environment())
  #print(head(gsm_demo))
  

  # Remove old connections if present
  if(exists("sra_con", envir = get(database_env))) rm(list = "sra_con", envir = get(database_env))
  if(exists("geo_con", envir = get(database_env))) rm(list = "geo_con", envir = get(database_env))
  if(exists("srr_gsm", envir = get(database_env))) rm(list = "srr_gsm", envir = get(database_env))
  
  
  # Create connections in the Global Environment
  .GlobalEnv$sra_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  .GlobalEnv$geo_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  .GlobalEnv$srr_gsm <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  
  
  
  DBI::dbWriteTable(conn=get("geo_con", envir = get(database_env)), name='gsm', value=SpideR::gsm_demo)
  DBI::dbWriteTable(conn=get("geo_con", envir = get(database_env)), name='gse', value=SpideR::gse_demo)
  DBI::dbWriteTable(conn=get("srr_gsm", envir = get(database_env)), name='srr_gsm', value=SpideR::srr_demo)
  DBI::dbWriteTable(conn=get("sra_con", envir = get(database_env)), name='sra', value=SpideR::sra_demo)
  createFtsTable("sra_con", "sra", "sra_ft")
  
}





