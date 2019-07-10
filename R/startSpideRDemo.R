
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
  

  # Remove old connections if present
  if(exists("sra_con", envir = get(database_env))) rm(list = "sra_con", envir = get(database_env))
  if(exists("geo_con", envir = get(database_env))) rm(list = "geo_con", envir = get(database_env))
  if(exists("srr_gsm", envir = get(database_env))) rm(list = "srr_gsm", envir = get(database_env))
  
  
  # Create connections in the Global Environment
  .GlobalEnv$sra_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  .GlobalEnv$geo_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  .GlobalEnv$srr_gsm <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  
  
  
  DBI::dbWriteTable(conn=get("geo_con", envir = get(database_env)), name='gsm', value=gsm_demo)
  DBI::dbWriteTable(conn=get("geo_con", envir = get(database_env)), name='gse', value=gse_demo)
  DBI::dbWriteTable(conn=get("srr_gsm", envir = get(database_env)), name='srr_gsm', value=srr_demo)
  DBI::dbWriteTable(conn=get("sra_con", envir = get(database_env)), name='sra', value=sra_demo)
  createFtsTable("sra_con", "sra", "sra_ft")
  
}




startSpideRDemoPrev <- function(){
  
  database_env <- ".GlobalEnv"
  
  sra_demo_file <- "SRAmetadb_DEMO.sqlite"
  geo_demo_file <- "GSEmetadb_DEMO.sqlite"
  srr_demo_file <- "SRR_GSM_DEMO.sqlite"
  
  
  if(file.exists(sra_demo_file)) file.remove(sra_demo_file)
  if(file.exists(geo_demo_file)) file.remove(geo_demo_file)
  if(file.exists(srr_demo_file)) file.remove(srr_demo_file)
  
  
  # Create connections in the Global Environment
  .GlobalEnv$sra_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = sra_demo_file)
  .GlobalEnv$geo_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = geo_demo_file)
  .GlobalEnv$srr_gsm <- DBI::dbConnect(RSQLite::SQLite(), dbname = srr_demo_file)
  
  
  
  DBI::dbWriteTable(conn=get("geo_con", envir = get(database_env)), name='gsm', value=gsm_demo)
  DBI::dbWriteTable(conn=get("geo_con", envir = get(database_env)), name='gse', value=gse_demo)
  DBI::dbWriteTable(conn=get("srr_gsm", envir = get(database_env)), name='srr_gsm', value=srr_demo)
  DBI::dbWriteTable(conn=get("sra_con", envir = get(database_env)), name='sra', value=sra_demo)
  createFtsTable("sra_con", "sra", "sra_ft")
  
}

