
#' Prepare the environment to run SpideR (DEMO)
#' 
#' @param None
#' @return Nothing. Create relevant connections in the Global Environment
#' 
#' This is a minimalist version of \code{startSpideR} function for use in package examples. It uses small extracts from the relevant databases to set up database files, just as \code{startSpideR} would do.
#' 
startSpideRDemo <- function(){
  
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

