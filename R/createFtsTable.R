
#----------------------------------------------------

#database_name <- "db_demo_ft"
#table_name <- "sra"
#new_table_name <- "sra_demo_ft"

#rm(db_demo_ft)
#db_demo_ft <- dbConnect(SQLite(), dbname = "SRAmetadb_demo_ft.sqlite")

#createFtsTable("db_demo_ft", "sra", "sra_demo_ft")

#----------------------------------------------------
#Creating gsm_ft

#Uncomment to run and change to gsm if needed

#geo_ft <- dbConnect(SQLite(), dbname = "GEOmetadb_ft.sqlite")
#start_time <- Sys.time()
#createFtsTable("geo_ft", "gse", "gse_ft")
#end_time <- Sys.time()
#end_time - start_time

# NOTE on timings for GSE: about 1 min
#
# NOTE on timings for GSM
# 16:47 start
# 17:05 insertion finished (18 min)
# 17:09 optimisation finished (22 min)
# 17:59 sqlite result finished (72 min)
# NOTE: Sys.time() above recorded finish time as 17:12, suggesting that display of SQLiteResult was not included in the calculation
# Size increased from 8.8 GB to 21.2 GB (from 8791856128 to 21158208512)

#----------------------------------------------------



#' Create a new table in a dabase to enable fulltext search (FTS)
#' 
#' @param database_name Name of database connection
#' @param table_name Name of the table to duplicate as FTS table
#' @param new_table_name Name of the new FTS table
#' @return Nothing. Update the relevant database (stored in memory or in a '*.sqlite' file)
#' 
#' @description 
#' Creates a new virtual (fts) table with exact same columns as the original table, populates it with the contents of original table and optimises the table.
#' 
#' 
#' @keywords internal
#' 
createFtsTable <- function(database_name, table_name, new_table_name){
  
  table_list <- DBI::dbListTables(get(database_name, envir = .GlobalEnv))
  if (new_table_name %in% table_list) stop("The table already exists")
  
  
  # Create table
  column_names <- DBI::dbListFields(get(database_name, envir = .GlobalEnv), table_name)
  column_names <- paste0(column_names, collapse = "", sep = ", ")
  column_names <- substr(column_names, 1, nchar(column_names)-2)
  
  # NOTE: fts4 used previously, but SRAdb uses fts3
  creation_query <- paste0("CREATE VIRTUAL TABLE ", new_table_name, " USING fts3 (", column_names, ")")
  print(creation_query)
  rs <- DBI::dbSendQuery(get(database_name, envir = .GlobalEnv), creation_query)
  DBI::dbClearResult(rs)
  
  print("Number of entries to be copied:")
  print(as.numeric(DBI::dbGetQuery(get(database_name, envir = .GlobalEnv), paste0("SELECT count(*) FROM ", table_name))))
  
  # Insert contents into table
  insertion_query <- paste0("INSERT INTO ", new_table_name, " SELECT * FROM ", table_name)
  print(insertion_query)
  rs <- DBI::dbSendQuery(get(database_name, envir = .GlobalEnv), insertion_query)
  DBI::dbClearResult(rs)
  
  print("Number of entries in new table:")
  print(as.numeric(DBI::dbGetQuery(get(database_name, envir = .GlobalEnv), paste0("SELECT count(*) FROM ", new_table_name))))
  
  # Optimise the database
  optimisation_query <- paste0( "INSERT INTO ", new_table_name, "(", new_table_name, ")", " VALUES('optimize')")
  rs <- DBI::dbSendQuery(get(database_name, envir = .GlobalEnv), optimisation_query)
  DBI::dbClearResult(rs)
  
}



