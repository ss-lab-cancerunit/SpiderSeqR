
#' Prepare the environment to run SpiderSeqR in DEMO mode
#' 
#'
#' This is a minimalist version of \code{\link{startSpiderSeqR}} function 
#' for use as a package DEMO and in the package documentation examples 
#' and testing. It uses small extracts from the relevant databases 
#' to set up database files, just as \code{\link{startSpiderSeqR}} would do. 
#' However, in contrast to \code{\link{startSpiderSeqR}}, no files are created,
#' because the database extracts are stored in memory.
#' 
#' To examine the data frames, which form the content of the databases, 
#' use them directly or search for extra documentation on these data files 
#' (\code{\link{sra_demo}}, \code{\link{srr_demo}}, \code{\link{gse_demo}}, 
#' \code{\link{gsm_demo}}.
#' 
#' @return Nothing. Create relevant connections in the Global Environment
#' 
#' @family Setup functions
#' @examples 
#' startSpiderSeqRDemo() # The function does not take any arguments
#' 
#' @export
#' 
startSpiderSeqRDemo <- function(){
    
    .mm(cli::rule(), "comm")
    .mm("...Setting up the database connections...", "comm")
    
    database_env <- ".GlobalEnv"
    
    # TBD (not necessary)
    #utils::data("gsm_demo", envir = environment())
    #utils::data("gse_demo", envir = environment())
    #utils::data("srr_demo", envir = environment())
    #utils::data("sra_demo", envir = environment())
    
    # Remove old connections if present
    if(exists("sra_con", envir = get(database_env))){
        rm(list = "sra_con", envir = get(database_env))
    }
    if(exists("geo_con", envir = get(database_env))){
        rm(list = "geo_con", envir = get(database_env))
    }
    if(exists("srr_gsm", envir = get(database_env))){
        rm(list = "srr_gsm", envir = get(database_env))
    }
    
    
    # Create connections in the Global Environment
    .GlobalEnv$sra_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    .GlobalEnv$geo_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    .GlobalEnv$srr_gsm <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    
    .createDemoGEO(.GlobalEnv$geo_con)
    .createDemoSRA(.GlobalEnv$sra_con)
    .createDemoSRR_GSM(.GlobalEnv$srr_gsm)
    
    
    .createFtsTable("sra_con", "sra", "sra_ft")
    
    .mm("Welcome to the SpiderSeqR Demo! All set!", "qn")
    .mm(paste0("To use SpiderSeqR for real queries, ",
                    "please run startSpiderSeqR()"), 
            "adverse")
    
    .mm(cli::rule(), "comm")
    #cat(cli::rule(col = "magenta", "\n"))
    
}





#' Create a DEMO GEO database
#' 
#' @param conn A database connection to load the data into
#' @return Nothing. Update the connection object
#' 
#' @keywords internal
#' 
.createDemoGEO <- function(conn){
    DBI::dbWriteTable(conn=conn, name='gsm', value=SpiderSeqR::gsm_demo)
    DBI::dbWriteTable(conn=conn, name='gse', value=SpiderSeqR::gse_demo)
    DBI::dbWriteTable(conn=conn, name='metaInfo', value=geo_metadata)
}



#' Create a DEMO SRA database
#' 
#' @param conn A database connection to load the data into
#' @return Nothing. Update the connection object. 
#' NOTE: does not currently include the creation of sra_ft table, 
#' this is done by .createFtsTable("sra_con", "sra", "sra_ft")
#' 
#' @keywords internal
#' 
.createDemoSRA <- function(conn){
    DBI::dbWriteTable(conn=conn, name='sra', value=SpiderSeqR::sra_demo)
    DBI::dbWriteTable(conn=conn, name='metaInfo', value=sra_metadata)
}



#' Create a DEMO SRR_GSM database
#' 
#' @param conn A database connection to load the data into
#' @return Nothing. Update the connection object
#' 
#' @keywords internal
#' 
.createDemoSRR_GSM <- function(conn){
    DBI::dbWriteTable(conn=conn, name='srr_gsm', value=SpiderSeqR::srr_demo)
    DBI::dbWriteTable(conn=conn, name='metaInfo', value=srr_gsm_metadata)
}

