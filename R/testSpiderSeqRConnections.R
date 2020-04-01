
# Function for testing connections which will be used by SpiderSeqR
# Returns counts of total entries within the databases

#' Test SpiderSeqR Connections
#' 
#' @return Nothing. Prints information about the number of entries 
#'    in the key database tables.
#'    
#' @examples
#' startSpiderSeqRDemo()
#' testSpiderSeqRConnections()
#' 
#' @export
#' 
testSpiderSeqRConnections <- function(){
    
    #DBI::dbGetQuery(.GlobalEnv$srr_gsm, "SELECT count(*) FROM srr_gsm")
    
    print(paste0("Entries in sra_con (SRR): ", 
                as.numeric(DBI::dbGetQuery(get("sra_con", envir = .GlobalEnv),
                                            "SELECT count(*) FROM sra"))))
    
    print(paste0("Entries in geo_con (GSM): ", 
                as.numeric(DBI::dbGetQuery(get("geo_con", envir = .GlobalEnv),
                                            "SELECT count(*) FROM gsm"))))
    
    print(paste0("Entries in geo_con (GSE): ", 
                as.numeric(DBI::dbGetQuery(get("geo_con", envir = .GlobalEnv),
                                            "SELECT count(*) FROM gse"))))
    
    print(paste0("Entries in srr_gsm (SRR/GSM): ", 
                as.numeric(DBI::dbGetQuery(get("srr_gsm", envir = .GlobalEnv),
                                            "SELECT count(*) FROM srr_gsm"))))
    
}



# A method for accessing db connections within functions:
# database_name <- "sra_con"
# database_env <- ".GlobalEnv"
# DBI::dbGetQuery(get(database_name, envir = get(database_env)), query)



# Checking whether embedding connections works

testEmbed <- function(){
    database_name <- "sra_con"
    database_env <- ".GlobalEnv"
    return(testCon(get(database_name, envir = get(database_env))))
}

testCon <- function(db_con){
    as.numeric(DBI::dbGetQuery(db_con, "SELECT count(*) FROM sra"))
}
