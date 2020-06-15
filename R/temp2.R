

#' A minimal example on setting up the database connections 
#' without using Global Environment
#' 
#' NOTE: setSpiderSeqROption would be done in startSpiderSeqR()
#' 
setSpiderSeqROption("geo_file", 
                    "C:/DD/Projects/SpideRs/SpiderSeqR-Auxillaries/Database_Files/GEOmetadb.sqlite")

.ConnectGEO <- function(){
    return(DBI::dbConnect(RSQLite::SQLite(), getSpiderSeqROption("geo_file")))
}