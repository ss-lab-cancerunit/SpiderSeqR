

#' Check if DB file is in date (based on metaInfo timestamp)
#' 
#' @param conn Database connection
#' @param db Database name (one of: SRA, GEO, SpiderSeqR)
#' @param sra_expiry,geo_expiry,srr_gsm_expiry Numeric with expiry dates
#' @return A logical indicating whether the file is in date (TRUE) or not 
#' (FALSE)
#' 
#' @keywords internal
#' 
.checkFileDate <- function(conn, db,  sra_expiry, geo_expiry, srr_gsm_expiry){
    
    db <- match.arg(db, c("SRA", "GEO", "SpiderSeqR"))
    
    df <- DBI::dbGetQuery(conn=conn, "SELECT * FROM metaInfo")
    db_date <- df[grepl("timestamp", df$name ), 2]
    db_date <- as.Date(db_date)
    
    if (db %in% c("SRA", "GEO")){
        if (db=="SRA") expiry <- sra_expiry
        if(db=="GEO") expiry <- geo_expiry
        
        if (as.numeric(Sys.Date()-as.Date(db_date))>expiry){
            check_result <- FALSE
        } else {
            check_result <- TRUE
        }
        
    } else {
        
        expiry <- srr_gsm_expiry
        checks <- as.numeric(Sys.Date()-temp)>expiry
        
        if (FALSE %in% checks){
            check_result <- FALSE
        } else {
            check_result <- TRUE
        }
        
    }
    
    return(check_result)
}

