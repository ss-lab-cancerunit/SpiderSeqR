

#' Find database files
#' 
#' @param path Path to search within
#' @return A list with paths to database files
#' 
#' @keywords internal
#' 
.findDBFiles <- function(path){
    
    sra_file <- .DBNames()[1]
    geo_file <- .DBNames()[2]
    srr_gsm_file <- .DBNames()[3]
    
    #print("SEARCHING FILES")
    #print(sra_file)
    #print(geo_file)
    #print(srr_gsm_file)
    
    sra_file_name <- sra_file
    geo_file_name <- geo_file
    srr_gsm_file_name <- srr_gsm_file
    
    #==========================================================
    # Checking files and searching within the directory####
    #==========================================================
    # NOTE: will be re-checked again later, this is for expanding search
    # and communicating to the user
    
    .mm(paste0("Searching for database files within: "), "comm")
    .mm(path, "qn")
    
    #print(dir(path))
    
    
    var_list <- c("sra_file", "geo_file", "srr_gsm_file")
    
    for (i in seq_along(var_list)){
        
        match_files <- .findFiles(path, paste0("(^|*)", get(var_list[i])))
        #print(match_files)
        
        # If length = 0, do nothing
        
        if (length(match_files) == 1){
            
            # Normalize path before saving, (only when matched)
            match_files <- normalizePath(match_files) 
            
            .mm(paste0("Found ", get(paste0(var_list[i], "_name")), 
                       " file:\n", match_files), "comm")
            
            assign(var_list[i], match_files) # Substitute the path
            
        } else if (length(match_files > 1)){
            
            for (k in seq_along(match_files)){
                match_files[k] <- normalizePath(match_files[k])
            }
            
            .mm(paste0("Found multiple matching files. ",
                       "Which one would you like to use?"), "qn")
            
            # Let the user choose the file
            file_choice <- utils::menu(match_files)
            assign(var_list[i], match_files[file_choice])
        }
        
    }
    
    
    #file_paths <- list(sra_file=sra_file, 
    #                   geo_file=geo_file, 
    #                   srr_gsm_file=srr_gsm_file)
    
    file_paths <- c(sra_file, geo_file, srr_gsm_file)
    
    return(file_paths)
}



#' Find files (a wrapper around list.files)
#' 
#' @param path A path to be searched
#' @param pattern Regular expression pattern to search for (passed to dir())
#' @return A full path with the matching file(s)
#' 
#' @examples 
#' #.findFiles(getwd(), "*.sqlite")
#' 
#' @keywords internal
#' 
.findFiles <- function(path, pattern){
    
    .mm(paste0("Searching for ", pattern, " files in: ", path), "dev")
    
    matches <- list.files(path = path, pattern = pattern, 
                          recursive = TRUE, full.names = TRUE)
    
    normalizePath(matches)
    
    return(matches)
}




.DBNames <- function(){
    return(c("SRAmetadb.sqlite",
             "GEOmetadb.sqlite",
             "SRR_GSM.sqlite"))
}




.checkDBNames <- function(db_file_name){
    db_file_name <- match.arg(db_file_name, .DBNames())
    return(db_file_name)
}




#' Check for missing files
#' 
#' @param sra_file Path to SRA file
#' @param geo_file Path to GEO file
#' @param srr_gsm_file Path to SRR_GSM file
#' @return A logical vector length 3 indicating whether the respective 
#'     files are present (in order as above). Also prints a relevant message 
#'     to the user
#'     
#' @keywords internal
#' 
.missingFileCheck <- function(file_paths){
    
    # Repeat missing check after extended search done
    #missing_logical <- !file.exists(file_paths)
    missing_logical <- c(!file.exists(file_paths[1]), 
                         !file.exists(file_paths[2]), 
                         !file.exists(file_paths[3]))
    
    # Get a vector with names of missing files (in *.sqlite format)
    missing_files <- .DBNames()
    missing_files <- missing_files[missing_logical]
    
    
    if (sum(missing_logical)==3){
        # ALL missing
        .mm(paste0("The required files could not be found (", 
                   paste0(missing_files, collapse = ", "), ")"), "comm")
        
    } else if (sum(missing_logical) > 0){
        # SOME missing
        .mm(paste0("Some of the required files could not be found (", 
                   paste0(missing_files, collapse = ", "), ")"), "comm")
        
    } else if (sum(missing_logical) == 0){
        # NONE missing
        .mm("Successfully found all database files", "comm")
    }
    
    # General message for (any number of) missing files
    if(sum(missing_logical)>0){
        .mm(paste0("You will shortly be prompted to download/generate ", 
                   "the missing files"), "comm")
        .mm(cli::rule(), "comm")
        #.mm("NOTE:", "qn")
        .mm(paste0("NOTE: The total size of all the files ",
                   "is on the order of a few GB (compressed)\n", 
                   "requiring a few dozen GBs disc space after extraction \n",
                   "(these numbers may change as the databases ",
                   "keep growing)"), "comm")
        #.mm(cli::rule(), "comm")
        
    }
    
    return(missing_logical)
    
}





#' Set expiry parameters
#' 
#' @param general_expiry Maximum number of days since database 
#'     file modification
#' @param sra_expiry,geo_expiry,srr_gsm_expiry Maximum number of days since 
#'    modification of respective database files
#' @param missing_file_number Number of missing files
#' @return Expiry parameters
#' 
#' Sets expiry parameters for three respective databases, according to the 
#' logic that specific parameters should be used where possible and for missing
#' parameters general_expiry will be used (i.e. if all arguments are provided,
#' general_expiry will be ignored).
#' Also checks whether the expiry parameters are numeric and returns 
#' appropriate errors or warnings.
#' 
#' @keywords internal
#' 
.setExpiryParameters <- function(general_expiry, 
                                 sra_expiry, 
                                 geo_expiry, 
                                 srr_gsm_expiry, 
                                 missing_file_number){
    
    # Logic:
    # Use specific parameters (sra, geo, srr_gsm) if available. 
    # If not, use the expiry date from general_expiry
    
    if ((!is.null(general_expiry))&
        (!is.null(sra_expiry))&
        (!is.null(geo_expiry))&
        (!is.null(srr_gsm_expiry))){
        warning(paste0("general_expiry argument will be ignored, since all ",
                       "the individual expiry dates have been provided"))
    }
    
    if (is.null(sra_expiry)){
        sra_expiry <- general_expiry
    }
    if (is.null(geo_expiry)){
        geo_expiry <- general_expiry
    }
    if (is.null(srr_gsm_expiry)){
        srr_gsm_expiry <- general_expiry
    }
    
    
    
    if ( !(is.numeric(general_expiry)) | 
         !(is.numeric(sra_expiry)) | 
         !(is.numeric(geo_expiry)) | 
         !(is.numeric(srr_gsm_expiry)) ){
        stop("Expiry parameters must be numeric")
    }
    
    
    # Only communicate the expiry dates if there are some files present
    if (missing_file_number<3){
        .mm(cli::rule(), "comm")
        .mm(paste0("Reminders for outdated database files will occur ",
                   "once files are older than XX days:"), "comm")
        
        #.mm(paste0("Using the following expiry dates for databases \n",
        #    "(max. number of days since file creation date):"), "comm")
        
        .mm(paste0("SRA: ", sra_expiry, " days"), "qn")
        .mm(paste0("GEO: ", geo_expiry, " days"), "qn")
        .mm(paste0("SRR_GSM: ", srr_gsm_expiry, " days"), "qn")
    }
    
    
    
    
    expiry_parameters <- list(sra_expiry=sra_expiry,
                              geo_expiry=geo_expiry,
                              srr_gsm_expiry=srr_gsm_expiry)
    
    
    return(expiry_parameters)
    
    
}







#' 
#' Check and ensure that the DB file is in place
#' 
#' @param path Path for storing database files (as passed to startSpiderSeqR())
#' @param file_paths Paths to the database files (all three)
#' @param file_ind File index (i.e. which file is of interest)
#' @param db_expiry Maximum number of days since file was modified
#' 
#' @return An updated file_paths vector. 
#'     If the file doesn't exist or is out of date, offer to 
#'     download/create it. Otherwise, print information about the file.
#' 
.checkDBFile <- function(path, file_paths, file_ind, db_expiry){
    file_ind <- match.arg(file_ind, 1:3)
    
    print("1")
    # NO FILE
    if(!file.exists(db_file)){ # NO FILE
    #    .noDBFile(path, db_file_name)
    }
    
    print("2")
    # OLD FILE
    if(file.exists(db_file) & 
       (difftime(Sys.Date(), 
                 file.info(db_file)$mtime, units = "days") > db_expiry) ){
        print("2a")
    #    .oldDBFile(path, db_file_name, db_file)
        
    } else if(file.exists(db_file)) {
        # FILE PRESENT AND UP TO DATE
        print("2b")
    #    .DBFileExists(db_file_name, db_file)
    }
    
    return(file_paths)
}







#' -------------------------------------------------------
#' ===*====


#' Suggested names:
#' createMockSRA
#' createMOckGEO
#' createMockCustomDB
#' 
#' Course of action
#' - fetch the tables from the environment
#' - set up the database connection into the relevant file (?path)
#' - write the tables into the database
#' - close the connection
#' - make sure that the directory is changed back to original
#' 
#' 
#' Options:
#' - everything in one function per DB
#' - split into db specific part and writing dfs into the database
#' 
#' 
#' .createDBFile
#' - df
#' - name
#' - database file name
#' - path to database file name
#' 


#' - path to file
#' - (file known)
#' - (tables known)
#' 



#' Create a mock SRA database file (for use in testing)
#' 
#' @param path A character with the path to the directory for the database file
#' @return Nothing. Creates the database file (unless already present) 
#'     and writes relevant tables.
#'     
#' @keywords internal
#' 
.createMockSRA <- function(path){
    .writeTableToFile(df=sra_demo, table_name="sra", path=path, 
                      database_file="SRAmetadb.sqlite")
    .writeTableToFile(df=sra_metadata, table_name="metaInfo", path=path, 
                      database_file="SRAmetadb.sqlite")
}



#' Create a mock GEO database file (for use in testing)
#' 
#' @param path A character with the path to the directory for the database file
#' @return Nothing. Creates the database file (unless already present) 
#'     and writes relevant tables.
#'     
#' @keywords internal
#' 
.createMockGEO <- function(path){
    .writeTableToFile(df=gsm_demo, table_name="gsm", path=path, 
                      database_file="GEOmetadb.sqlite")
    .writeTableToFile(df=gse_demo, table_name="gse", path=path, 
                      database_file="GEOmetadb.sqlite")
    .writeTableToFile(df=geo_metadata, table_name="metaInfo", path=path, 
                      database_file="GEOmetadb.sqlite")
}




#' Create a mock custom database file (for use in testing)
#' 
#' @param path A character with the path to the directory for the database file
#' @return Nothing. Creates the database file (unless already present) 
#'     and writes relevant tables.
#'     
#' @keywords internal
#' 
.createMockCustomDB <- function(path){
    .writeTableToFile(df=srr_demo, table_name="srr_gsm", path=path, 
                      database_file="SRR_GSM.sqlite")
    .writeTableToFile(df=srr_gsm_metadata, table_name="metaInfo", path=path, 
                      database_file="SRR_GSM.sqlite")
}




#' Write table to database file
#' @param df Data frame to be written as a table in the database
#' @param table_name A character with the name for the table
#' @param database_file A character with the name of the database file 
#'     (without the path)
#' @param overwrite A logical indicating whether to overwrite an existing 
#'     table with the same name. Defaults to FALSE.
#'     
#' @return Nothing. Write the dataframe into the specified database
#' 
#' @keywords internal
#' 
.writeTableToFile <- function(df, table_name, path, database_file, 
                              overwrite=FALSE){
    
    path <- normalizePath(path) # Needed as used for test setup
    database_file <- file.path(path, database_file)
    
    #print(database_file)
    if(getSpiderSeqROption("testing")){
        if (file.exists(database_file)) return()
    }
    
    conn <- DBI::dbConnect(RSQLite::SQLite(), 
                           database_file, overwrite=overwrite)
    on.exit(DBI::dbDisconnect(conn), add=TRUE)
    #on.exit(print("done done"), add=TRUE)
    DBI::dbWriteTable(conn=conn, name=table_name, value = df)
}




