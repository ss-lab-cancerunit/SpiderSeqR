#' Functions:
#'  - .findDBFiles
#' - .missingFileCheck
#' - .setExpiryParameters
#' - .DBNames
#' - .checkDBNames
#' - .checkDBFile
#' - .noDBFile
#' - .oldDBFile
#' - .DBFileExists
#' - .getDBFile
#' - .createCustomDBFile
#' - .getFurtherDBInfo


#' Find database files
#' 
#' @param path Path to search within
#' @return A list with paths to database files
#' 
#' @keywords internal
#' 
.findDBFiles <- function(path){
    
    #===*===
    ori_wd <- getwd()
    setwd(path)
    on.exit(setwd(ori_wd))
    
    sra_file <- "SRAmetadb.sqlite"
    geo_file <- "GEOmetadb.sqlite"
    srr_gsm_file <- "SRR_GSM.sqlite"
    
    sra_file_name <- sra_file
    geo_file_name <- geo_file
    srr_gsm_file_name <- srr_gsm_file
    
    #==========================================================
    # Checking files and searching within the directory####
    #==========================================================
    # NOTE: will be re-checked again later, this is for expanding search
    # and communicating to the user
    
    .mm(paste0("Searching for database files within: "), "comm")
    .mm(getwd(), "qn")
    
    
    var_list <- c("sra_file", "geo_file", "srr_gsm_file")
    
    for (i in seq_along(var_list)){
        
        match_files <- .findFiles(paste0("(^|*)", get(var_list[i])), 
                                  recurse_levels=0)
        
        # If length = 0, do nothing
        if (length(match_files) == 1){
            .mm(paste0("Found ", get(paste0(var_list[i], "_name")), 
                       " file:\n", match_files), "comm")
            assign(var_list[i], match_files) # Substitute the path
        } else if (length(match_files > 1)){
            
            .mm(paste0("Found multiple matching files. ",
                       "Which one would you like to use?"), "qn")
            
            # Let the user choose the file
            file_choice <- utils::menu(match_files)
            assign(var_list[i], match_files[file_choice])
        }
        
    }
    
    file_list <- list(sra_file=sra_file, 
                        geo_file=geo_file, 
                        srr_gsm_file=srr_gsm_file)
    
    
    return(file_list)

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
.missingFileCheck <- function(sra_file=sra_file, 
                                geo_file=geo_file,
                                srr_gsm_file=srr_gsm_file){
    # Repeat missing check after extended search done
    missing_logical <- c(!file.exists(sra_file), 
                         !file.exists(geo_file), 
                         !file.exists(srr_gsm_file))
    
    missing_files <- c(sra_file, geo_file, srr_gsm_file)
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
        .mm(paste0("NOTE: The total download size of all three files ",
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



.DBNames <- function(){
    return(c("SRAmetadb.sqlite",
             "GEOmetadb.sqlite",
             "SRR_GSM.sqlite"))
}



.checkDBNames <- function(db_file_name){
    db_file_name <- match.arg(db_file_name, .DBNames())
    return(db_file_name)
}



#' 
#' Check and ensure that the DB file is in place
#' 
#' @param path Path for storing database files (as passed to startSpiderSeqR())
#' @param db_file Path to the database file
#' @param db_file_name File name (without the path)
#' @param db_expiry Maximum number of days since file was modified
#' 
#' @return Nothing. If the file doesn't exist or is out of date, offer to 
#'     download/create it. Otherwise, print information about the file.
#' 
.checkDBFile <- function(path, db_file, db_file_name, db_expiry){
    
    print("1")
    # NO FILE
    if(!file.exists(db_file)){ # NO FILE
        .noDBFile(path, db_file_name)
    }
    
    print("2")
    # OLD FILE
    if(file.exists(db_file) & 
       (difftime(Sys.Date(), 
                 file.info(db_file)$mtime, units = "days") > db_expiry) ){
        print("2a")
        .oldDBFile(path, db_file_name, db_file)
        
    } else if(file.exists(db_file)) {
        # FILE PRESENT AND UP TO DATE
        print("2b")
        .DBFileExists(db_file_name, db_file)
    }
    
}



#' Course of action to follow if a db file is missing
#' 
#' @param path Path for storing database files
#' @param db_file_name A character with the db_file_name (SRAmetadb.sqlite, 
#'     GEOmetadb.sqlite or SRR_GSM.sqlite)
#' @return Nothing. Offer to download/create the file, otherwise return error.
#' 
#' @keywords internal
#' 
.noDBFile <- function(path, db_file_name){
    
    db_file_name <- .checkDBNames(db_file_name)
    
    .mm(paste0("The file ", 
               db_file_name, 
               " was not found in the specified directories"), 
        "qn")
    
    
    if (db_file_name %in% .DBNames()[1:2]){
        .mm("Would you like to download the file now?", "qn")
    } else {
        .mm(paste0("Would you like to create a cutstom database for ",
            "converting between GEO and SRA? "), "qn")
        .mm(paste0("This might take a little while, but it is ",
            "necessary for the correct functioning \nof the package."), "comm")
    }
    
    
    file_menu <- .tmenu(c("yes", "no"), menu_name = "download_file")
    if (file_menu == 1){
        .mm("Downloading the file", "comm")
        
        .getDBFile(path=path, db_file_name=db_file_name)
        
        
    } else {
        stop(paste0(db_file_name, 
                    " file is necessary for the functioning of the package"))
    }
    
}



.oldDBFile <- function(path, db_file_name, db_file){
    
    db_file_name <- .checkDBNames(db_file_name)
    
    .mm(paste0("The file ", db_file_name, " is out of date"), "qn")
    .mm(paste0("Last modified: ", file.info(db_file)$mtime), "comm")
    .mm(paste0("Would you like to download a new version of the file ",
               "right now?\n(this is recommended, though not necessary)"), "qn")
    
    db_menu <- .tmenu(c("yes", "no"), menu_name = "download_file")
    if (db_menu == 1){
        .mm("Downloading the file", "comm")
        .getDBFile(path=path, db_file_name=db_file_name)
    } else {
        if (db_file_name %in% .DBNames()[1:2]){
            .mm(paste0(
                "Next time please consider downloading a new version of ", 
                db_file_name, " file"), "adverse")
        } else {
            .mm(paste0("Next time please consider re-creating the ", 
                       srr_gsm_file, " file"), "adverse")
        }

    }
}




.DBFileExists <- function(db_file_name, db_file){
    
    db_file_name <- .checkDBNames(db_file_name)
    
    .mm(paste0("The file ", db_file_name, " is up to date"), "comm")
    .mm(paste0("Last modified: ", file.info(db_file)$mtime), "comm")
}




.getDBFile <- function(path, db_file_name){
    
    db_file_name <- .checkDBNames(db_file_name)
    ori_path <- getwd()
    setwd(path)
    on.exit(setwd(ori_path))
    
    if (isTRUE(getSpiderSeqROption("testing"))){
        
        # MOCK FILES (for testing)
        
        if (db_file_name == .DBNames()[1]){
            .createMockSRA(".")
        }
        
        if (db_file_name == .DBNames()[2]){
            .createMockGEO(".")
        }
        
        if (db_file_name == .DBNames()[3]){
            .createMockCustomDB(".")
        }
        
    } else {
        
        # REAL
        
        if (db_file_name == .DBNames()[1]){
            sra_file <- SRAdb::getSRAdbFile()
        }
        
        if (db_file_name == .DBNames()[2]){
            geo_gz_file <- GEOmetadb::getSQLiteFile(destfile = 
                                                        "GEOmetadb.sqlite.gz")
        }
        
        if (db_file_name == .DBNames()[3]){
            stop("Not working yet")
            .createCustomDBFile(sra_file, geo_file)
        }
        
    }
    
    print(getwd())
    print(db_file_name)
    
    db_file <- list.files(path=getwd(), 
                            pattern=paste0("^", db_file_name, "$"), 
                            full.names = TRUE)
    
    #print(db_file)
    #===*=== Display message here:
    .mm(paste0("New file created: ", db_file), "comm")
    
    return(db_file)
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
    
    ori_path <- getwd()
    setwd(path)
    on.exit(setwd(ori_path), add=TRUE)
    conn <- DBI::dbConnect(RSQLite::SQLite(), 
                            database_file, overwrite=overwrite)
    on.exit(DBI::dbDisconnect(conn), add=TRUE)
    on.exit(print("done done"), add=TRUE)
    DBI::dbWriteTable(conn=conn, name=table_name, value = df)
}





#' ===*===
#' -------------------------------------------------------


#' Create custom database file
#' 
#' @param sra_file A character with the path to SRA database file
#' @param geo_file A character with the path to GEO database file
#' @return Nothing. Create a file with a conversion database 
#'     between SRA and GEO
#' 
#' @keywords  internal
.createCustomDBFile <- function(sra_file, geo_file){
    
    
    .mm("Please wait, creating the custom database...", "comm")
    
    
    db_df <- .createSRR_GSM(sra_file, geo_file)
    
    
    metainfo <- .createSpiderMetaInfo(sra_file, geo_file)
    
    #Save df as an sqlite object
    srr_gsm <- DBI::dbConnect(RSQLite::SQLite(), dbname = "SRR_GSM.sqlite")
    
    DBI::dbWriteTable(conn = srr_gsm, 
                      name = "srr_gsm", value = db_df, overwrite = TRUE)
    DBI::dbWriteTable(conn = srr_gsm, 
                      name = "metaInfo", value = metainfo, overwrite = TRUE)
    
    .vex("db_df", db_df)
    
    DBI::dbDisconnect(srr_gsm)
    
    #print(Sys.time())
    
    
}



.createSRR_GSM <- function(sra_file, geo_file){
    #==========================================================
    #SRR_GSM
    #==========================================================
    # Creating a new database for SRA runs:
    
    #IDEA
    # find entries which contain GSM in run alias
    #                   or in experiment_attribute (GEO Accession: GSM)
    
    #BY CHUNK
    #Select: SRR, SRX, SRS, SRP, run_alias, experiment_attribute
    #create two new columns: run_gsm, exp_gsm
    #do grepl(gsm) on run_alias and experiment_attribute
    #for grepled rows, extract run_gsm and exp_gsm respectively
    #check two columns are identical - if not, have a false in a new column
    
    #MERGE CHUNKS
    #WRITE AS AN SQLITE FILE (ESTABLISH THE CONNECTION?)
    print(sra_file)
    print(geo_file)
    
    sra_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = sra_file)
    geo_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = geo_file)
    
    db_df <- data.frame()
    
    #print(Sys.time())
    #tt <- 1
    
    
    #Not searching for the total number of entries; it adds a huge overhead
    #tot_query <- paste0("SELECT count(*) FROM sra WHERE run_alias ",
    #               "LIKE 'GSM%' OR experiment_attribute LIKE '%GSM%'")
    #tot_n <- DBI::dbGetQuery(sra_con, tot_query)
    #tot_n <- as.integer(tot_n)
    
    # Last found total (1618978) + ~100 000
    i <- 80000
    tot_n <- 1700000
    .progressBar(i, tot_n)
    
    rs <- DBI::dbSendQuery(sra_con, "SELECT
                        run_accession,
                        experiment_accession,
                        sample_accession,
                        study_accession,
                        run_alias, --For GSM
                        experiment_attribute --For GSM
                        FROM sra WHERE run_alias 
                        LIKE 'GSM%' OR experiment_attribute LIKE '%GSM%'")
    i <- i + 20000
    .progressBar(i, tot_n)
    while (!DBI::dbHasCompleted(rs)){
        #cat(".")
        #if (tt %% 80 ==0) cat("\n")
        #tt <- tt + 1
        .progressBar(i, tot_n)
        
        chunk <- DBI::dbFetch(rs, 1000)
        
        #Create intermediate columns for extracting GSM information
        chunk$run_gsm <- NA #from run_alias
        chunk$exp_gsm <- NA #from experiment_attribute
        
        #Find indices where GSM is present
        run_gsm_indices <- grepl("GSM\\d\\d\\d+", chunk$run_alias)
        #exp_gsm_indices <- grepl("GSM\\d\\d\\d+", 
        # chunk$experiment_attribute)
        exp_gsm_indices <- grepl("GEO Accession: GSM\\d\\d\\d+", 
                                 chunk$experiment_attribute, ignore.case = TRUE)
        
        #Extract GSM information
        chunk$run_gsm[run_gsm_indices] <- 
            gsub(".*?(GSM\\d\\d\\d+).*", "\\1", 
                 chunk$run_alias[run_gsm_indices])
        
        chunk$exp_gsm[exp_gsm_indices] <- 
            gsub(".*?GEO Accession: (GSM\\d\\d\\d+).*", "\\1", 
                 chunk$experiment_attribute[exp_gsm_indices], 
                 ignore.case = TRUE)
        
        #Create a column to indicate whether GSMs agree between two columns
        chunk$gsm_check <- NA
        
        #Fill in check column
        #Get indices where run_alias and experiment_attribute 
        # are both present
        both_indices <- !(is.na(chunk$run_gsm) | is.na(chunk$exp_gsm)) 
        
        #Check those indices for equality
        chunk$gsm_check[both_indices] <- 
            chunk$run_gsm[both_indices] == chunk$exp_gsm[both_indices] 
        
        #GSM_CHECK: NA - either one or both missing
        #           F - run_gsm and exp_gsm NOT the same
        #           T - run_gsm and exp_gsm the same
        
        #Create a new column for storing GSMs
        chunk$gsm <- NA
        
        #Extract information from exp_gsm and run_gsm columns
        #Non-NA entries from experiment_attribute
        chunk$gsm[!is.na(chunk$exp_gsm)] <- 
            chunk$exp_gsm[!is.na(chunk$exp_gsm)] 
        
        #Non-NA entries from run_alias
        chunk$gsm[!is.na(chunk$run_gsm)] <- 
            chunk$run_gsm[!is.na(chunk$run_gsm)] 
        
        #NOTE: if both exp_gsm and run_gsm are present, 
        #  the GSM obtained from run_alias will be retained
        
        #Chunk columns at present: "run_accession", "experiment_accession",
        #"sample_accession", "study_accession", "run_alias", 
        #"experiment_attribute", "run_gsm", "exp_gsm", "gsm_check"
        
        #Select columns
        chunk <- chunk[,c("run_accession", 
                          "experiment_accession", 
                          "sample_accession", 
                          "study_accession", 
                          "gsm", 
                          "gsm_check")]
        
        #Get the number of entries with GSM content
        .mm(sum(run_gsm_indices | exp_gsm_indices), "dev")
        
        
        db_df <- rbind(db_df, chunk)
        
        i <- i+1000
        
    }
    
    .progressBar(tot_n, tot_n)
    
    cat("\n")
    
    #print(Sys.time())
    
    DBI::dbClearResult(rs)
    
    
    #Remove duplicates
    db_df <- unique(db_df)
    
    #Remove entries without successfully extracted GSMs
    db_df <- db_df[!is.na(db_df$gsm),]
    
    #Order (will not be used - this will keep the same order as in the db)
    #order_columns <- list(db_df$study_accession,
    #                      db_df$sample_accession,
    #                      db_df$experiment_accession,
    #                      db_df$run_accession,
    #                      db_df$gsm)
    #db_df <- db_df[orderAccessions(order_columns),]
    
    DBI::dbDisconnect(sra_con)
    DBI::dbDisconnect(geo_con)
    
    return(db_df)
}



#' Create metaInfo table for SpiderSeqR database
#' @param sra_file A character to the path with SRA file
#' @param geo_file A character to the path with GEO file
#' 
#' @return A dataframe with metaInfo
#' 
#' @keywords internal
.createSpiderMetaInfo <- function(sra_file, geo_file){
    
    sra_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = sra_file)
    geo_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = geo_file)
    
    sra_metainfo <- DBI::dbGetQuery(sra_con, "SELECT * FROM metaInfo")
    geo_metainfo <- DBI::dbGetQuery(geo_con, "SELECT * FROM metaInfo")
    
    sra_metainfo$name <- paste("SRA", sra_metainfo$name)
    geo_metainfo$name <- paste("GEO", geo_metainfo$name)
    
    
    metainfo <- rbind(sra_metainfo, geo_metainfo)
    metainfo <- rbind(metainfo, c("SpiderSeqR schema version", "1.0"))
    metainfo <- rbind(metainfo, c("SpiderSeqR creation timestamp", 
                                  format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    
    DBI::dbDisconnect(sra_con)
    DBI::dbDisconnect(geo_con)
    
    
    return(metainfo)
    
    
}





#' Get futher database information (metaInfo)
#' 
#' @param db_file_name A character with the name of the database file
#' @param database_name A character with the database (connection) name
#' @return Nothing. Display messages with information in the console
#' 
#' @keywords internal
#' 
.getFurtherDBInfo <- function(db_file_name, database_name){
    .mm(paste0("Further information on ", db_file_name, " database:"), "comm")
    #.mm(cli::rule(), "comm")
    
    df <- DBI::dbGetQuery(get(database_name, envir = get(".GlobalEnv")), 
                            "SELECT * FROM metaInfo")
    print(df)
    #.mm(df, "comm")
}


