
# - startSpiderSeqR
# - findFiles



#' Prepare the environment to run SpiderSeqR
#' 
#' \code{startSpiderSeqR} prepares the environment so that 
#' other SpiderSeqR functions can be used. 
#' Run \code{startSpiderSeqR} every time you begin with a clear environment 
#' and want to use any of the other SpiderSeqR functions.
#' In particular, the function does the following:
#' \itemize{
#'     \item Ensure that SRAmetadb.sqlite is downloaded and up to date
#'     \item Ensure that GEOmetadb.sqlite is downloaded and up to date
#'     \item Ensure that SRR_GSM.sqlite is created and up to date
#'     \item Set up database connections to the above files 
#'         in the \code{.GlobalEnv}
#' } 
#' 
#' 
#' Depending on the contents of the specified directory, 
#' \code{startSpiderSeqR} may download/create the database files. 
#' It will always create the database connections in the global environment.
#' 
#' It is necessary to fulfil all the above requirements; 
#' without them the package will not work.
#' The first two database files are relatively large in size 
#' (32 GB and 9 GB at the time of writing), so please ensure that you have 
#' adequate internet connection and sufficient disk space.
#' 
#' It is recommended that the newest version of the databases is used. 
#' However, it is possible to ignore this requirement by manually setting 
#' the expiry date of the files (or by running default settings 
#' of the function and selecting the option not to download the newer files).
#' 
#' 
#' 
#' @section When to run \code{startSpiderSeqR}?:
#' 
#' Run \code{startSpiderSeqR} every time you start with a fresh environment. 
#' There is no harm in running it too many times.
#' 
#' 
#' 
#' @section Which options to choose from \code{startSpiderSeqR} menu options?:
#' 
#' Should you have any missing or outdated files in the specified directory, 
#' \code{startSpiderSeqR} will offer to download/create the files. 
#' 
#' Please note that it is \emph{required} to have all the three database files;
#' if you choose not to download/create them, it will not be possible 
#' to run other SpiderSeqR functions. 
#' 
#' However, it is \emph{not required} for all the files to be up-to-date; 
#' \code{startSpiderSeqR} will suggest re-downloading/re-creating the files, 
#' but you can still use SpiderSeqR, even if you do not agree 
#' to re-download/re-create the files.
#' 
#' 
#' 
#' @section Time and space requirements:
#' 
#' The following files must be present in order to run other SpiderSeqR 
#' functions; \code{startSpiderSeqR} will download/create them if necessary:
#' \itemize{
#'   \item SRAmetadb.sqlite (from SRAdb package)
#'   \item GEOmetadb.sqlite (from GEOmetadb package)
#'   \item SRR_GSM.sqlite (custom-made at the time of running 
#'       \code{startSpiderSeqR})
#' }
#' 
#' 
#' They take approximately 40 GB of disk space 
#' (32 GB, 9 GB and 100 MB respectively at the time of writing), 
#' so please ensure that you have adequate internet connection 
#' and sufficient disk space. 
#' The two downloaded files are downloaded compressed, 
#' so the download size is smaller than the final file size. 
#' In order to save disk space, the previous files will be overwritten 
#' when downloading a newer version, so if you would like to keep them, 
#' please rename them before updating.
#' 
#' 
#' Running \code{startSpiderSeqR} for the first time will inevitably 
#' take some time, because large database files need to be downloaded 
#' and custom database created. However, once all the files are present, 
#' the function should take an order of \emph{seconds} to complete 
#' (it is a matter of setting up database connections).
#' 
#' 
#' 
#' @examples 
#' 
#' ## Database files are stored (or will be downloaded) 
#' ##    in the working directory
#' # startSpiderSeqR(dir = getwd()) 
#' 
#' ## Use the following if you would like to download 
#' ##   the newest database files
#' # startSpiderSeqR(dir = getwd(), general_expiry = 0) 
#' 
#' ## Use the following if you have old database files
#' ##   that you do not wish to re-download on this occasion
#' # startSpiderSeqR(dir = getwd(), general_expiry = 365) 
#' 
#' ## Use the following if you only wish to ignore 
#' ##    an old SRAmetadb.sqlite file, 
#' ##    but get reminders to re-download the other files
#' # startSpiderSeqR(dir = getwd(), sra_expiry = 365) 
#' 
#' ## Use the following if you would like to locate
#' ##   the database files in a few directory levels above
#' # startSpiderSeqR(dir = getwd(), recurse_levels = 4)
#' 
#' 
#' @param dir Directory where database files will be stored
#' @param general_expiry Maximum number of days since creation 
#'     of all database files
#' @param sra_expiry Maximum number of days since creation 
#'     of SRAmetadb.sqlite file
#' @param geo_expiry Maximum number of days since creation 
#'     of GEOmetadb.sqlite file
#' @param srr_gsm_expiry Maximum number of days since creation 
#'     of SRR_GSM.sqlite file
#' 
#' @return Nothing. If necessary, it may download/create database files. 
#' Sets up database connections in the global environment.
#' 
#' @family Setup functions
#' 
#' 
#'
#'
#' @export
#' 
startSpiderSeqR <- function(dir, 
                            general_expiry=90, 
                            sra_expiry, 
                            geo_expiry, 
                            srr_gsm_expiry){
    
    ori_wd <- getwd()
    setwd(dir)
    
    
    .mm(cli::rule(), "comm")
    .mm("Welcome to SpiderSeqR!!!", "qn")
    .mm(paste0("Please wait while the database files and connections ",
               "are being configured..."), "comm")
    .mm(cli::rule(), "comm")
    
    
    
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
    
    
    file_list <- c("sra_file", "geo_file", "srr_gsm_file")
    
    for (i in seq_along(file_list)){
        
        match_files <- .findFiles(paste0("(^|*)", get(file_list[i])), 
                                  recurse_levels=0)
        
        # If length = 0, do nothing
        if (length(match_files) == 1){
            .mm(paste0("Found ", get(paste0(file_list[i], "_name")), 
                       " file:\n", match_files), "comm")
            assign(file_list[i], match_files) # Substitute the path
        } else if (length(match_files > 1)){
            
            .mm(paste0("Found multiple matching files. ",
                       "Which one would you like to use?"), "qn")
            
            # Let the user choose the file
            file_choice <- utils::menu(match_files)
            assign(file_list[i], match_files[file_choice])
        }
        
    }
    
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
                   "is about 10-15 GB (compressed)\n", 
                   "requiring about 40-50 GB disc space after extraction \n",
                   "(these numbers may change as the databases ",
                   "keep growing)"), "comm")
        #.mm(cli::rule(), "comm")
        
    }
    
    
    
    #==========================================================
    
    
    
    #Setup:
    # - SRAmetadb
    # - GEOmetadb
    # - SRR_GSM (custom database for converting between the ids 
    #     of SRA and GSMs (GEO))
    #print("Setting up SpiderSeqR")
    
    # Logic:
    # - SRAmetadb:
    #    * if does not exist - download (else: stop)
    #    * if exists and not up to date - offer to re-download (else: message)
    # - GEOmetadb:
    #    * if does not exist - download (else: stop)
    #    * if exists and not up to date - offer to re-download (else: message)
    # - SRR_GSM:
    #    * if file exists
    #        - if in date - do nth
    #        - if not up to date - offer to re-create (else: message)
    #    * if does not exist - create (else: stop)
    
    
    
    
    #==========================================================
    # Setting expiry parameters ####
    #==========================================================
    # Logic:
    # Use specific parameters (sra, geo, srr_gsm) if available. 
    # If not, use the expiry date from general_expiry
    
    if ((!missing(general_expiry))&
        (!missing(sra_expiry))&
        (!missing(geo_expiry))&
        (!missing(srr_gsm_expiry))){
        warning(paste0("general_expiry argument will be ignored, ",
                       "since all the individual expiry dates have been provided"))
    }
    
    if (missing(sra_expiry)){
        sra_expiry <- general_expiry
    }
    if (missing(geo_expiry)){
        geo_expiry <- general_expiry
    }
    if (missing(srr_gsm_expiry)){
        srr_gsm_expiry <- general_expiry
    }
    
    if ( !(is.numeric(general_expiry)) | 
         !(is.numeric(sra_expiry)) | 
         !(is.numeric(geo_expiry)) | 
         !(is.numeric(srr_gsm_expiry)) ){
        stop("Expiry parameters must be numeric")
    }
    
    #==========================================================
    
    # Only communicate the expiry dates if there are some files present
    if (sum(missing_logical)<3){
        .mm(cli::rule(), "comm")
        .mm(paste0("Reminders for outdated database files will occur ",
                   "once files are older than XX days:"), "comm")
        
        #.mm(paste0("Using the following expiry dates for databases \n",
        #    "(max. number of days since file creation date):"), "comm")
        
        .mm(paste0("SRA: ", sra_expiry, " days"), "qn")
        .mm(paste0("GEO: ", geo_expiry, " days"), "qn")
        .mm(paste0("SRR_GSM: ", srr_gsm_expiry, " days"), "qn")
    }
    
    
    
    .mm(cli::rule(), "comm")
    
    
    #==========================================================
    
    
    
    #==========================================================
    # SRAmetadb ####
    #==========================================================
    
    # NO SRA FILE
    if(!file.exists(sra_file)){ # NO FILE
        
        .mm(paste0("The file ", 
                   sra_file_name, 
                   " was not found in the specified directories"), 
            "qn")
        
        .mm("Would you like to download the file now?", "qn")
        
        sra_menu <- utils::menu(c("yes", "no"))
        if (sra_menu == 1){
            .mm("Downloading the file", "comm")
            sra_file <- SRAdb::getSRAdbFile()
        } else {
            stop(paste0(sra_file_name, 
                        " file is necessary for the functioning of the package"))
        }
    }
    
    
    # OLD SRA FILE
    if(file.exists(sra_file) & 
       (difftime(Sys.Date(), 
                 file.info(sra_file)$mtime, units = "days") > sra_expiry) ){
        
        .mm(paste0("The file ", sra_file_name, " is out of date"), "qn")
        .mm(paste0("Last modified: ", file.info(sra_file)$mtime), "comm")
        .mm(paste0("Would you like to download a new version of the file ",
                   "right now?\n(this is recommended, though not necessary)"), "qn")
        
        sra_menu <- utils::menu(c("yes", "no"))
        if (sra_menu == 1){
            .mm("Downloading the file", "comm")
            sra_file <- SRAdb::getSRAdbFile()
        } else {
            .mm(paste0(
                "Next time please consider downloading a new version of ", 
                sra_file_name, " file"), "adverse")
        }
        
    } else if(file.exists(sra_file)) {
        .mm(paste0("The file ", sra_file_name, " is up to date"), "comm")
        .mm(paste0("Last modified: ", file.info(sra_file)$mtime), "comm")
    }
    
    
    #==========================================================
    
    
    
    .mm(cli::rule(), "comm")
    
    
    #==========================================================
    # GEOmetadb ####
    #==========================================================
    
    # NO GEO FILE
    if(!file.exists(geo_file)){ # NO FILE
        
        
        .mm(paste0("The file ", 
                   geo_file_name, 
                   " was not found in the specified directories"), 
            "qn")
        
        .mm("Would you like to download the file right now?", "qn")
        
        geo_menu <- utils::menu(c("yes", "no"))
        if (geo_menu == 1){
            .mm("Downloading the file", "comm")
            geo_gz_file <- GEOmetadb::getSQLiteFile(destfile = 
                                                        "GEOmetadb.sqlite.gz")
        } else {
            stop(paste0(geo_file_name, 
                        " file is necessary for the functioning of the package"))
        }
    }
    
    
    # OLD GEO FILE
    if(file.exists(geo_file) & 
       (difftime(Sys.Date(), 
                 file.info(geo_file)$mtime, units = "days") > geo_expiry) ){
        
        .mm(paste0("The file ", geo_file_name, "is out of date"), "qn")
        .mm(paste0("Last modified: ", file.info(geo_file)$mtime), "comm")
        .mm(paste0("Would you like to download a new version of the file ",
                   "right now?\n(this is recommended, though not necessary)"), "qn")
        
        geo_menu <- utils::menu(c("yes", "no"))
        if (geo_menu == 1){
            .mm("Downloading the file", "comm")
            geo_gz_file <- 
                GEOmetadb::getSQLiteFile(destfile = "GEOmetadb.sqlite.gz")
        } else {
            .mm(paste0(
                "Next time please consider downloading a new version of ", 
                geo_file_name, " file"), "adverse")
        }
        
    } else if(file.exists(geo_file)) {
        .mm(paste0("The file ", geo_file_name, " is up to date"), "comm")
        .mm(paste0("Last modified: ", file.info(geo_file)$mtime), "comm")
    }
    
    #==========================================================
    
    
    .mm(cli::rule(), "comm")
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    #==========================================================
    # RE-CHECK THE PRESENCE OF FILES (have they downloaded successfully???)
    #==========================================================
    if (!(file.exists(sra_file) & file.exists(geo_file))){
        warning(paste0("Something went wrong. Some database files might ",
                       "be missing. Consider loading the package again."))
    } else {
        .mm(paste0(
            "Both db files are present (remember not to remove them!)\n",
            "Proceeding to the final step ",
            "(checking for the custom database)."), 
            "comm")
    }
    #==========================================================
    
    
    .mm(cli::rule(), "comm")
    
    #==========================================================
    # FIND OUT WHETHER IT IS NECESSARY TO CREATE CUSTOM DATABASE ####
    #==========================================================
    
    if (!file.exists(srr_gsm_file)){ # NO FILE
        
        .mm(paste0("Would you like to create a cutstom database for converting",
                   " between GEO and SRA? "), "qn")
        .mm(paste0("This might take a little while, but it is ",
                   "necessary for the correct functioning \nof the package."), "comm")
        srr_gsm_menu <- utils::menu(c("yes", "no"))
        if (srr_gsm_menu == 1){ # WILLING TO CREATE DB
            .mm("Database will be created shortly", "comm")
            db_needed <- TRUE
        } else { # DECLINED CREATION OF DB
            db_needed <- FALSE
            stop(
                "The database is necessary for the functioning of the package")
        }
        
    } else { # FILE PRESENT
        if (difftime(Sys.Date(), 
                     file.info(srr_gsm_file)$mtime, 
                     units = "days") < srr_gsm_expiry){
            db_needed <- FALSE
            .mm(paste0("The custom database for converting between ",
                       "SRA and GEO is up to date"), "comm")
            .mm(paste0("Last modified: ", 
                       file.info(srr_gsm_file)$mtime), 
                "comm")
            
        } else { # OLD FILE
            
            .mm(paste0("The file\n", 
                       srr_gsm_file, "\nis out of date"), "qn")
            
            .mm(paste0("Last modified: ", 
                       file.info(srr_gsm_file)$mtime), "comm")
            
            .mm(paste0("Would you like to create a new version of the ",
                       "file right now?\n(this is recommended, though not necessary)"), 
                "qn")
            
            srr_gsm_menu <- utils::menu(c("yes", "no"))
            if (srr_gsm_menu == 1){ # WILLING TO CREATE DB
                .mm("Database will be re-created shortly", "comm")
                db_needed <- TRUE
            } else { # DECLINED CREATION OF DB
                db_needed <- FALSE
                .mm(paste0("Next time please consider re-creating the ", 
                           srr_gsm_file, " file"), "adverse")
            }
        }
        
    }
    #==========================================================
    
    
    
    
    #==========================================================
    # Create custom database ####
    #==========================================================
    if (db_needed){
        .mm("Please wait, creating the custom database...", "comm")
        
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
        
        #Save df as an slite object
        srr_gsm <- DBI::dbConnect(RSQLite::SQLite(), dbname = "SRR_GSM.sqlite")
        
        DBI::dbWriteTable(conn = srr_gsm, 
                          name = "srr_gsm", value = db_df, overwrite = TRUE)
        
        .vex("db_df", db_df)
        
        DBI::dbDisconnect(sra_con)
        DBI::dbDisconnect(geo_con)
        
        #print(Sys.time())
        
        
    }
    #==========================================================
    
    
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    
    if (!isTRUE(getSpiderSeqROption("testing"))){
        .GlobalEnv$sra_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = sra_file)
        .GlobalEnv$geo_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = geo_file)
        .GlobalEnv$srr_gsm <- 
            DBI::dbConnect(RSQLite::SQLite(), dbname = srr_gsm_file)
    } else {
        setwd(ori_wd)
        return(TRUE)
    }
    
    setwd(ori_wd)
    
    .mm(cli::rule(), "comm")
    .mm(paste0("Further information on ", sra_file_name, " database:"), "comm")
    #.mm(cli::rule(), "comm")
    .mm(DBI::dbGetQuery(get("geo_con", 
                            envir = get(".GlobalEnv")), 
                        "SELECT * FROM metaInfo"), "comm")
    
    .mm(cli::rule(), "comm")
    .mm(paste0("Further information on ", geo_file_name, " database:"), "comm")
    #.mm(cli::rule(), "comm")
    .mm(DBI::dbGetQuery(get("sra_con", 
                            envir = get(".GlobalEnv")), 
                        "SELECT * FROM metaInfo"), "comm")
    .mm(cli::rule(), "comm")
    .mm("SpiderSeqR setup complete", "qn")
    
    
    
    
}





#' Find files (wrapper around dir allowing to search a few levels up)
#' 
#' @param pattern Regular expression pattern to search for (passed to dir())
#' @param recurse_levels Integer with the number of directory levels 
#'     (above current working directory) to search in
#' @return A full path with the matching file(s)
#' 
#' @examples 
#' #.findFiles("*.sqlite", 5)
#' 
#' @keywords internal
#' 
.findFiles <- function(pattern, recurse_levels){
    ori_wd <- getwd()
    
    # recurse_levels patterns:
    # rl 0 "."
    # rl 1 ".."
    # rl 2 "../.."
    # rl n "..[n-1](/..)
    
    if (recurse_levels == 0){
        path <- "."
    } else {
        path <- ".."
        if ((recurse_levels-1)>0){
            path <- paste0(path, 
                           paste0(rep("/..", recurse_levels-1), collapse = ""))
        }
    }
    
    setwd(path)
    #print(getwd())
    .mm(paste0("Searching for ", pattern, " files in: ", getwd()), "dev")
    
    matches <- dir(path = getwd(), pattern = pattern, 
                   recursive = TRUE, full.names = TRUE)
    
    
    setwd(ori_wd)
    #print(matches)
    
    return(matches)
}






#' Generate a progress bar scaled by width
#' 
#' @param i Current stage
#' @param n Total number of stages
#' @param width Number of characters for progress display 
#'     (the whole bar will have 2 extra ones used for marking both ends)
#' @return Nothing. Output progress to console
#' 
#' @examples 
#' 
#' #for (i in 1:10){
#' #    .progressBar(i, 10, 100)
#' #    Sys.sleep(1)
#' #}
#' 
#' @keywords internal
#' 
.progressBar <- function(i, n, width=78){
    
    if (i > n) i <- n
    
    x <- (i*width) %/% n
    
    
    #cat("PROGRESS:\n")
    string <- paste0("[", paste0(rep("|", x), collapse = ""), 
                     paste0(rep(" ", width-x), collapse = ""), "]")
    cat(string, " \r")
    utils::flush.console()
}

