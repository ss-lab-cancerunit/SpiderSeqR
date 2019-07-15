

#' Prepare the environment to run SpideR
#' 
#' \code{startSpideR} prepares the environment so that other SpideR functions can be used. Run \code{startSpideR} every time you begin with a clear environment and want to use any of the other SpideR functions.
#' In particular, the function does the following:
#' \itemize{
#'     \item Ensure that SRAmetadb.sqlite is downloaded and up to date
#'     \item Ensure that GEOmetadb.sqlite is downloaded and up to date
#'     \item Ensure that SRR_GSM.sqlite is created and up to date
#'     \item Set up database connections to the above files in the \code{.GlobalEnv}
#' } 
#' 
#' 
#' Depending on the contents of the specified directory, \code{startSpideR} may download/create the database files. It will always create the database connections in the global environment.
#' 
#' It is necessary to fulfil all the above requirements; without them the package will not work.
#' The first two database files are relatively large in size (33 GB and 6 GB at the time of writing), so please ensure that you have adequate internet connection and sufficient disk space.
#' 
#' It is recommended that the newest version of the databases is used. However, it is possible to ignore this requirement by manually setting the expiry date of the files (or by running default settings of the function and selecting the option not to download the newer files).
#' 
#' 
#' 
#' @section When to run \code{startSpideR}?:
#' 
#' Run \code{startSpideR} every time you start with a fresh environment. There is no harm in running it too many times.
#' 
#' 
#' 
#' @section Which options to choose from \code{startSpideR} menu options?:
#' 
#' Should you have any missing or outdated files in the specified directory, \code{startSpideR} will offer to download/create the files. 
#' 
#' Please note that it is \emph{required} to have all the three database files; if you choose not to download/create them, it will not be possible to run other SpideR functions. 
#' 
#' However, it is \emph{not required} for all the files to be up-to-date; \code{startSpideR} will suggest re-downloading/re-creating the files, but you can still use SpideR, even if you do not agree to re-download/re-create the files.
#' 
#' 
#' 
#' @section Time and space requirements:
#' 
#' The following files must be present in order to run other SpideR functions; \code{startSpideR} will download/create them if necessary:
#' \itemize{
#'   \item SRAmetadb.sqlite (from SRAdb package)
#'   \item GEOmetadb.sqlite (from GEOmetadb package)
#'   \item SRR_GSM.sqlite (custom-made at the time of running \code{startSpideR})
#' }
#' 
#' 
#' They take approximately 40 GB of disk space (33 GB, 6 GB, <50 MB respectively at the time of writing), so please ensure that you have adequate internet connection and sufficient disk space. In order to save disk space, the previous files will be overwritten when downloading a newer version.
#' 
#' 
#' Running \code{startSpideR} for the first time will inevitably take some time, because large database files need to be downloaded and custom database created. However, once all the files are present, the function should take an order of \emph{seconds} to complete (it is a matter of setting up database connections).
#' 
#' 
#' 
#' @examples 
#' 
#' # Database files are stored (or will be downloaded) 
#' #    in the working directory
#' startSpideR(dir = getwd()) 
#' 
#' # Use the following if you would like to download 
#' #   the newest database files
#' startSpideR(dir = getwd(), general_expiry = 0) 
#' 
#' # Use the following if you have old database files
#' #   that you do not wish to re-download on this occasion
#' startSpideR(dir = getwd(), general_expiry = 365) 
#' 
#' # Use the following if you only wish to ignore 
#' #    an old SRAmetadb.sqlite file, 
#' #    but get reminders to re-download the other files
#' startSpideR(dir = getwd(), sra_expiry = 365) 
#' 
#' 
#' 
#' 
#' @param dir Directory where database files will be stored
#' @param general_expiry Maximum number of days since creation of all database files
#' @param sra_expiry Maximum number of days since creation of SRAmetadb.sqlite file
#' @param geo_expiry Maximum number of days since creation of GEOmetadb.sqlite file
#' @param srr_gsm_expiry Maximum number of days since creation of SRR_GSM.sqlite file
#' 
#' @return Nothing. If necessary, it may download/create database files. Sets up database connections in the global environment.
#' 
#'
#'
#' @export
startSpideR <- function(dir, general_expiry=90, sra_expiry, geo_expiry, srr_gsm_expiry){
  ori_wd <- getwd()
  setwd(dir)
  print(paste0("Location of database files: ", getwd()))

  
  #Setup:
  # - SRAmetadb
  # - GEOmetadb
  # - SRR_GSM (custom database for converting between the ids of SRA and GSMs (GEO))
  #print("Setting up SpideR")
  
  # Logic:
  # - SRAmetadb:
  #    * if does not exist - download (else: stop)
  #    * if exists and not up to date - offer to re-download (else: warning)
  # - GEOmetadb:
  #    * if does not exist - download (else: stop)
  #    * if exists and not up to date - offer to re-download (else: warning)
  # - SRR_GSM:
  #    * if file exists
  #        - if in date - do nth
  #        - if not up to date - offer to re-create (else: warning)
  #    * if does not exist - create (else: stop)
  
  
  sra_file <- "SRAmetadb.sqlite"
  geo_file <- "GEOmetadb.sqlite"
  srr_gsm_file <- "SRR_GSM.sqlite"

  
  #==========================================================
  # Setting expiry parameters
  #==========================================================
  # Logic:
  # Use specific parameters (sra, geo, srr_gsm) if available. If not, use the expiry date from general_expiry
  
  if ((!missing(general_expiry))&(!missing(sra_expiry))&(!missing(geo_expiry))&(!missing(srr_gsm_expiry))){
    warning("general_expiry argument will be ignored, since all the individual expiry dates have been provided")
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
  
  if ( !(is.numeric(general_expiry)) | !(is.numeric(sra_expiry)) | !(is.numeric(geo_expiry)) | !(is.numeric(srr_gsm_expiry)) ){
    stop("Expiry parameters must be numeric")
  }
  #==========================================================
  
  
  print("Expiry dates for databases (number of days since file creation date):")
  print(paste0("SRA: ", sra_expiry, " days"))
  print(paste0("GEO: ", geo_expiry, " days"))
  print(paste0("SRR_GSM: ", srr_gsm_expiry, " days"))
  
  
  #==========================================================
  # SRAmetadb
  #==========================================================
  
  # NO SRA FILE
  if(!file.exists(sra_file)){ # NO FILE
    
    print(paste0("The file ", sra_file, " was not found in the current working directory"))
    print("Would you like to download the file now?")
    
    sra_menu <- utils::menu(c("yes", "no"))
    if (sra_menu == 1){
      print("Downloading the file")
      sra_file <<- SRAdb::getSRAdbFile()
    } else {
      stop(paste0(sra_file, " file is necessary for the functioning of the package"))
    }
  }
  
  
  # OLD SRA FILE
  if(file.exists(sra_file) & (difftime(Sys.Date(), file.info(sra_file)$mtime, units = "days") > sra_expiry) ){ # OLD FILE
    
    print(paste0("The file ", sra_file, " is out of date"))
    print(paste0("Last modified: ", file.info(sra_file)$mtime))
    print("Would you like to download a new version of the file right now? (this is recommended, though not necessary)?")
    
    sra_menu <- utils::menu(c("yes", "no"))
    if (sra_menu == 1){
      print("Downloading the file")
      sra_file <<- SRAdb::getSRAdbFile()
    } else {
      warning(paste0("Next time consider downloading a new version of ", sra_file, " file"))
    }
    
  } else if(file.exists(sra_file)) {
    print(paste0("The file ", sra_file, " is up to date"))
    print(paste0("Last modified: ", file.info(sra_file)$mtime))
  }
  
  
  #==========================================================
  
  
  
  
  
  
  #==========================================================
  # GEOmetadb
  #==========================================================
  
  # NO GEO FILE
  if(!file.exists(geo_file)){ # NO FILE
    
    
    print(paste0("The file ", geo_file, " was not found in the current working directory"))
    print("Would you like to download the file right now?")
    
    geo_menu <- utils::menu(c("yes", "no"))
    if (geo_menu == 1){
      print("Downloading the file")
      geo_gz_file <- GEOmetadb::getSQLiteFile(destfile = "GEOmetadb.sqlite.gz")
    } else {
      stop(paste0(geo_file, " file is necessary for the functioning of the package"))
    }
  }
  
  
  # OLD GEO FILE
  if(file.exists(geo_file) & (difftime(Sys.Date(), file.info(geo_file)$mtime, units = "days") > geo_expiry) ){ # OLD FILE
    
    print(paste0("The file ", geo_file, " is out of date"))
    print(paste0("Last modified: ", file.info(geo_file)$mtime))
    print("Would you like to download a new version of the file right now? (this is recommended, though not necessary)?")
    
    geo_menu <- utils::menu(c("yes", "no"))
    if (geo_menu == 1){
      print("Downloading the file")
      geo_gz_file <- GEOmetadb::getSQLiteFile(destfile = "GEOmetadb.sqlite.gz")
    } else {
      warning(paste0("Next time consider downloading a new version of ", geo_file, " file"))
    }
    
  } else if(file.exists(geo_file)) {
    print(paste0("The file ", geo_file, " is up to date"))
    print(paste0("Last modified: ", file.info(geo_file)$mtime))
  }
  
  #==========================================================
  
  
  
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #==========================================================
  # RE-CHECK THE PRESENCE OF FILES (have they downloaded successfully???)
  #==========================================================
  if (!(file.exists(sra_file) & file.exists(geo_file))){
    warning("Something went wrong. Some database files might be missing. Consider loading the package again.")
  } else {
    print("Both db files are present (remember not to remove them!). Proceeding to the final step (custom database creation).")
  }
  #==========================================================
  
  
  
  #==========================================================
  # FIND OUT WHETHER IT IS NECESSARY TO CREATE CUSTOM DATABASE
  #==========================================================
  
  if (!file.exists(srr_gsm_file)){ # NO FILE
    
    print("Would you like to create a cutstom database for converting between GEO and SRA? This might take a little while, but it is necessary for the correct functioning of the package.")
    srr_gsm_menu <- utils::menu(c("yes", "no"))
    if (srr_gsm_menu == 1){ # WILLING TO CREATE DB
      print("Database will be created shortly")
      db_needed <- TRUE
    } else { # DECLINED CREATION OF DB
      db_needed <- FALSE
      stop("The database is necessary for the functioning of the package")
    }
    
  } else { # FILE PRESENT
    if (difftime(Sys.Date(), file.info(srr_gsm_file)$mtime, units = "days") < srr_gsm_expiry){ # FILE UP TO DATE
      db_needed <- FALSE
      print("The custom database for converting between SRA and GEO is up to date")
      print(paste0("Last modified: ", file.info(srr_gsm_file)$mtime))
      
    } else { # OLD FILE
      
      print(paste0("The file ", srr_gsm_file, " is out of date"))
      print(paste0("Last modified: ", file.info(srr_gsm_file)$mtime))
      print("Would you like to create a new version of the file right now? (this is recommended, though not necessary)?")
      srr_gsm_menu <- utils::menu(c("yes", "no"))
      if (srr_gsm_menu == 1){ # WILLING TO CREATE DB
        print("Database will be re-created shortly")
        db_needed <- TRUE
      } else { # DECLINED CREATION OF DB
        db_needed <- FALSE
        warning(paste0("Next time consider re-creating the ", srr_gsm_file, " file"))
      }
    }
    
  }
  #==========================================================
  
  
  
  #==========================================================
  # Create custom database
  #==========================================================
  if (db_needed){
    print("Creating the custom database")
    
    #==========================================================
    #SRR_GSM
    #==========================================================
    # Creating a new database for SRA runs:
    
    #IDEA
    # find entries which contain GSM in run alias
    #                               or in experiment_attribute (GEO Accession: GSM)
    
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
    rs <- DBI::dbSendQuery(sra_con, "SELECT
                      run_accession,
                      experiment_accession,
                      sample_accession,
                      study_accession,
                      run_alias, --For GSM
                      experiment_attribute --For GSM
                      FROM sra WHERE run_alias LIKE 'GSM%' OR experiment_attribute LIKE '%GSM%'")
    
    while (!DBI::dbHasCompleted(rs)){
      chunk <- DBI::dbFetch(rs, 1000)
      
      #Create intermediate columns for extracting GSM information
      chunk$run_gsm <- NA #from run_alias
      chunk$exp_gsm <- NA #from experiment_attribute
      
      #Find indices where GSM is present
      run_gsm_indices <- grepl("GSM\\d\\d\\d+", chunk$run_alias)
      #exp_gsm_indices <- grepl("GSM\\d\\d\\d+", chunk$experiment_attribute)
      exp_gsm_indices <- grepl("GEO Accession: GSM\\d\\d\\d+", chunk$experiment_attribute, ignore.case = TRUE)
      
      #Extract GSM information
      chunk$run_gsm[run_gsm_indices] <- gsub(".*?(GSM\\d\\d\\d+).*", "\\1", chunk$run_alias[run_gsm_indices])
      chunk$exp_gsm[exp_gsm_indices] <- gsub(".*?GEO Accession: (GSM\\d\\d\\d+).*", "\\1", chunk$experiment_attribute[exp_gsm_indices], ignore.case = TRUE)
      
      #Create a column to indicate whether GSMs agree between two columns
      chunk$gsm_check <- NA
      
      #Fill in check column
      both_indices <- !(is.na(chunk$run_gsm) | is.na(chunk$exp_gsm)) #Get indices where run_alias and experiment_attribute are both present
      chunk$gsm_check[both_indices] <- chunk$run_gsm[both_indices] == chunk$exp_gsm[both_indices] #Check those indices for equality
      #GSM_CHECK: NA - either one or both missing
      #           F - run_gsm and exp_gsm NOT the same
      #           T - run_gsm and exp_gsm the same
      
      #Create a new column for storing GSMs
      chunk$gsm <- NA
      
      #Extract information from exp_gsm and run_gsm columns
      chunk$gsm[!is.na(chunk$exp_gsm)] <- chunk$exp_gsm[!is.na(chunk$exp_gsm)] #Non-NA entries from experiment_attribute
      chunk$gsm[!is.na(chunk$run_gsm)] <- chunk$run_gsm[!is.na(chunk$run_gsm)] #Non-NA entries from run_alias
      #NOTE: if both exp_gsm and run_gsm are present, the GSM obtained from run_alias will be retained
      
      #Chunk columns at present: "run_accession", "experiment_accession", "sample_accession", "study_accession", "run_alias", "experiment_attribute", "run_gsm", "exp_gsm", "gsm_check"
      
      #Select columns
      chunk <- chunk[,c("run_accession", "experiment_accession", "sample_accession", "study_accession", "gsm", "gsm_check")]
      
      #Get the number of entries with GSM content
      print(sum(run_gsm_indices | exp_gsm_indices))
      
      db_df <- rbind(db_df, chunk)
      
    }
    
    
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
    DBI::dbWriteTable(conn = srr_gsm, name = "srr_gsm", value = db_df, overwrite = TRUE)
    
    .GlobalEnv$db_df <- db_df
    
    DBI::dbDisconnect(sra_con)
    DBI::dbDisconnect(geo_con)
    
    
  }
  #==========================================================
  
  
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  .GlobalEnv$sra_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = sra_file)
  .GlobalEnv$geo_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = geo_file)
  .GlobalEnv$srr_gsm <- DBI::dbConnect(RSQLite::SQLite(), dbname = srr_gsm_file)
  
  print("Welcome to SpideR")
  
  
  setwd(ori_wd)
  
}
