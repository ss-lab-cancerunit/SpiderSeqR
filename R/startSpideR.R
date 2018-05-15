

startSpideR <- function(dir){
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
  age_limit <- 360 #Maximum acceptable age of the database files (in days)
  
  
  #==========================================================
  # SRAmetadb
  #==========================================================
  
  # NO SRA FILE
  if(!file.exists(sra_file)){ # NO FILE
    
    print(paste0("The file ", sra_file, " was not found in the current working directory"))
    print("Would you like to download the file now?")
    
    sra_menu <- menu(c("yes", "no"))
    if (sra_menu == 1){
      print("Downloading the file")
      sra_file <<- SRAdb::getSRAdbFile()
    } else {
      stop(paste0(sra_file, " file is necessary for the functioning of the package"))
    }
  }
  
  
  # OLD SRA FILE
  if(file.exists(sra_file) & (difftime(Sys.Date(), file.info(sra_file)$mtime, units = "days") > age_limit) ){ # OLD FILE
    
    print(paste0("The file ", sra_file, " is out of date"))
    print(paste0("Last modified: ", file.info(sra_file)$mtime))
    print("Would you like to download a new version of the file right now? (this is recommended, though not necessary)?")
    
    sra_menu <- menu(c("yes", "no"))
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
    
    geo_menu <- menu(c("yes", "no"))
    if (geo_menu == 1){
      print("Downloading the file")
      geo_gz_file <- GEOmetadb::getSQLiteFile(destfile = "GEOmetadb.sqlite.gz")
    } else {
      stop(paste0(geo_file, " file is necessary for the functioning of the package"))
    }
  }
  
  
  # OLD GEO FILE
  if(file.exists(geo_file) & (difftime(Sys.Date(), file.info(geo_file)$mtime, units = "days") > age_limit) ){ # OLD FILE
    
    print(paste0("The file ", geo_file, " is out of date"))
    print(paste0("Last modified: ", file.info(geo_file)$mtime))
    print("Would you like to download a new version of the file right now? (this is recommended, though not necessary)?")
    
    geo_menu <- menu(c("yes", "no"))
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
    srr_gsm_menu <- menu(c("yes", "no"))
    if (srr_gsm_menu == 1){ # WILLING TO CREATE DB
      print("Database will be created shortly")
      db_needed <- TRUE
    } else { # DECLINED CREATION OF DB
      db_needed <- FALSE
      stop("The database is necessary for the functioning of the package")
    }
    
  } else { # FILE PRESENT
    if (difftime(Sys.Date(), file.info(srr_gsm_file)$mtime, units = "days") < age_limit){ # FILE UP TO DATE
      db_needed <- FALSE
      print("The custom database for converting between SRA and GEO is up to date")
      print(paste0("Last modified: ", file.info(srr_gsm_file)$mtime))
      
    } else { # OLD FILE
      
      print(paste0("The file ", srr_gsm_file, " is out of date"))
      print(paste0("Last modified: ", file.info(srr_gsm_file)$mtime))
      print("Would you like to create a new version of the file right now? (this is recommended, though not necessary)?")
      srr_gsm_menu <- menu(c("yes", "no"))
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
    
    sra_con <- dbConnect(SQLite(), dbname = sra_file)
    geo_con <- dbConnect(SQLite(), dbname = geo_file)
    
    db_df <- data.frame()
    rs <- dbSendQuery(sra_con, "SELECT
                      run_accession,
                      experiment_accession,
                      sample_accession,
                      study_accession,
                      run_alias, --For GSM
                      experiment_attribute --For GSM
                      FROM sra WHERE run_alias LIKE 'GSM%' OR experiment_attribute LIKE '%GSM%'")
    
    while (!dbHasCompleted(rs)){
      chunk <- dbFetch(rs, 1000)
      
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
    
    
    dbClearResult(rs)
    
    
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
    #db_df <- db_df[digitSort(order_columns),]
    
    #Save df as an slite object
    srr_gsm <- dbConnect(SQLite(), dbname = "SRR_GSM.sqlite")
    dbWriteTable(conn = srr_gsm, name = "srr_gsm", value = db_df, overwrite = TRUE)
    
    .GlobalEnv$db_df <- db_df
    
    dbDisconnect(sra_con)
    dbDisconnect(geo_con)
    
    
  }
  #==========================================================
  
  
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  .GlobalEnv$sra_con <- dbConnect(SQLite(), dbname = sra_file)
  .GlobalEnv$geo_con <- dbConnect(SQLite(), dbname = geo_file)
  .GlobalEnv$srr_gsm <- dbConnect(SQLite(), dbname = srr_gsm_file)
  
  print("Welcome to SpideR")
  
  
  setwd(ori_wd)
}
