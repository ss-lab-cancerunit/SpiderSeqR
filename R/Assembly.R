
#' Fetch the documentation ere ====*===

ass_startSpiderSeqR <- function(path, 
                            general_expiry=90, 
                            sra_expiry=NULL, 
                            geo_expiry=NULL, 
                            srr_gsm_expiry=NULL){
    
    
    .mm(cli::rule(), "comm")
    .mm("Welcome to SpiderSeqR!!!", "qn")
    .mm(paste0("Please wait while the database files and connections ",
               "are being configured..."), "comm")
    .mm(cli::rule(), "comm")
    
    path <- normalizePath(path, "\\")
    
    #print(path)
    
    # FIND FILES IN THE DIRECTORY
    file_paths <- .findDBFiles(path=path)
    
    #print(getwd())
    print("THESE ARE THE FILES (inside wd)")
    print(dir())
    #print(c(file.exists(file_paths[1]), 
    #        file.exists(file_paths[2]), 
    #        file.exists(file_paths[3])))
    
    
    print("ALL FILES EXIST")
    print(file.exists(file_paths))
    
    
    
    #sra_file <- file_paths[1]
    #geo_file <- file_paths[2]
    #srr_gsm_file <- file_paths[3]
    
    #print(file_paths)
    
    
    missing_logical <- .missingFileCheck(file_paths)
    #missing_logical <- do.call(.missingFileCheck, list(file_paths))
    
    
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
    
    
    raw_expiry_parameters <- list(general_expiry=general_expiry,
                                  sra_expiry=sra_expiry,
                                  geo_expiry=geo_expiry,
                                  srr_gsm_expiry=srr_gsm_expiry,
                                  missing_file_number=sum(missing_logical))
    
    
    expiry_parameters <- do.call(.setExpiryParameters, c(raw_expiry_parameters))
    
    .mm(cli::rule(), "comm")
    
    
    .GlobalEnv$file_paths <- file_paths
    .GlobalEnv$expiry_parameters <- expiry_parameters
    
    
    print("THIS IS THE END")
    return(TRUE)
    
    
    # Check (or download) SRA and GEO files
    for (i in 1:2){
        
        .checkDBFile(path=path, 
                     db_file=file_paths[[i]], 
                     db_file_name=.DBNames()[i], 
                     db_expiry=expiry_parameters[[i]])
        
        .mm(cli::rule(), "comm")
        
    }
    
    print(sra_file)
    print(geo_file)
    print(srr_gsm_file)
    
    print(file.exists(sra_file))
    print(file.exists(geo_file))
    
    #==========================================================
    # RE-CHECK THE PRESENCE OF FILES (have they downloaded successfully???)
    #==========================================================
    if (!(file.exists(sra_file) & file.exists(geo_file))){
        warning(paste0("Something went wrong. Some database files might ",
                       "be missing. Please run this function again."))
    } else {
        .mm(paste0(
            "Both db files are present (remember not to (re)move them!)\n",
            "Proceeding to the final step ",
            "(checking for the custom database)."), 
            "comm")
    }
    #==========================================================
    
    
    .mm(cli::rule(), "comm")
    
    .checkDBFile(path=path, 
                 db_file=file_paths[[3]], 
                 db_file_name=.DBNames()[3], 
                 db_expiry=expiry_parameters[[3]])
    
    
    if (!isTRUE(getSpiderSeqROption("testing"))){
        .setDBConnections(sra_file=sra_file, 
                          geo_file=geo_file, 
                          srr_gsm_file=srr_gsm_file)
        
    } else {
        #setwd(ori_wd)
        # Early exit if testing (no real database connections established)
        return(TRUE)
    }
    
    #setwd(ori_wd)
    
    con_names <- c("sra_con", "geo_con", "srr_gsm")
    
    # Display metaInfo tables from the databases
    for (i in seq_along(con_names)){
        .mm(cli::rule(), "comm")
        .getFurtherDBInfo(db_file_name = .DBNames()[i], 
                          database_name = con_names[i])
    }
    
    
    .mm(cli::rule(), "comm")
    .mm("SpiderSeqR setup complete", "qn")
    
    
}
