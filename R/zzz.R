#Copy of pryr's where function (couldn't load pryr)
where <- function(name, env = parent.frame()) {
    if (identical(env, emptyenv())) {
      # Base case
      stop("Can't find ", name, call. = FALSE)

    } else if (exists(name, envir = env, inherits = FALSE)) {
      # Success case
      env

    } else {
      # Recursive case
      where(name, parent.env(env))

    }
}


#PROBLEM WITH LOADING BIOCONDUCTOR PACKAGES:
#IN SESSION LOADING:
#source("https://bioconductor.org/biocLite.R")
#biocLite(c("SRAdb", "GEOmetadb"))



.onLoad <- function(libname, pkgname){
  #Setup:
  # - GEOmetadb
  # - SRAmetadb
  # - SRR_GSM (custom database for converting between the ids of SRA and GSMs (GEO))
  #print("Setting up SpideR")

  #==========================================================
  #GEOmetadb
  #==========================================================
  #if (file.exists("GEOmetadb.sqlite") & (difftime(Sys.Date(), file.info("GEOmetadb.sqlite")$mtime, units = "days") < 50)) {
  if(!file.exists("GEOmetadb.sqlite")){
    #if(1==1){
    #if(!file.exists("test.txt")){
    print("The file GEOmetadb.sqlite was not found in the current working directory")
    print("Would you like to download the file now?")
    geo_menu <- menu(c("yes", "no"))
    if (geo_menu == 1){
      print("Downloading the file")
      geofile <- getSQLiteFile(destfile = "GEOmetadb.sqlite.gz")
    } else {
      stop("GEOmetadb.sqlite is necessary to initiate package operation")
    }
  }
  #==========================================================

  #==========================================================
  #SRAmetadb
  #==========================================================
  srafile <- 'SRAmetadb.sqlite'

  #if (file.exists("SRAmetadb.sqlite") & (difftime(Sys.Date(), file.info("SRAmetadb.sqlite")$mtime, units = "days") < 50)){
  if(!file.exists("SRAmetadb.sqlite")){
    #if(!file.exists("test.txt")){
    print("The file SRAmetadb.sqlite was not found in the current working directory")
    print("Would you like to download the file now?")
    geo_menu <- menu(c("yes", "no"))
    if (geo_menu == 1){
      print("Downloading the file")
      srafile <<- getSRAdbFile()
    } else {
      stop("SRAmetadb.sqlite is necessary to initiate package operation")
    }
  }
  #==========================================================


  #==========================================================
  #Both files present
  #==========================================================
  if (file.exists("SRAmetadb.sqlite") & file.exists("GEOmetadb.sqlite")){

    print("Both db files are present (remember not to remove them!). Ready to proceed")


    #===*=== Remove when the time comes

    #This didn't work
    #sra_con <- dbConnect(SQLite(), dbname = 'SRAmetadb.sqlite')
    #geo_con <- dbConnect(SQLite(),'GEOmetadb.sqlite')


    #This did get the connections to the global environment, but the functions were not able to access it
    #Two options:
    # - change the functions to search for sra_con and geo_con in global environment
    # - establish db connection within each function
    #.GlobalEnv$sra_con <- dbConnect(SQLite(), dbname = 'SRAmetadb.sqlite')
    #.GlobalEnv$geo_con <- dbConnect(SQLite(),'GEOmetadb.sqlite')

    #print(where("sra_con"))


    if (file.exists("SRR_GSM.sqlite") & (difftime(Sys.Date(), file.info("SRR_GSM.sqlite")$mtime, units = "days") < 120) ){
      print("Custom database for converting between SRA and GEO is up to date")
      print(paste0("Last modified: ", file.info("SRR_GSM.sqlite")$mtime))
    } else {
      #Need to create the custom database
      print("Would you like to create a cutstom database for converting between GEO and SRA? This might take a little while, but it is necessary for the correct functioning of the package.")
      geo_menu <- menu(c("yes", "no"))
      if (geo_menu == 1){
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

        sra_con <- dbConnect(SQLite(), dbname = 'SRAmetadb.sqlite')
        geo_con <- dbConnect(SQLite(),'GEOmetadb.sqlite')

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
        srr_gsm <- dbConnect(SQLite(), dbname = 'SRR_GSM.sqlite')
        dbWriteTable(conn = srr_gsm, name = "srr_gsm", value = db_df, overwrite = TRUE)

        .GlobalEnv$db_df <- db_df

        dbDisconnect(sra_con)
        dbDisconnect(geo_con)
        #==========================================================



      } else {
        stop("The database is necessary for the functioning of the package")
      }
    }

  }
  #==========================================================

}



.onAttach <- function(libname, pkgname){
  packageStartupMessage("Welcome to SpideR")
}
