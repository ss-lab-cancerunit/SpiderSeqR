


#' INFO ON TESTS
#' 
#' test-Accession_Search_Functions
#' - searchForAccessionAcrossDBsDF
#' - searchSRAForAccession
#' - searchGEOForGSE
#' - searchGEOForGSM
#' NOTE: currently expecting errors for incorrect or non-existent entries.
#' Consider changing the logic to warnings (within searchForAccessionAcrossDBsDF etc.) 
#' (search for 'No matching entries found') ===*===
#' 
#' test-convertAccession
#' - convertAccession (commutative, associative, count)
#' 
#' test-convertCategoriesToLibraryStrategyType
#' - convertCategoriesToLibraryStrategyType
#' 
#' test-manageLibraryStrategy
#' 
#' test-orderAccessions_list
#' 
#' test-orderAccessions_vector
#' 
#' test-superseriesVerifier
#' 






#' # General setup
#' 
#' 



print("setup")


#assign("x", 2, envir = .GlobalEnv)
#print(ls(envir = .GlobalEnv))


q <- getSpiderSeqROption("quiet")

setSpiderSeqROption("quiet", TRUE)

testing <- getSpiderSeqROption("testing")

setSpiderSeqROption("testing", TRUE)

local_run <- FALSE



#' # Remove existing database files and prepare new ones for testing
#' 

db_files <- list.files("testdata/Mock_Database_Files", 
                        "*.sqlite", recursive=TRUE, full.names = TRUE)
for (d in db_files){
    file.remove(d)
}
print(db_files)



#------------------
# TBD
#current_files <- list.files("testdata/Mock_Database_Files", recursive=TRUE, 
#                              full.names = TRUE)

#current_files <- paste0(getwd(), "/", current_files)
#for (i in current_files){
#  file.remove(i, recursive=TRUE)
#}
#-------------------


# Populate the directory with mock database files
# Only run if testing locally
if (local_run){
  .createMockSRA("testdata/Mock_Database_Files/All_Present")
  .createMockGEO("testdata/Mock_Database_Files/All_Present")
  .createMockCustomDB("testdata/Mock_Database_Files/All_Present")
  
  .createMockSRA("testdata/Mock_Database_Files/Files_in_Subdirectory/Subdir")
  .createMockGEO("testdata/Mock_Database_Files/Files_in_Subdirectory/Subdir")
  .createMockCustomDB("testdata/Mock_Database_Files/Files_in_Subdirectory/Subdir")
  
  
  #.createMockSRA()
  .createMockGEO("testdata/Mock_Database_Files/SRA_Missing")
  .createMockCustomDB("testdata/Mock_Database_Files/SRA_Missing")
  
  
  .createMockSRA("testdata/Mock_Database_Files/GEO_Missing")
  #.createMockGEO()
  .createMockCustomDB("testdata/Mock_Database_Files/GEO_Missing")
  
  
  .createMockSRA("testdata/Mock_Database_Files/SpiderSeqR_Missing")
  .createMockGEO("testdata/Mock_Database_Files/SpiderSeqR_Missing")
  #.createMockCustomDB()
}







#' # Preserve existing connections
#' 
#' 

preserve_connection <- TRUE # Change to FALSE for real ===*===

# If pre-existing connections are to be preserved 
# (NOTE: this will throw a warning due to connection which is not disconnected)
if (preserve_connection){
  
  if (exists("sra_con", envir = .GlobalEnv)){
    assign("temp_sra_con", get("sra_con", envir = .GlobalEnv), envir = .GlobalEnv)
  }
  
  if (exists("geo_con", envir = .GlobalEnv)){
    assign("temp_geo_con", get("geo_con", envir = .GlobalEnv), envir = .GlobalEnv)
  }
  
  if (exists("srr_gsm", envir = .GlobalEnv)){
    assign("temp_srr_gsm", get("srr_gsm", envir = .GlobalEnv), envir = .GlobalEnv)
  }
}



startSpiderSeqRDemo()







