


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





print("setup")


assign("x", 2, envir = .GlobalEnv)
print(ls(envir = .GlobalEnv))


q <- getSpiderSeqROption("quiet")

setSpiderSeqROption("quiet", TRUE)




preserve_connection <- TRUE # Change to FALSE for real ===*===

# If pre-existing connections are to be preserved (NOTE: this will throw a warning due to connection which is not disconnected)
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






