#SEARCH_FUNCTIONS.R
#Merged indev2.R script and testf function


#USAGE
#st <- list(library_strategy="ChIP-Seq", gene="STAT1", antibody="STAT1", secondary_library_strategy = "RNA-Seq")
#do.call(searchForTerm, st)


#' Search for samples matching criteria of interest
#' 
#' \code{searchForTerm} provides an automated framework for searching for samples matching a range of different criteria from the SRA database. It also supplements the sample information with data from GEO.
#' 
#' 
#' @param library_strategy Experimental method (e.g. RNA-Seq, ChIP-Seq). Only one library_strategy is allowed in a single query. To get a list of available library strategies, run \code{ getDatabaseInformation()}
#' @param gene A character vector with genes of interest (it is recommended to provide a few synonyms)
#' @param antibody A character vector with antibodies of interest (it is recommended to provide a few synonyms, some studies annotate their antibodies with trade names/symbols)
#' @param cell_type A character vector describing source types of interest (cell type, tissue, organ etc.)
#' @param treatment A character vector with keywords regarding treatment protocol
#' @param species A character vector with taxonomy IDs
#' @param platform A character vector with sequencing platforms
#' @param secondary_library_strategy Additional experimental method of interest filtered from the studies featured in search results
#' 
#' @return Nothing. Creates a range of files with the query information and search results.
#' 
#' @section Argument requirements:
#' \strong{REQUIRED}: library_strategy AND at least one of: gene, antibody, cell_type or treatment
#' 
#' \strong{OPTIONAL}: species, platform, secondary_library_strategy
#' 
#' @section Further information:
#' For further information (especially on the output files) please refer to the package vignettes.
#' 
#' 
#' 
#' 
#' @export
#NEW searchForTerm FUNCTION (in progress) - WILL BE COMPLETED IN INDEV3.R
searchForTerm <- function(library_strategy, gene=NULL, antibody=NULL, cell_type=NULL, treatment=NULL, species=NULL, platform=NULL, secondary_library_strategy=NULL){
  #Function for searching
  #Inputs: search terms
  #REQUIRED: library_strategy AND at least one of gene, antibody, cell_type or treatment
  #OPTIONAL: species, platform, secondary_library_strategy

  
  #============================================================================
  # Checking arguments
  #============================================================================

  #REMOVE EMPTY STRINGS FROM PROVIDED INPUTS
  variable_list <- c("library_strategy", "gene", "antibody", "cell_type", "treatment", "species", "platform", "secondary_library_strategy")

  e <- environment()

  # The top 15 most popular entries for library_strategy
  supported_library_strategy <- c("WGS",
                                  "AMPLICON",
                                  "RNA-Seq",
                                  "OTHER",
                                  "WXS",
                                  "ChIP-Seq",
                                  "CLONE",
                                  "POOLCLONE",
                                  "Bisulfite-Seq",
                                  "SELEX",
                                  "miRNA-Seq",
                                  "WGA",
                                  "RAD-Seq",
                                  "Targeted-Capture",
                                  "ATAC-seq")
  
  supported_secondary_library_strategy <- supported_library_strategy

  #Require library_strategy (not missing, not NULL, not "", not a vector of length >1, must belong to the list)
  if (missing(library_strategy)){
    stop("No library_strategy provided")
  } else if (is.null(library_strategy)) { #NULL needs to be checked before NA (otherwise will get logical(0))
    stop("No library_strategy provided")
  } else if (is.na(library_strategy)){
    stop("No library_strategy provided")
  } else if (library_strategy == "") {
    stop("No library_strategy provided")
  } else if (length(library_strategy) != 1 ) {
    stop("Only one library_strategy can be supported at any given time")
  } else if (!(library_strategy %in% supported_library_strategy)){
    sls <- paste(supported_library_strategy, collapse = ", ")
    warning(paste0("Library strategy does not belong to the recommended options. If you do not get satisfying results, please try one of: ", sls, ". Alternatively, check your options with getDatabaseInformation()"))
  }

  
  #Check validity of secondary_library_strategy (may be NULL, BUT not NA, not "", all elements belong to the list)
  if (!is.null(secondary_library_strategy)){
    if (is.na(secondary_library_strategy)){
      stop("No secondary_library_strategy provided")
    } else if (secondary_library_strategy == "") {
      stop("No secondary_library_strategy provided")
    } else {
      for (l in seq_along(secondary_library_strategy)){
        if (!(secondary_library_strategy[[l]] %in% supported_secondary_library_strategy)) {
          sls <- paste(supported_secondary_library_strategy, collapse = ", ")
          stop(paste0("Library strategy does not belong) to the list of supported library strategies. Please try one of: ", sls, "."))
        }
      }
    }
  }



  for (v in seq_along(variable_list)){
    cn <- variable_list[v]
    for (i in seq_along(get(cn))){
      #print(e[[cn]][i])
      if (get(cn)[i]==""){
        #print(e[[cn]][i])
        e[[cn]] <- e[[cn]][-i]
      }
    }
  }
  #NOTE: this converts empty string elements (of length 1) into empty elements (of length 0)
  #To test for presence of a search term, use length(search_term)==0

  #CHECK IF AT LEAST ONE OF REQUIRED SEARCH_TERMS IS PROVIDED
  min_one_required <- c("gene", "antibody", "cell_type", "treatment")
  l <- 0
  for (v in seq_along(min_one_required)){
    l <- l + length(e[[min_one_required[v]]])
  }
  if (l==0){
    mess <- paste0("Minimum one search term is required from the following list: ", paste0(min_one_required, collapse = ", "))
    stop(mess)
  }

  #PRINT SEARCH CONDITIONS SUMMARY
  print("SEARCH CONDITIONS SUMMARY")
  print(paste0("Selected gene: ", paste(gene, collapse = " OR ")))
  print(paste0("Selected antibody: ", paste(antibody, collapse = " OR ")))
  print(paste0("Selected cell type: ", paste(cell_type, collapse = " OR ")))
  print(paste0("Selected treatment: ", paste(treatment, collapse = " OR ")))
  print(paste0("Selected species: ", paste(species, collapse = " OR ")))
  print(paste0("Selected library strategy: ", paste(library_strategy, collapse = " OR ")))
  print(paste0("Selected platform: ", paste(platform, collapse = " OR ")))
  print(paste0("Selected secondary library_strategy: ", paste(secondary_library_strategy, collapse = " OR ")))


  #============================================================================





  #============================================================================
  # Save search terms as a list
  #============================================================================
  st <- list(library_strategy=library_strategy, gene=gene, antibody=antibody, cell_type=cell_type, treatment=treatment, species=species, platform=platform, secondary_library_strategy=secondary_library_strategy)
  #============================================================================


  #============================================================================
  # Save search parameters and call details in a file
  #============================================================================
  parameterRecordGenerator(st = st, file = do.call(filenameGenerator, c(st, list(output="PAR"), list(file_type="tab"))), fun_name = "searchForTerm")

  callRecordGenerator(file = do.call(filenameGenerator, c(st, list(output="CALL"), list(file_type="Rda"))))
  #============================================================================



  #============================================================================
  #Find entries containing search_terms (sample_list)
  #============================================================================

  sample_list <- do.call(searchSRA, st[-grep("secondary_library_strategy", names(st))])
  #============================================================================


  #============================================================================
  #Add input and control columns
  #============================================================================
  if (st$library_strategy == "ChIP-Seq"){
    sample_list$input <- "N"
  } else {
    sample_list$input <- NA
  }

  if (st$library_strategy == "RNA-Seq"){
    sample_list$control <- "N"
  } else {
    sample_list$control <- NA
  }
  #============================================================================

  saveRDS(sample_list, "sample_list.Rda")



  #============================================================================
  #For all the projects from the sample list, find all the corresponding SRRs
  #============================================================================
  #all_list <- searchForSRPChildren(unique(sample_list$study_accession), sra_columns)
  all_list <- searchForSRPChildren(unique(sample_list$study_accession), "*") #Changed to avoid problems with column management
  all_list$input <- NA
  all_list$control <- NA
  #============================================================================





  #============================================================================
  #Combine the sample_list with the list of all SRRs
  #Mark SRRs from sample_list with "N" and the remaining SRRs with "check"
  #============================================================================
  #spider_combined <- rbindUnique(sample_list, all_list) #PREVIOUSLY
  spider_combined <- rbindUniqueCols(x=sample_list, y = all_list, disregard_columns=c("input", "control"))
  #============================================================================

  #spider_combined$input <- "check" #===*===Pondered having a separate column for checking inputs (different from the one added by rbindUnique)

  ##Extracting information from sample_attribute column
  #spider_combined$sra_sa_sample_type <- NA
  #spider_combined$sra_sa_cell_line <- NA
  #spider_combined$sra_sa_cell_type <- NA
  #spider_combined$sra_sa_antibody <- NA


  #===*===
  #Search for input-like entries and label them "I?"
  #===*===
  #Decide which columns to search

  #============================================================================
  #Extract GSMs from the experiment_title
  #============================================================================
  spider_combined <- gsmExtractor(spider_combined)
  #============================================================================





  #============================================================================
  #Extract SRA sample attributes
  #============================================================================
  spider_combined <- saExtractor(spider_combined)
  #============================================================================




  #============================================================================
  #Detect inputs (ChIP) and controls (RNA)
  #============================================================================
  spider_combined <- inputDetector(spider_combined) #Detect ChIP-Seq inputs

  #spider_combined$rna_control <- NA #Add new column
  spider_combined <- controlDetector(spider_combined) #Detect RNA-Seq controls
  #============================================================================



  #============================================================================
  #Add columns for sample sheets (lane and merge* (will label it mer to avoid interference with merge function))
  #============================================================================
  spider_combined <- mergeDetector(spider_combined)
  missingRunVerifier(spider_combined$run_accession) #Check if there are any missing runs ===*=== Disabled (dbSendQuery stopped working!)
  #============================================================================


  #============================================================================
  #Add pairedEnd column
  #============================================================================
  spider_combined <- pairedEndConverter(spider_combined)
  #============================================================================



  #============================================================================
  #Search for entries in GEO
  #============================================================================
  #-------------------------
  #Inputs
  gsm_db_name <- "geo_con"
  database_env <- ".GlobalEnv"
  gsm_columns <- c("gsm", "series_id", "gpl", "title", "source_name_ch1", "organism_ch1", "characteristics_ch1")

  #gse_columns <- c("title", "pubmed_id")
  gse_columns <- c("pubmed_id")


  #gsm_list <- test12[1:10, 2]
  #gsm_list <- c("GSM2342088")
  #gsm_list <- c("GSM2342088", "GSM2140962")

  gsm_list <- spider_combined$sample #Get GSMs from column created by gsmExtractor()
  gsm_list <- unique(gsm_list[!is.na(gsm_list)]) #Leave only unique, non-na entries
  #-------------------------

  spider_geo <- geoFinder(get(gsm_db_name, envir = get(database_env)), gsm_list = gsm_list, gsm_columns = gsm_columns, gse_columns = gse_columns)

  #============================================================================



  #============================================================================
  #Extract characteristics_ch1 into separate columns
  #============================================================================
  spider_geo <- chExtractor(spider_geo)
  #============================================================================


  #============================================================================
  #Merge spider_comibined and spider_geo
  #============================================================================
  spider_combined <- merge(spider_combined, spider_geo, by.x = "sample", by.y = "gsm", all.x = TRUE)

  #saveRDS(spider_combined, "spider_combined_prelim.Rda")

  spider_superseries <- superseriesVerifier(spider_combined$series_id) #Give info on superseries
  #============================================================================



  #============================================================================
  # Generate outputs
  #============================================================================

  outputGenerator(spider_combined, spider_superseries, st = st)
  #============================================================================

  #return(spider_combined)


}
