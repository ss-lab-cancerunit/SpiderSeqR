#SEARCH_FUNCTIONS.R
#Merged indev2.R script and testf function


#USAGE
#st <- list(SRA_library_strategy="ChIP-Seq", gene="STAT1", antibody="STAT1", SRA_secondary_library_strategy = "RNA-Seq")
#do.call(searchForTerm, st)


#' Search for samples matching criteria of interest
#' 
#' \code{searchForTerm} provides an automated framework for searching within SRA database for samples matching a range of different criteria (e.g. experimental method, tissue type). It also supplements the sample information with data from GEO (if available).
#' 
#' 
#' @param SRA_library_strategy Experimental method (e.g. RNA-Seq, ChIP-Seq). Only one SRA_library_strategy is allowed in a single query. To get a list of available library strategies, run \code{ getDatabaseInformation()}
#' @param gene A character vector with genes of interest (it is recommended to provide a few synonyms)
#' @param antibody A character vector with antibodies of interest (it is recommended to provide a few synonyms, some studies annotate their antibodies with trade names/symbols)
#' @param cell_type A character vector describing source types of interest (cell type, tissue, organ etc.)
#' @param treatment A character vector with keywords regarding treatment protocol
#' @param species A character vector with taxonomy IDs (e.g. "9606" for human)
#' @param platform A character vector with sequencing platforms
#' @param SRA_secondary_library_strategy Additional experimental method of interest filtered from the studies featured in search results
#' @param return_all A logical indicating what results should be returned. FALSE means that only samples matching criteria of interest will be returned (and their putative inputs/controls for RNA-Seq or ChIP-Seq). TRUE means that the whole SRPs will be returned(containing matching samples, but also all the other samples within an SRP). Defaults to TRUE
#' 
#' 
#' @return Nothing. Creates a range of files with the query information and search results.
#' 
#' @section Argument requirements:
#' \strong{REQUIRED}: SRA_library_strategy AND at least one of: gene, antibody, cell_type or treatment
#' 
#' \strong{OPTIONAL}: species, platform, SRA_secondary_library_strategy
#' 
#' @section Further information:
#' For further information (especially on the output files) please refer to the package vignettes.
#' 
#' 
#' @examples
#' 
#' #Simple search
#' searchForTerm(SRA_library_strategy = "RNA-Seq", gene = c("p53", "tp53"), species = "9606") 
#' 
#' #Search with parameters stored in a list
#' st <- list(SRA_library_strategy="ChIP-Seq", gene="STAT1", antibody="STAT1")
#' do.call(searchForTerm, st)
#' 
#' 
#' @export
#
#NEW searchForTerm FUNCTION (in progress) - WILL BE COMPLETED IN INDEV3.R
searchForTerm <- function(SRA_library_strategy, gene=NULL, antibody=NULL, cell_type=NULL, treatment=NULL, species=NULL, platform=NULL, SRA_secondary_library_strategy=NULL, return_all = TRUE){
    #Function for searching
    #Inputs: search terms
    #REQUIRED: SRA_library_strategy AND at least one of gene, antibody, cell_type or treatment
    #OPTIONAL: species, platform, SRA_secondary_library_strategy
    
    
    OTH_input <- NULL
    OTH_control <- NULL
    
    #============================================================================
    # Checking arguments
    #============================================================================
    
    #REMOVE EMPTY STRINGS FROM PROVIDED INPUTS
    variable_list <- c("SRA_library_strategy", "gene", "antibody", "cell_type", "treatment", "species", "platform", "SRA_secondary_library_strategy")
    
    e <- environment()
    
    # The top 15 most popular entries for SRA_library_strategy
    supported_SRA_library_strategy <- c("WGS",
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
    
    supported_SRA_secondary_library_strategy <- supported_SRA_library_strategy
    
    
    
    library_warning_message <- "Library strategy does not belong to the recommended options. If you do not get satisfying results, please run manageLibraryStrategy for further information" 
    
    
    #print(manageLibraryStrategy(SRA_secondary_library_strategy, task="check_can"))
    
    
    if( !manageLibraryStrategy(SRA_library_strategy, task="check_can") ){
        warning(library_warning_message)
    }
    
    
    #Require SRA_library_strategy (not missing, not NULL, not "", not a vector of length >1, must belong to the list)
    if (missing(SRA_library_strategy)){
        stop("No SRA_library_strategy provided")
    } else if (is.null(SRA_library_strategy)) { #NULL needs to be checked before NA (otherwise will get logical(0))
        stop("No SRA_library_strategy provided")
    } else if (is.na(SRA_library_strategy)){
        stop("No SRA_library_strategy provided")
    } else if (SRA_library_strategy == "") {
        stop("No SRA_library_strategy provided")
    } else if (length(SRA_library_strategy) != 1 ) {
        stop("Only one SRA_library_strategy can be supported at any given time")
    } else if (!(SRA_library_strategy %in% supported_SRA_library_strategy)){
        sls <- paste(supported_SRA_library_strategy, collapse = ", ")
        warning(paste0("Library strategy does not belong to the recommended options. If you do not get satisfying results, please try one of: ", sls, ". Alternatively, check your options with getDatabaseInformation()"))
    }
    
    
    
    
    
    
    #Check validity of SRA_secondary_library_strategy (may be NULL, BUT not NA, not "", all elements belong to the list)
    if (!is.null(SRA_secondary_library_strategy)){
        if (is.na(SRA_secondary_library_strategy)){
            stop("No SRA_secondary_library_strategy provided")
        } else if (SRA_secondary_library_strategy == "") {
            stop("No SRA_secondary_library_strategy provided")
        } else {
            for (l in seq_along(SRA_secondary_library_strategy)){
                if (!(SRA_secondary_library_strategy[[l]] %in% supported_SRA_secondary_library_strategy)) {
                    sls <- paste(supported_SRA_secondary_library_strategy, collapse = ", ")
                    stop(paste0("Library strategy does not belong) to the list of supported library strategies. Please try one of: ", sls, "."))
                }
            }
        }
    }
    
    #Warning message if SRA_secondary_library_strategy not in canonical form
    if (!is.null(SRA_secondary_library_strategy)){
        if( !manageLibraryStrategy(SRA_secondary_library_strategy, task="check_can") ){
            warning(library_warning_message)
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
    mm("SEARCH CONDITIONS SUMMARY", "search")
    mm(paste0("Selected gene: ", paste(gene, collapse = " OR ")), "search")
    mm(paste0("Selected antibody: ", paste(antibody, collapse = " OR ")), "search")
    mm(paste0("Selected cell type: ", paste(cell_type, collapse = " OR ")), "search")
    mm(paste0("Selected treatment: ", paste(treatment, collapse = " OR ")), "search")
    mm(paste0("Selected species: ", paste(species, collapse = " OR ")), "search")
    mm(paste0("Selected SRA_library_strategy: ", paste(SRA_library_strategy, collapse = " OR ")), "search")
    mm(paste0("Selected platform: ", paste(platform, collapse = " OR ")), "search")
    mm(paste0("Selected SRA_secondary_library_strategy: ", paste(SRA_secondary_library_strategy, collapse = " OR ")), "search")
    
    
    #============================================================================
    
    
    
    
    
    #============================================================================
    # Save search terms as a list
    #============================================================================
    st <- list(SRA_library_strategy=SRA_library_strategy, gene=gene, antibody=antibody, cell_type=cell_type, treatment=treatment, species=species, platform=platform, SRA_secondary_library_strategy=SRA_secondary_library_strategy)
    #============================================================================
    
    
    #============================================================================
    # Save search parameters and call details in a file
    #============================================================================
    parameterRecordGenerator(st = st, file = do.call(filenameGenerator, c(st, list(output="PAR"), list(file_type="tab"))), fun_name = "searchForTerm")
    
    callRecordGenerator(file = do.call(filenameGenerator, c(st, list(output="CALL"), list(file_type="Rda"))))
    #============================================================================
    
    
    
    #============================================================================
    # Find entries containing search_terms (sample_list)
    #============================================================================
    
    sample_list <- do.call(searchSRA, st[-grep("SRA_secondary_library_strategy", names(st))])
    #============================================================================
    
    
    #============================================================================
    # Add input and control columns
    #============================================================================
    if (st$SRA_library_strategy == "ChIP-Seq"){
        sample_list$input <- "N"
    } else {
        sample_list$input <- NA
    }
    
    if (st$SRA_library_strategy == "ChIP-Seq"){
        sample_list$control <- NA
    } else {
        sample_list$control <- "N" # Add 'N' for all non-ChIP library strategies (including RNA-Seq) 
    }
    #============================================================================
    
    #saveRDS(sample_list, "sample_list.Rda")
    
    
    
    #============================================================================
    # For all the projects from the sample list, find all the corresponding SRRs
    #============================================================================
    #all_list <- searchForSRPChildren(unique(sample_list$study_accession), sra_columns)
    all_list <- searchForSRPChildren(unique(sample_list$study_accession), "*") #Changed to avoid problems with column management
    all_list$input <- NA
    all_list$control <- NA
    #============================================================================
    
    
    
    
    
    #============================================================================
    # Combine the sample_list with the list of all SRRs
    # Mark SRRs from sample_list with "N" and the remaining SRRs with "check"
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
    # Extract GSMs from the experiment_title
    #============================================================================
    spider_combined <- extractGSM(spider_combined)
    #============================================================================
    
    
    
    
    
    #============================================================================
    # Extract SRA sample attributes
    #============================================================================
    spider_combined <- saExtractor(spider_combined)
    #============================================================================
    
    
    
    
    #============================================================================
    # Detect inputs (ChIP) and controls (RNA)
    #============================================================================
    spider_combined <- detectInputs(spider_combined) #Detect ChIP-Seq inputs
    
    #spider_combined$rna_control <- NA #Add new column
    spider_combined <- detectControls(spider_combined) #Detect RNA-Seq controls
    #============================================================================
    
    
    
    #============================================================================
    # Add columns for sample sheets (lane and merge* (will label it mer to avoid interference with merge function))
    #============================================================================
    spider_combined <- detectMerges(spider_combined)
    verifyMissingRuns(spider_combined$run_accession) #Check if there are any missing runs ===*=== Disabled (dbSendQuery stopped working!)
    #============================================================================
    
    
    #============================================================================
    # Add pairedEnd column
    #============================================================================
    spider_combined <- convertPairedEnds(spider_combined)
    #============================================================================
    
    
    .GlobalEnv$spider_output_combined_no_geo <- spider_combined
    
    #============================================================================
    # Search for entries in GEO
    #============================================================================
    #-------------------------
    #Inputs
    gsm_db_name <- "geo_con"
    database_env <- ".GlobalEnv"
    #gsm_columns <- c("gsm", "series_id", "gpl", "title", "source_name_ch1", "organism_ch1", "characteristics_ch1")
    gsm_columns <- "*"
    
    
    
    #gse_columns <- c("title", "pubmed_id")
    #gse_columns <- c("pubmed_id")
    gse_columns <- "*"
    
    
    #gsm_list <- test12[1:10, 2]
    #gsm_list <- c("GSM2342088")
    #gsm_list <- c("GSM2342088", "GSM2140962")
    
    gsm_list <- spider_combined$gsm #Get GSMs from column created by extractGSM() # sampletogsm ===*===
    gsm_list <- unique(gsm_list[!is.na(gsm_list)]) #Leave only unique, non-na entries
    #-------------------------
    
    #spider_geo <- geoFinder(get(gsm_db_name, envir = get(database_env)), gsm_list = gsm_list, gsm_columns = gsm_columns, gse_columns = gse_columns)
    if (length(gsm_list)>0){
        spider_geo <- searchGEOForGSM(gsm_list, geo_columns = gsm_columns, gse_columns = gse_columns)
        
        #============================================================================
        # Extract characteristics_ch1 into separate columns
        #============================================================================
        spider_geo <- chExtractor(spider_geo)
        #============================================================================
        
        #============================================================================
        # Merge spider_comibined and spider_geo
        #============================================================================
        spider_combined <- merge(spider_combined, spider_geo, by.x = "gsm", by.y = "gsm", all.x = TRUE) # by.x sampletogsm ===*===
        
        #saveRDS(spider_combined, "spider_combined_prelim.Rda")
        
        spider_superseries <- verifySuperseries(spider_combined$series_id) #Give info on superseries
        #============================================================================
        
        
    } else {
        if (length(gsm_columns)==1 & gsm_columns[1] == "*" & length(gse_columns)==1 & gse_columns[1]== "*"){
            columns_to_add <- as.character(unlist(listValidColumns()[c("GSM", "GSE")]))
            spider_combined[, columns_to_add] <- NA
            spider_combined <- chExtractor(spider_combined)
            spider_superseries <- NULL
        } else {
            stop("No results in GEO. No appropriate ways to deal with this") # ===*===
        }
    }
    
    #.GlobalEnv$temp_spider_geo <- spider_geo
    
    
    #============================================================================
    
    
    
    
    
    
    
    
    #============================================================================
    # Rename SRA and OTH columns, check if all valid
    #============================================================================
    #print(colnames(spider_combined)) #===*===
    if ("sra_ID" %in% colnames(spider_combined)){
        spider_combined <- spider_combined[, -grep("sra_ID", colnames(spider_combined))]
    }
    
    spider_combined <- renameSRAColumns(spider_combined)
    spider_combined <- renameOTHColumns(spider_combined)
    checkValidColumns(spider_combined)
    #============================================================================
    
    
    #============================================================================
    # Generate outputs
    #============================================================================
    
    outputGenerator(spider_combined, spider_superseries, st = st)
    #============================================================================
    
    if (return_all == FALSE){
        
        
        if (st$SRA_library_strategy =="ChIP-Seq"){
            #======
            #ChIP
            #======
            spider_combined <- dplyr::filter(spider_combined, SRA_library_strategy == st$SRA_library_strategy & OTH_input %in% c("N", "input"))
        } else if (st$SRA_library_strategy =="RNA-Seq"){
            #======
            #RNA
            #======
            spider_combined <- dplyr::filter(spider_combined, SRA_library_strategy == st$SRA_library_strategy & OTH_control %in% c("N", "control", "otherwise"))
        } else {
            #======
            #ELSE
            #======
            spider_combined <- dplyr::filter(spider_combined, SRA_library_strategy == st$SRA_library_strategy & OTH_control %in% c("N"))
        }
        
        
    }
    
    return(spider_combined)
    
    
}
