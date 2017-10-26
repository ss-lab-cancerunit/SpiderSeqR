#SEARCH_FUNCTIONS.R
#Merged indev2.R script and testf function


#USAGE
#st <- list(library_strategy="ChIP-Seq", gene="STAT1", antibody="STAT1", secondary_library_strategy = "RNA-Seq")
#do.call(searchForTerm, st)


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

  supported_library_strategy <- c("ChIP-Seq", "RNA-Seq")
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
    stop(paste0("Library strategy does not belong to the list of supported library strategies. Please try one of: ", sls, "."))
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
          stop(paste0("Library strategy does not belong to the list of supported library strategies. Please try one of: ", sls, "."))
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
  #PREVIOUSLY
  #sra_attr_keywords <- list(c("tissue: ", "cell.line: ", "source.name: ", "cell.type: "),
  #                          c("antibody: "),
  #                          c("hgn: "),
  #                          c("treatment: "))
  #===*=== Make a better choice

  sra_tissue <- c("strain", "tissue", "source.?name", "isolation.?source", "isolate", "body.?site", "sample.?type", "cell.?type", "cell.?line", "ArrayExpress-CellType", "inferred.?cell.?type", "cell", "cre.?line", "cell.?description", "cell.?subtype", "cell.?or.?tisue.?type",
                  "ArrayExpress-StrainOrLine", "lineage", "line", "strain.?or.?line",
                  "body.?site", "site", "corrected.?sample.?site", "host.?body.?site",
                  "tissue.?type", "host.?tissue.?sampled", "tissue.?depot",
                  "source_material_id", "source",
                  "organism.?part") #TISSUE #ONLY USED HERE #RESEARCHED WELL
  sra_tissue <- paste0(sra_tissue, ": ")

  sra_antibody <- c("chip.antibody", "antibody", "ArrayExpress.Immunoprecipitate", "ip.antibody", "rip.antibody", "medip.antibody", "clip.antibody", "frip.antibody", "chip-seq.antibody") #ANTIBODY #RESEARCHED WELL
  sra_antibody <- paste0(sra_antibody, ": ")

  sra_gene <- c("genotype", "ArrayExpress.Genotype", "genotype/variation", "target.gene", "genetic.background", "host.genotype", "Plant.genotype", "genetic.modification", "transgene", "gene.id", "myd88.genotype", "gene.perturbation.type", "genetic.condition", "cytogenetics", "concise.genotype.name", "genspecies.abbr", "melanoma.genetic.conditions", "marker.gene", "gene", "strain/genotype", "genotype/variation", "knockout", "knockdown", "hgn") #GENE #RESEARCHED (based on sa_categories)
  sra_gene <- paste0(sra_gene, ": ")

  sra_treatment <- c("treatment", "ArrayExpress.Treatment", "treated.with", "treatment.description", "drug.treatment", "treatment.protocol", "Vaccine.Treatment", "experimental.treatment", "diet.treatment", "treatment.group") #TREATMENT #RESEARCHED
  sra_treatment <- paste0(sra_treatment, ": ")

  sra_attr_keywords <- list(sra_tissue, sra_antibody, sra_gene, sra_treatment)


  sra_sep_split <- " \\|\\| "
  sra_sep_collapse <- " || "

  spider_sra_attr <- ldply(spider_combined$sample_attribute, function(x) universalExtractor(x, sra_sep_split, sra_sep_collapse, sra_attr_keywords))

  colnames(spider_sra_attr) <- c("sa_original", "sa_remainder", "sa_tissue", "sa_antibody", "sa_gene", "sa_treatment")
  spider_combined <- cbind(spider_combined, spider_sra_attr[,(-1)]) #Combine extracted columns with df (except attr_original column)
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
  gsm_columns <- c("gsm", "series_id", "gpl", "title", "source_name_ch1", "organism_ch1", "characteristics_ch1")

  #gse_columns <- c("title", "pubmed_id")
  gse_columns <- c("pubmed_id")


  #gsm_list <- test12[1:10, 2]
  #gsm_list <- c("GSM2342088")
  #gsm_list <- c("GSM2342088", "GSM2140962")

  gsm_list <- spider_combined$sample #Get GSMs from column created by gsmExtractor()
  gsm_list <- unique(gsm_list[!is.na(gsm_list)]) #Leave only unique, non-na entries
  #-------------------------

  spider_geo <- geoFinder(gsm_db_name = gsm_db_name, gsm_list = gsm_list, gsm_columns = gsm_columns, gse_columns = gse_columns)

  #============================================================================



  #============================================================================
  #Extract characteristics_ch1 into separate columns
  #============================================================================

  #===*=== Come back and add more choices to the category names

  geo_tissue <- c("tissue", "cell.?type", "cell.?line") #NOT RESEARCHED
  geo_tissue <- paste0(geo_tissue, ": ")

  geo_antibody <- c("antibody") #NOT RESEARCHED
  geo_antibody <- paste0(geo_antibody, ": ")

  geo_gene <- c("genotype") #NOT RESEARCHED
  geo_gene <- paste0(geo_gene, ": ")

  geo_treatment <- c("treatment") #NOT RESEARCHED
  geo_treatment <- paste0(geo_treatment, ": ")

  geo_char_keywords <- list(geo_tissue, geo_antibody, geo_gene, geo_treatment)

  geo_sep_split <- ";\t"
  geo_sep_collapse <- ";\t"

  spider_geo_char <-  ldply(spider_geo$characteristics_ch1, function(x) universalExtractor(x, geo_sep_split, geo_sep_collapse, geo_char_keywords))

  colnames(spider_geo_char) <- c("ch1_original", "ch1_remainder", "ch1_tissue", "ch1_antibody", "ch1_gene", "ch1_treatment")
  spider_geo <- cbind(spider_geo, spider_geo_char[, (-1)]) #Combine extracted columns with geo df (except ch1_original column)
  #============================================================================


  #============================================================================
  #Merge spider_comibined and spider_geo
  #============================================================================
  spider_combined <- merge(spider_combined, spider_geo, by.x = "sample", by.y = "gsm", all.x = TRUE)
  superseriesVerifier(spider_combined$series_id) #Give info on superseries
  #============================================================================



  #============================================================================
  # Generate outputs
  #============================================================================
  outputGenerator(spider_combined, st = st)
  #============================================================================

  #return(spider_combined)


}
