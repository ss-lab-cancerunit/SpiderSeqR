
#Output_Functions.R

#Stores functions necessary for output generation (with the exception of spider-wide functions like columnVerifier)

#- outputGenerator() - main function
#- filenameGenerator() - for generating file names
#- orderAccessions() - for sorting by _R_s (ignoring the letter prefix) - previously digitSort

#- universalSampleSheetGenerator() - layout for sample sheets
#    USES:
#     - chipSampleSheetGenerator()
#     - rnaSampleSheetGenerator()
#     - otherSampleSheetGenerator()
#     (all included here)
#
#- dbExtractGenerator() - layout for db extracts
#- columnSelector() - main powerhorse of dbExtractGenerator()


#============================================================================
# outputGenerator()
#============================================================================

#Developed in outputGenerator.R
outputGenerator <- function(df, ss=NULL, st){
  # Args: df - data frame to output (unfilter)
  #       ss - list of series (GSE) from superseriesVerifier
  #       st - search terms (used for conditional filtering and file naming)
  # Returns: nothing
  #
  # Action: orders the df, replaces tabs (in characteristics_ch1), filters rows as appropriate, outputs files
  #
  # Creates the following files:
  # - TARGET - samples of interest with relevant inputs/controls (ChIP/RNA)
  # - ALL - all samples within SRP (that belong to the SRA_library_strategy) - only in the case of ChIP/RNA
  # - SECONDARY - samples that belong to SRA_secondary_library_strategy - only for ChIP with specified library_strategy
  #
  # All the files are made available as sample sheets (.csv and .Rda) and database extracts (.csv, .Rda)
  #
  print("Running outputGenerator")
  
  spiderSet <- list(internal = TRUE, extract_columns = listColumnSets()$Overview) # ENV ===*===

  #ORDER BY SRP, then SRS,  then SRX, then SRR
  order_columns <- list(df$study_accession,
                        df$sample_accession,
                        df$experiment_accession,
                        df$run_accession)

  #Using custom function for ordering to disregard the '_RP/_RS/_RX/_RR' prefixes
  df <- df[orderAccessions(order_columns),]


  #Converting tabs in characteristics_ch1 column
  if (sum(grepl("GSM_characteristics_ch1", colnames(df)))==1){
    df$GSM_characteristics_ch1 <- unlist(lapply(df$GSM_characteristics_ch1, function(x) gsub(";\t", " \\|\\| ", x)))
  } else {
    df$characteristics_ch1 <- unlist(lapply(df$characteristics_ch1, function(x) gsub(";\t", " \\|\\| ", x)))
  }

  
  
  if (!is.null(ss)){
    saveRDS(ss, do.call(filenameGenerator, c(st, list(output="ss"), list(file_type="Rda"))))
  }


  if (st$SRA_library_strategy =="ChIP-Seq"){
    #======
    #ChIP
    #======
    target <- dplyr::filter(df, SRA_library_strategy == st$SRA_library_strategy & OTH_input %in% c("N", "input"))
  } else if (st$SRA_library_strategy =="RNA-Seq"){
    #======
    #RNA
    #======
    target <- dplyr::filter(df, SRA_library_strategy == st$SRA_library_strategy & OTH_control %in% c("N", "control", "otherwise"))
  } else {
    #======
    #ELSE
    #======
    target <- dplyr::filter(df, SRA_library_strategy == st$SRA_library_strategy & OTH_control %in% c("N"))
  }


  #=========================
  # 'TARGET' sheet (all samples and inputs/controls, as appropriate)
  #=========================

  if (spiderSet$internal == TRUE){
    #GENERATE TARGET SAMPLE SHEET
    target_sample_sheet <- universalSampleSheetGenerator(df=target, SRA_library_strategy = st$SRA_library_strategy)
    
    #SAVE TARGET SAMPLE SHEET
    saveRDS(target_sample_sheet, do.call(filenameGenerator, c(st, list(output="tar_sm"), list(file_type="Rda"))))
    cwt(target_sample_sheet, do.call(filenameGenerator, c(st, list(output="tar_sm"), list(file_type="csv"))))
    
  }

  #GENERATE TARGET DB EXTRACT
  target_db_extract <- dbExtractGenerator(target)

  #SAVE TARGET DB EXTRACT
  saveRDS(target_db_extract, do.call(filenameGenerator, c(st, list(output="tar_db"), list(file_type="Rda"))))
  cwt(target_db_extract, do.call(filenameGenerator, c(st, list(output="tar_db"), list(file_type="csv"))))

  if (st$SRA_library_strategy %in% c("ChIP-Seq", "RNA-Seq")){
    #=========================
    # 'ALL' sheet (all entries within SRP that are from the specified SRA_library_strategy)
    #=========================
    all <- dplyr::filter(df, SRA_library_strategy == st$SRA_library_strategy)

    if (spiderSet$internal == TRUE){
      #GENERATE ALL SAMPLE SHEET
      all_sample_sheet <- universalSampleSheetGenerator(df=all, SRA_library_strategy = st$SRA_library_strategy)
      
      #SAVE ALL SAMPLE SHEET
      saveRDS(all_sample_sheet, do.call(filenameGenerator, c(st, list(output="all_sm"), list(file_type="Rda"))))
      cwt(all_sample_sheet, do.call(filenameGenerator, c(st, list(output="all_sm"), list(file_type="csv"))))
    }


    #GENERATE ALL DB EXTRACT
    all_db_extract <- dbExtractGenerator(all)

    #SAVE ALL DB EXTRACT
    saveRDS(all_db_extract, do.call(filenameGenerator, c(st, list(output="all_db"), list(file_type="Rda"))))
    cwt(all_db_extract, do.call(filenameGenerator, c(st, list(output="all_db"), list(file_type="csv"))))

    if (st$SRA_library_strategy == "ChIP-Seq" & !is.null(st$SRA_secondary_library_strategy)){
      #=========================
      # 'SECONDARY' sheet (all entries within SRP that are from the SRA_secondary_library_strategy)
      #    ------ONLY AVAILABLE FOR CHIP------
      #=========================

      secondary <- dplyr::filter(df, SRA_library_strategy == st$SRA_secondary_library_strategy)

      if (spiderSet$internal == TRUE){
        
        #GENERATE SECONDARY SAMPLE SHEET
        secondary_sample_sheet <- universalSampleSheetGenerator(df=secondary, SRA_library_strategy = st$SRA_secondary_library_strategy)
        
        #SAVE SECONDARY SAMPLE SHEET
        saveRDS(secondary_sample_sheet, do.call(filenameGenerator, c(st, list(output="2ry_sm"), list(file_type="Rda"))))
        cwt(secondary_sample_sheet, do.call(filenameGenerator, c(st, list(output="2ry_sm"), list(file_type="csv"))))
      
      }  

      #GENERATE SECONDARY DB EXTRACT
      secondary_db_extract <- dbExtractGenerator(secondary)

      #SAVE SECONDARY DB EXTRACT
      saveRDS(secondary_db_extract, do.call(filenameGenerator, c(st, list(output="2ry_db"), list(file_type="Rda"))))
      cwt(secondary_db_extract, do.call(filenameGenerator, c(st, list(output="2ry_db"), list(file_type="csv"))))
    }
  } else {
    #=========================
    # 'ALL' sheet (all entries within SRP FULL STOP!)
    #=========================
    all <- df # No filtering for library_strategy
    
    if (spiderSet$internal == TRUE){
      #GENERATE ALL SAMPLE SHEET
      all_sample_sheet <- universalSampleSheetGenerator(df=all, SRA_library_strategy = st$SRA_library_strategy)
      
      #SAVE ALL SAMPLE SHEET
      saveRDS(all_sample_sheet, do.call(filenameGenerator, c(st, list(output="all_sm"), list(file_type="Rda"))))
      cwt(all_sample_sheet, do.call(filenameGenerator, c(st, list(output="all_sm"), list(file_type="csv"))))
    }
    
    
    #GENERATE ALL DB EXTRACT
    all_db_extract <- dbExtractGenerator(all)
    
    #SAVE ALL DB EXTRACT
    saveRDS(all_db_extract, do.call(filenameGenerator, c(st, list(output="all_db"), list(file_type="Rda"))))
    cwt(all_db_extract, do.call(filenameGenerator, c(st, list(output="all_db"), list(file_type="csv"))))
    
  }
  print("outputGenerator completed")
}
#============================================================================




#============================================================================
# filenameGenerator()
#============================================================================

#Developed in outputGenerator.R
filenameGenerator <- function(SRA_library_strategy, gene=NULL, antibody=NULL, cell_type=NULL, treatment=NULL, species=NULL, platform=NULL, SRA_secondary_library_strategy=NULL, output, file_type){ #Same arguments as testf, as well as output and file_type

  print("Running filenameGenerator")

  argument_list <- c("SRA_library_strategy", "gene", "antibody", "cell_type", "treatment", "species", "platform", "SRA_secondary_library_strategy", "output") #file_type will be handled separately

  #Shorten SRA_library_strategy names whenever appropriate
  ls_substitute_list <- list()
  ls_substitute_list[[1]] <- c("RNA-Seq", "ChIP-Seq") #Original names
  ls_substitute_list[[2]] <- c("RNA", "ChIP") #New names

  for (s in seq_along(ls_substitute_list[[1]])){
    if (SRA_library_strategy == ls_substitute_list[[1]][s]){
      SRA_library_strategy <- ls_substitute_list[[2]][s]
    }
  }

  #Shorten SRA_secondary_library_strategy names
  for (s in seq_along(ls_substitute_list[[1]])){
    for (l in seq_along(SRA_secondary_library_strategy)){
      if (SRA_secondary_library_strategy[l] == ls_substitute_list[[1]][s]){
        SRA_secondary_library_strategy <- ls_substitute_list[[2]][s]
      }
    }
  }
  #Add '2' prefix
  SRA_secondary_library_strategy <- paste0("2", SRA_secondary_library_strategy)


  name <- character()
  for (arg in seq_along(argument_list)){
    chunk <- get(argument_list[arg])[[1]] #Only the first element will be used for naming
    if (!is.null(chunk)){
      if (nchar(chunk)>8){
        chunk <- substr(chunk, 1, 8) #Truncate long entries to max. 8 characters
      }
      name <- paste0(name, "_", chunk)
    }
  }

  #today <- Sys.Date()
  #today <- format(today, format = "%y%m%d")

  today <- Sys.time()
  today <- format(today, format = "%y%m%d-%H%M%S")

  name <- paste0(name, "_", today)

  name <- substr(name, 2, (nchar(name)))
  name <- paste0(name, ".", file_type) #file_type added separately at the end

  print("filenameGenerator completed")
  return(name)

}
#============================================================================



#============================================================================
# outputGenerator_acc()
#============================================================================

#Developed in outputGenerator.R
#Based largely on outputGenerator()
# Differences:
# - arguments (accession instead of st elements)
# - filtering method

outputGenerator_acc <- function(df, ss=NULL, accession){
  # Args: df - data frame to output (unfilter)
  #       ss - list of series (GSE) from superseriesVerifier
  #       accession - accession for which search was undertaken
  # Returns: nothing
  #
  # Action: orders the df, replaces tabs (in characteristics_ch1), filters rows as appropriate, outputs files
  #
  # Creates the following files:
  # - ChIP-Seq - if present
  # - RNA-Seq - if present
  # - OTHER - if present
  #
  #
  # All the files are made available as sample sheets (.csv and .Rda) and database extracts (.csv, .Rda)
  #

  print("Running outputGenerator_acc")

  #ORDER BY SRP, then SRS,  then SRX, then SRR
  order_columns <- list(df$study_accession,
                        df$sample_accession,
                        df$experiment_accession,
                        df$run_accession)

  #Using custom function for ordering to disregard the '_RP/_RS/_RX/_RR' prefixes
  df <- df[orderAccessions(order_columns),]
  #df <- df[orderAccessions(order_columns),]

  
  #Converting tabs in characteristics_ch1 column
  if (sum(grepl("GSM_characteristics_ch1", colnames(df)))==1){
    df$GSM_characteristics_ch1 <- unlist(lapply(df$GSM_characteristics_ch1, function(x) gsub(";\t", " \\|\\| ", x)))
  } else {
    df$characteristics_ch1 <- unlist(lapply(df$characteristics_ch1, function(x) gsub(";\t", " \\|\\| ", x)))
  }
  


  if (!is.null(ss)){
    saveRDS(ss, do.call(filenameGenerator, c(st, list(output="ss"), list(file_type="Rda"))))
  }


  #============================================================================
  # Generate unfiltered outputs
  #============================================================================
  #GENERATE SAMPLE SHEET
  df_sample_sheet <- universalSampleSheetGenerator(df, "OTHER")

  #SAVE SAMPLE SHEET
  saveRDS(df_sample_sheet, filenameGenerator_acc(SRA_library_strategy = "ALL", accession = accession, output = "sm", file_type = "Rda"))
  cwt(df_sample_sheet, filenameGenerator_acc(SRA_library_strategy = "ALL", accession = accession, output = "sm", file_type = "csv"))

  #GENERATE DB EXTRACT
  df_db_extract <- dbExtractGenerator(df)

  #SAVE DB EXTRACT
  saveRDS(df_db_extract, filenameGenerator_acc(SRA_library_strategy = "ALL", accession = accession, output = "db", file_type = "Rda"))
  cwt(df_db_extract, filenameGenerator_acc(SRA_library_strategy = "ALL", accession = accession, output = "db", file_type = "csv"))
  #============================================================================




  #============================================================================
  # Filter by SRA_library_strategy
  #============================================================================
  #FILTER CHIP-SEQ DATA
  df_chip <- dplyr::filter(df, SRA_library_strategy == "ChIP-Seq")

  #FILTER RNA-SEQ DATA
  df_rna <- dplyr::filter(df, SRA_library_strategy == "RNA-Seq")

  #FILTER OTHER DATA
  df_other <- dplyr::filter(df, !(SRA_library_strategy %in% c("ChIP-Seq", "RNA-Seq")) )
  #============================================================================



  #============================================================================
  # Generate outputs and save ChIP samples
  #============================================================================
  #Check if empty
  if (dim(df_chip)[1]!=0){

    #GENERATE SAMPLE SHEET
    df_chip_sample_sheet <- universalSampleSheetGenerator(df_chip, "ChIP-Seq")

    #SAVE SAMPLE SHEET
    saveRDS(df_chip_sample_sheet, filenameGenerator_acc(SRA_library_strategy = "ChIP-Seq", accession = accession, output = "sm", file_type = "Rda"))
    cwt(df_chip_sample_sheet, filenameGenerator_acc(SRA_library_strategy = "ChIP-Seq", accession = accession, output = "sm", file_type = "csv"))

    #GENERATE DB EXTRACT
    df_chip_db_extract <- dbExtractGenerator(df_chip)

    #SAVE DB EXTRACT
    saveRDS(df_chip_db_extract, filenameGenerator_acc(SRA_library_strategy = "ChIP-Seq", accession = accession, output = "db", file_type = "Rda"))
    cwt(df_chip_db_extract, filenameGenerator_acc(SRA_library_strategy = "ChIP-Seq", accession = accession, output = "db", file_type = "csv"))

  }

  #============================================================================




  #============================================================================
  # Generate outputs and save RNA samples
  #============================================================================

  #Check if empty
  if (dim(df_rna)[1]!=0){

    #GENERATE SAMPLE SHEET
    df_rna_sample_sheet <- universalSampleSheetGenerator(df_rna, "RNA-Seq")

    #SAVE SAMPLE SHEET
    saveRDS(df_rna_sample_sheet, filenameGenerator_acc(SRA_library_strategy = "RNA-Seq", accession = accession, output = "sm", file_type = "Rda"))
    cwt(df_rna_sample_sheet, filenameGenerator_acc(SRA_library_strategy = "RNA-Seq", accession = accession, output = "sm", file_type = "csv"))

    #GENERATE DB EXTRACT
    df_rna_db_extract <- dbExtractGenerator(df_rna)

    #SAVE DB EXTRACT
    saveRDS(df_rna_db_extract, filenameGenerator_acc(SRA_library_strategy = "RNA-Seq", accession = accession, output = "db", file_type = "Rda"))
    cwt(df_rna_db_extract, filenameGenerator_acc(SRA_library_strategy = "RNA-Seq", accession = accession, output = "db", file_type = "csv"))

  }

  #============================================================================



  #============================================================================
  # Generate outputs and save OTHER samples
  #============================================================================
  #Check if empty
  if (dim(df_other)[1]!=0){

    #GENERATE SAMPLE SHEET
    df_other_sample_sheet <- universalSampleSheetGenerator(df_other, "OTHER")

    #SAVE SAMPLE SHEET
    saveRDS(df_other_sample_sheet, filenameGenerator_acc(SRA_library_strategy = "OTHER", accession = accession, output = "sm", file_type = "Rda"))
    cwt(df_other_sample_sheet, filenameGenerator_acc(SRA_library_strategy = "OTHER", accession = accession, output = "sm", file_type = "csv"))

    #GENERATE DB EXTRACT
    df_other_db_extract <- dbExtractGenerator(df_other)

    #SAVE DB EXTRACT
    saveRDS(df_other_db_extract, filenameGenerator_acc(SRA_library_strategy = "OTHER", accession = accession, output = "db", file_type = "Rda"))
    cwt(df_other_db_extract, filenameGenerator_acc(SRA_library_strategy = "OTHER", accession = accession, output = "db", file_type = "csv"))

  }

  #============================================================================







  print("outputGenerator_acc completed")
}
#============================================================================




#============================================================================
# filenameGenerator_acc()
#============================================================================
#Developed in outputGenerator.R
#Based largely on filenameGenerator
#Differences:
# - arguments
# - order (accession before library strategy)
# - number of letters allowed (12 instead of 8)

filenameGenerator_acc <- function(SRA_library_strategy, accession, output, file_type){

  print("Running filenameGenerator_acc")

  argument_list <- c("accession", "SRA_library_strategy", "output") #file_type will be handled separately

  #Shorten the SRA_library_strategy
  SRA_library_strategy <- manageLibraryStrategy(SRA_library_strategy, input = "can", output = "short", mismatch.ignore = TRUE)

  #Combine together the arguments and truncate if necessary
  name <- character()
  for (arg in seq_along(argument_list)){
    chunk <- get(argument_list[arg])[[1]] #Only the first element will be used for naming
    if (!is.null(chunk)){
      if (nchar(chunk)>8){
        chunk <- substr(chunk, 1, 12) #Truncate long entries to max. 12 characters
      }
      name <- paste0(name, "_", chunk)
    }
  }

  today <- Sys.time()
  today <- format(today, format = "%y%m%d-%H%M%S")

  name <- paste0(name, "_", today)

  name <- substr(name, 2, (nchar(name)))
  name <- paste0(name, ".", file_type) #file_type added separately at the end

  print("filenameGenerator_acc completed")
  return(name)

}

#============================================================================





#============================================================================
# orderAccessions()
#============================================================================

#' Order Accessions
#' 
#' \code{orderAccessions} orders character vectors or lists according to numbers, disregarding any lettrs.
#' 
#' @param x Character vector (or list containing character vectors) to be ordered
#' @param na.last For controlling the treatment of NAs (see \code{\link[base]{order}})
#' @return Integer vector with order of indices
#' 
#' @examples 
#' \dontrun{
#' # Order rows in df according to multiple accession types
#' order_columns <- list(df$study_accession, df$sample_accession, df$experiment_accession, df$run_accession, df$gsm)
#' df <- df[orderAccessions(order_columns), ]
#' }
#' 
#' @seealso \code{\link[base]{order}}
#' 
#' @export
#' 
#' 
orderAccessions <- function(x, na.last = TRUE){
  # Steps (separate tracks for lists and vectors):
  # - warning if not just alphanumeric (correct for NAs). NOTE: can contain commas
  # - identify elements which contain commas
  # - for comma entries:
  #      - separate by commas
  #      - remove letters
  #      - convert to numeric
  #      - find the smallest number
  # - for non-comma entries: remove alpha
  # - sort by numeric
  
  print("Running orderAccessions")
  
  if (is.list(x)){ #Input is a list
    
    x_num <- list()
    
    for (i in seq_along(x)){
      
      
      #n_nas <- sum(is.na(x[[i]])) # NOTE: NAs do not give TRUE on grepl below
      ##if ( sum(grepl("^[[:alnum:]]*$", x[[i]])) != (length(x[[i]]) - n_nas) ){
      #if ( sum(grepl("^[a-zA-Z0-9,]*$", x[[i]])) != (length(x[[i]]) - n_nas) ){ # Relaxed constraints to include comma
      #  stop("Only alphanumeric characters are allowed")
      #}
      
      checkCondition("^[a-zA-Z0-9,]*$", x[[i]])
      
      x_num[[i]] <- rep(NA, length(x[[i]])) #Initialise the subset of the list with a correct length
      
      comma_ind <- grepl(",", x[[i]])
      #print(comma_ind)
      
      #Special treatment for entries with commas (superseries)
      for (j in seq_along(x[[i]])){
        if (comma_ind[j]){
          a <- x[[i]][j] #Get the current entry
          #print(a)
          a <- unlist(strsplit(a, split = ",")) #Now a vector (split by commas)
          #print(a)
          
          checkCondition("^[[:alnum:]]*$", a)
          
          a <- as.numeric(gsub("[[:alpha:]]", "", a)) #Remove letters
          #print(a)
          a <- min(a) #Set as minimum
          
          #print(a)
          x_num[[i]][j] <- a
          #print(a)
        }
        
      }
      
      
      
      #Remove alphanumeric characters and convert remainder to numeric
      x_num[[i]][!comma_ind] <- as.numeric(gsub("[[:alpha:]]", "", x[[i]][!comma_ind]))
      
    }
    
    ord <- do.call(order, c(x_num, na.last = na.last))
    
  } else { #Input is not a list
    
    
    
    #n_nas <- sum(is.na(x)) # NOTE: NAs do not give TRUE on grepl below
    ##if ( sum(grepl("^[[:alnum:]]*$", x)) != (length(x) - n_nas) ){
    #if ( sum(grepl("^[a-zA-Z0-9,]*$", x)) != (length(x) - n_nas) ){  
    #  stop("Only alphanumeric characters are allowed")
    #}
    
    checkCondition("^[a-zA-Z0-9,]*$", x)
    
    
    x_num <- rep(NA, length(x)) #Initialise the subset of the list with a correct length
    
    comma_ind <- grepl(",", x)
    #print(comma_ind)
    
    #Special treatment for entries with commas (superseries)
    for (j in seq_along(x)){
      if (comma_ind[j]){
        a <- x[j] #Get the current entry
        #print(a)
        a <- unlist(strsplit(a, split = ",")) #Now a vector (split by commas)
        #print(a)
        
        checkCondition("^[[:alnum:]]*$", a)
        
        a <- as.numeric(gsub("[[:alpha:]]", "", a)) #Remove letters
        #print(a)
        a <- min(a) #Set as minimum
        
        #print(a)
        x_num[j] <- a
        #print(a)
      }
      
    }
    
    
    #Remove alphanumeric characters and convert remainder to numeric
    x_num[!comma_ind] <- as.numeric(gsub("[[:alpha:]]", "", x[!comma_ind]))
    
    
    
    ord <- do.call(order, list(x_num, na.last = na.last))
    
  }
  
  print("orderAccessions completed")
  
  return(ord)
  
}

#============================================================================




#============================================================================
# checkCondition()
#============================================================================
#' Check Condition
#' 
#' \code{checkCondition} verifies a condition (using grepl), ignoring any NA elements in a vector
#' 
#' @param x Character vector (or list containing character vectors) to be checked
#' @param condition String with a regular expression to be checked
#' @param message Error message to be displayed (not required)
#' @return Nothing. Produces an error message if condition is not fulfilled in all vector elements
#' 
#' @keywords internal
#' 
#' 
checkCondition <- function(condition, x, message){
  
  n_nas <- sum(is.na(x))
  
  #print(sum(grepl(condition, x)))
  #print((length(x)-n_nas))
  
  if (sum(grepl(condition, x)) != (length(x)-n_nas)){
    if (missing(message)){
      stop("The string contains forbidden characters")
    } else {
      stop(message)
    }
  }
  
}

#============================================================================



#============================================================================
# universalSampleSheetGenerator()
#============================================================================
#Developed in sampleSheetGenerator.R
universalSampleSheetGenerator <- function(df, SRA_library_strategy){
  # Selects correct sample sheet generating function, depending on the SRA_library_strategy
  # ===*=== Consider having a list of acceptable strategies and testing if input is valid
  print("Running universalSampleSheetGenerator")

  if (SRA_library_strategy == "ChIP-Seq"){
    sample_sheet <- chipSampleSheetGenerator(df)
  } else if (SRA_library_strategy == "RNA-Seq"){
    sample_sheet <- rnaSampleSheetGenerator(df)
  } else {
    #If library strategy IS NOT chip or rna
    sample_sheet <- otherSampleSheetGenerator(df)
  }
  print("universalSampleSheetGenerator completed")
  return(sample_sheet)
}
#============================================================================


#============================================================================
# chipSampleSheetGenerator()
#============================================================================
chipSampleSheetGenerator <- function(df){
  # Arg: df from which sample sheet will be generated
  # Returns: sample_sheet (format compatible with the pipeline)
  print("Running chipSampleSheetGenerator")

  #List of required columns
  required_columns <- c("run_accession",
                        "study_accession",
                        "OTH_sa_tissue",
                        "SRA_experiment_title",
                        "OTH_lane",
                        "OTH_mer",
                        "OTH_input",
                        "OTH_pairedEnd")

  #Check if required columns exist within the data frame
  columnVerifier(df, required_columns)

  #Create the sample_sheet
  sample_sheet <- data.frame(ID=df[ , c("run_accession")])
  #sample_sheet$ID <- df[ , c("run_accession")]
  sample_sheet$fileName <- ""
  sample_sheet$fileNamePE <- ""
  sample_sheet$experiment <- df[ , c("study_accession")]
  #sample_sheet$experiment <- df[ , c("series_id")] #Previously used column
  sample_sheet$tissue <- df[ , c("OTH_sa_tissue")]
  #sample_sheet$tissue <- df[ , c("source_name_ch1")] #Another option
  sample_sheet$condition <- df[, c("SRA_experiment_title")]
  sample_sheet$replicate <- ""
  sample_sheet$lane <- df[ , c("OTH_lane")]
  sample_sheet$merge <- df[ , c("OTH_mer")]
  sample_sheet$input <- df[ , c("OTH_input")]
  sample_sheet$pairedEnd <- df[ , c("OTH_pairedEnd")]
  sample_sheet$macsGroup <- ""
  sample_sheet$phredScore <- ""
  sample_sheet$adapter <- ""
  sample_sheet$trimQuality <- ""
  sample_sheet$adapterPE <- ""
  sample_sheet$trimQualityPE <- ""

  print("chipSampleSheetGenerator completed")
  return(sample_sheet)
}
#============================================================================



#============================================================================
# rnaSampleSheetGenerator()
#============================================================================
rnaSampleSheetGenerator <- function(df){
  # Arg: df from which sample sheet will be generated
  # Returns: sample_sheet (format compatible with the pipeline)

  # In comparison to ChIP: does not have tissue, input and macsGroup columns
  print("Running rnaSampleSheetGenerator")

  #List of required columns
  required_columns <- c("run_accession",
                        "study_accession",
                        "SRA_experiment_title",
                        "OTH_lane",
                        "OTH_mer",
                        "OTH_pairedEnd")

  #Check if required columns exist within the data frame
  columnVerifier(df, required_columns)

  #Create the sample_sheet
  sample_sheet <- data.frame(ID=df[ , c("run_accession")])
  #sample_sheet$ID <- df[ , c("run_accession")]
  sample_sheet$fileName <- ""
  sample_sheet$fileNamePE <- ""
  sample_sheet$experiment <- df[ , c("study_accession")]
  #sample_sheet$experiment <- df[ , c("series_id")] #Previously used column

  #sample_sheet$tissue <- df[ , c("source_name_ch1")] #Another option
  sample_sheet$condition <- df[, c("SRA_experiment_title")]
  sample_sheet$replicate <- ""
  sample_sheet$lane <- df[ , c("OTH_lane")]
  sample_sheet$merge <- df[ , c("OTH_mer")]

  sample_sheet$pairedEnd <- df[ , c("OTH_pairedEnd")]

  sample_sheet$phredScore <- ""
  sample_sheet$adapter <- ""
  sample_sheet$trimQuality <- ""
  sample_sheet$adapterPE <- ""
  sample_sheet$trimQualityPE <- ""

  print("rnaSampleSheetGenerator completed")
  return(sample_sheet)
}
#============================================================================



#============================================================================
# otherSampleSheetGenerator()
#============================================================================
otherSampleSheetGenerator <- function(df){
  # Arg: df from which sample sheet will be generated
  # Returns: sample_sheet (format compatible with the pipeline)

  # Currently same as rnaSampleSheetGenerator
  # In comparison to ChIP: does not have tissue, input and macsGroup columns

  print("Running otherSampleSheetGenerator")

  #List of required columns
  required_columns <- c("run_accession",
                        "study_accession",
                        "SRA_experiment_title",
                        "OTH_lane",
                        "OTH_mer",
                        "OTH_pairedEnd")

  #Check if required columns exist within the data frame
  columnVerifier(df, required_columns)

  #Create the sample_sheet
  sample_sheet <- data.frame(ID=df[ , c("run_accession")])
  #sample_sheet$ID <- df[ , c("run_accession")]
  sample_sheet$fileName <- ""
  sample_sheet$fileNamePE <- ""
  sample_sheet$experiment <- df[ , c("study_accession")]
  #sample_sheet$experiment <- df[ , c("series_id")] #Previously used column

  #sample_sheet$tissue <- df[ , c("source_name_ch1")] #Another option
  sample_sheet$condition <- df[, c("SRA_experiment_title")]
  sample_sheet$replicate <- ""
  sample_sheet$lane <- df[ , c("OTH_lane")]
  sample_sheet$merge <- df[ , c("OTH_mer")]

  sample_sheet$pairedEnd <- df[ , c("OTH_pairedEnd")]

  sample_sheet$phredScore <- ""
  sample_sheet$adapter <- ""
  sample_sheet$trimQuality <- ""
  sample_sheet$adapterPE <- ""
  sample_sheet$trimQualityPE <- ""

  print("otherSampleSheetGenerator completed")
  return(sample_sheet)
}
#============================================================================





#============================================================================
#dbExtractGenerator()
#============================================================================

#Developed in dbExtractGenerator.R
#============================================================================
dbExtractGenerator <- function(df){
  print("Running dbExtractGenerator")
  
  
  if (is.null(getSpideROption("output_columns"))){  # output_columns is null - default setting
    internal_pre <- getSpideROption("internal")
    setSpideROption("internal", TRUE)
    df_columns <- listColumnSets()$dbExtract
    setSpideROption("internal", internal_pre)
  } else { # output_columns user-defined
    df_columns <- getSpideROption("output_columns")
  }
  
  


  #Select new column names
  db_extract_columns <- df_columns

  #Check if the specified columns exist within the df
  #Not needed, because already done by columnSelector
  #columnVerifier(df, df_columns)

  #Create an extract
  db_extract <- columnSelector(df, df_columns = df_columns, out_columns = db_extract_columns)

  print("dbExtractGenerator completed")
  return(db_extract)

}

#============================================================================



#============================================================================
# columnSelector()
#============================================================================


#Developed in dbExtractGenerator.R
columnSelector <- function(df, df_columns, out_columns){

  print("Running columnSelector")

  columnVerifier(df, df_columns) #Check if all specified columns exist within the df

  if (length(df_columns)!=length(out_columns)){
    stop("Df_columns and out_columns need to be the same length")
  }

  out <- data.frame(matrix(nrow=dim(df)[1])) #Initialise the df
  where <- environment()

  for (n in seq_along(df_columns)){
    #print(select(df, eval(df_columns[n])))
    where[["out"]][out_columns[n]] <- dplyr::select(df, eval(df_columns[n]))
  }

  out <- out[,-1]
  print("columnSelector completed")
  return(out)
}

#============================================================================



#============================================================================
# Column Sets and Selection ####
# Functions around columnSelector()
#============================================================================

#' Select columns from a data frame
#' 
#' @param df Data frame
#' @param cols Character vector with column names to be retained
#' @return Original data frame containing only columns specified in cols
#' 
#' @description 
#' This is a universal function for extracting only columns of interest from a data frame. Related functions exist, with pre-defined sets of columns, see documentation for \code{\link{selectColumnsAccn}} ===*===
#' 
#' 
#' @export
selectColumns <- function(df, cols){
  df <- columnSelector(df = df, df_columns = cols, out_columns = cols)
  return(df)
}



#' Sets of column names for use in display or filtering
#' 
#' @return List of character vectors with column names
#' 
#' @description 
#' List of sets of column names for use when subsetting results data frame. 
#' 
#' To access a list of all available columns, please see \code{listValidColumns()}.
#' 
#' @examples 
#' listColumnSets() # List all
#' listColumnSets()$Overview # Access the 'Overview' set of column names
#' listColumnSets()$Accession # Access the 'Accession' set of column names
#' names(listColumnSets()) # Get names of the available sets
#' 
#' @section Available sets:
#' 
#' \itemize{
#'     \item Accession - accession ids only (SRA and GEO)
#'     \item Overview - a subjective selection of most important columns (from SRA and GEO) for getting an overview of the samples
#'     \item Overview2 - columns from Overview with added columns of extracted information from SRA_sample_attribute and GSM_characteristics_ch1 columns
#' }
#' 
#' @section Applications:
#' 
#' The sets can be used for any purpose, though they have been created with display and filtering applications in mind. Here are recommended uses:
#' 
#' \itemize{
#'     \item Display: Accession, Overview, Overview2
#'     \item Filtering: ===*=== (in progress)
#'     
#' }
#' 
#' @export
#' 
listColumnSets <- function(){
  column_set <- list(
    Accession = c("run_accession", "experiment_accession", "sample_accession", "study_accession", "gsm", "series_id"),
    Overview = c("run_accession", "experiment_accession", "sample_accession", "study_accession", "gsm", "series_id", # Accn
                "SRA_library_strategy", "SRA_platform", "SRA_library_layout", "SRA_taxon_id",
                "SRA_sample_name", "SRA_experiment_title", "SRA_experiment_name", "GSM_title", "SRA_sample_attribute", "GSM_characteristics_ch1", "GSM_source_name_ch1"),
    Overview2 = c("run_accession", "experiment_accession", "sample_accession", "study_accession", "gsm", "series_id", # Accn
                  "SRA_library_strategy", "SRA_platform", "SRA_library_layout", "SRA_taxon_id",
                  "SRA_sample_name", "SRA_experiment_title", "SRA_experiment_name", "GSM_title", "SRA_sample_attribute", "GSM_characteristics_ch1", "GSM_source_name_ch1",       
                  "OTH_sa_tissue",
                  "OTH_ch1_tissue",
                  "OTH_sa_antibody",
                  "OTH_ch1_antibody",
                  "OTH_sa_gene",
                  "OTH_ch1_gene",
                  "OTH_sa_treatment",
                  "OTH_ch1_treatment")
  )
  
  
  # For internal use:
  if (getSpideROption("internal")==TRUE){
    
    column_set[[length(column_set)+1]] <- c("run_accession",
                                                        "experiment_accession",
                                                        "sample_accession",
                                                        "study_accession",
                                                        "gsm", #sampletogsm ===*===
                                                        "series_id",
                                                        "SRA_library_strategy",
                                                        "SRA_platform",
                                                        "SRA_library_layout",
                                                        "OTH_pairedEnd",
                                                        "SRA_taxon_id",
                                                        "SRA_sample_name",
                                                        "SRA_experiment_title",
                                                        "SRA_experiment_name",
                                                        "SRA_sample_attribute",
                                                        "GSM_characteristics_ch1",
                                                        "OTH_sa_tissue",
                                                        "OTH_ch1_tissue",
                                                        "OTH_sa_antibody",
                                                        "OTH_ch1_antibody",
                                                        "OTH_sa_gene",
                                                        "OTH_ch1_gene",
                                                        "OTH_sa_treatment",
                                                        "OTH_ch1_treatment"
    ) 
    
    names(column_set)[length(names(column_set))] <- "dbExtract"

  }
    
    
  return(column_set)
}



#' Select columns with accession information
#' 
#' @param df Data frame
#' @return Original data frame containing only columns with accession information
#' 
#' @description 
#' This is a shortcut function for \code{selectColumns(df, cols = listColumnSets()$Accession)}
#' 
#' @export
#' 
selectColumnsAccession <- function(df){
  cols <- listColumnSets()$Accession
  df <- columnSelector(df, df_columns = cols, out_columns = cols)
  return(df)
}


#' Select columns with overview of sample information
#' 
#' @param df Data frame
#' @return Original data frame containing only selected columns (most relevant for getting an overview about the samples)
#' 
#' @description 
#' This is a shortcut function for \code{selectColumns(df, cols = listColumnSets()$Overview)}
#' 
#' @export
#' 
selectColumnsOverview <- function(df){
  cols <- listColumnSets()$Overview
  df <- columnSelector(df, df_columns = cols, out_columns = cols)
  return(df)
}

#============================================================================
#============================================================================





#============================================================================
# cwt_prev
#============================================================================

# ===*=== TBD
cwt_prev <- function(object, filename){
  # Custom write table (a wrapper to unify the parameters)
  # Previous settings, which didn't facilitate automatic opening by Excel
  utils::write.table(x = object,
                     file = filename,
                     sep = "\t",
                     row.names = FALSE,
                     quote = FALSE
  )
}

#============================================================================






#============================================================================
# cwt
#============================================================================

cwt <- function(object, filename){
  # Custom write table (a wrapper to unify the parameters)
  utils::write.table(x = object,
                     file = filename,
                     sep = ";",
                     row.names = FALSE,
                     quote = TRUE
  )
}


#============================================================================


