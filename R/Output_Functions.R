
#Output_Functions.R

#Stores functions necessary for output generation (with the exception of spider-wide functions like columnVerifier)

#- outputGenerator() - main function
#- filenameGenerator() - for generating file names
#- digitSort() - for sorting by _R_s (ignoring the letter prefix)

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
outputGenerator <- function(df, st){
  # Args: df - data frame to output (unfilter)
  #       st - search terms (used for conditional filtering and file naming)
  # Returns: nothing
  #
  # Action: orders the df, replaces tabs (in characteristics_ch1), filters rows as appropriate, outputs files
  #
  # Creates the following files:
  # - TARGET - samples of interest with relevant inputs/controls (ChIP/RNA)
  # - ALL - all samples within SRP (that belong to the library_strategy) - only in the case of ChIP/RNA
  # - SECONDARY - samples that belong to secondary_library_strategy - only for ChIP with specified library_strategy
  #
  # All the files are made available as sample sheets (.csv and .Rda) and database extracts (.csv, .Rda)
  #
  
  #ORDER BY SRP, then SRS,  then SRX, then SRR
  order_columns <- list(df$study_accession,
                        df$sample_accession,
                        df$experiment_accession,
                        df$run_accession)
  
  #Using custom function for ordering to disregard the '_RP/_RS/_RX/_RR' prefixes
  df <- df[digitSort(order_columns),]
  
  #Converting tabs in characteristics_ch1 column
  df$characteristics_ch1 <- unlist(lapply(df$characteristics_ch1, function(x) gsub(";\t", " \\|\\| ", x)))
  
  
  cwt <- function(object, filename){
    # Custom write table (a wrapper to unify the parameters)
    write.table(x = object,
                file = filename,
                sep = "\t",
                row.names = FALSE,
                quote = FALSE
    )
  }
  
  
  if (st$library_strategy =="ChIP-Seq"){
    #======
    #ChIP
    #======
    target <- filter(df, library_strategy == st$library_strategy & input %in% c("N", "input"))
  } else if (st$library_strategy =="RNA-Seq"){
    #======
    #RNA
    #======
    target <- filter(df, library_strategy == st$library_strategy & control %in% c("N", "control", "otherwise"))
  } else {
    #======
    #ELSE
    #======
    target <- filter(df, library_strategy == st$library_strategy)
  }
  
  
  #=========================
  # 'TARGET' sheet (all samples and inputs/controls, as appropriate)
  #=========================
  
  #GENERATE TARGET SAMPLE SHEET
  target_sample_sheet <- universalSampleSheetGenerator(df=target, library_strategy = st$library_strategy)
  
  #SAVE TARGET SAMPLE SHEET
  saveRDS(target_sample_sheet, do.call(filenameGenerator, c(st, list(output="tar_sm"), list(file_type="Rda"))))
  cwt(target_sample_sheet, do.call(filenameGenerator, c(st, list(output="tar_sm"), list(file_type="csv"))))
  
  #GENERATE TARGET DB EXTRACT
  target_db_extract <- dbExtractGenerator(target)
  
  #SAVE TARGET DB EXTRACT
  saveRDS(target_db_extract, do.call(filenameGenerator, c(st, list(output="tar_db"), list(file_type="Rda"))))
  cwt(target_db_extract, do.call(filenameGenerator, c(st, list(output="tar_db"), list(file_type="csv"))))
  
  if (st$library_strategy %in% c("ChIP-Seq", "RNA-Seq")){
    #=========================
    # 'ALL' sheet (all entries within SRP that are from the specified library_strategy)
    #=========================
    all <- filter(df, library_strategy == st$library_strategy)
    
    #GENERATE ALL SAMPLE SHEET
    all_sample_sheet <- universalSampleSheetGenerator(df=all, library_strategy = st$library_strategy)
    
    #SAVE ALL SAMPLE SHEET
    saveRDS(all_sample_sheet, do.call(filenameGenerator, c(st, list(output="all_sm"), list(file_type="Rda"))))
    cwt(all_sample_sheet, do.call(filenameGenerator, c(st, list(output="all_sm"), list(file_type="csv"))))
    
    #GENERATE ALL DB EXTRACT
    all_db_extract <- dbExtractGenerator(all)
    
    #SAVE ALL DB EXTRACT
    saveRDS(all_db_extract, do.call(filenameGenerator, c(st, list(output="all_db"), list(file_type="Rda"))))
    cwt(all_db_extract, do.call(filenameGenerator, c(st, list(output="all_db"), list(file_type="csv"))))
    
    if (st$library_strategy == "ChIP-Seq" & !is.null(st$secondary_library_strategy)){
      #=========================
      # 'SECONDARY' sheet (all entries within SRP that are from the secondary_library_strategy)
      #    ------ONLY AVAILABLE FOR CHIP------
      #=========================
      
      secondary <- filter(df, library_strategy == st$secondary_library_strategy)
      
      #GENERATE SECONDARY SAMPLE SHEET
      secondary_sample_sheet <- universalSampleSheetGenerator(df=secondary, library_strategy = st$secondary_library_strategy)
      
      #SAVE SECONDARY SAMPLE SHEET
      saveRDS(secondary_sample_sheet, do.call(filenameGenerator, c(st, list(output="2ry_sm"), list(file_type="Rda"))))
      cwt(secondary_sample_sheet, do.call(filenameGenerator, c(st, list(output="2ry_sm"), list(file_type="csv"))))
      
      #GENERATE SECONDARY DB EXTRACT
      secondary_db_extract <- dbExtractGenerator(secondary)
      
      #SAVE SECONDARY DB EXTRACT
      saveRDS(secondary_db_extract, do.call(filenameGenerator, c(st, list(output="2ry_db"), list(file_type="Rda"))))
      cwt(secondary_db_extract, do.call(filenameGenerator, c(st, list(output="2ry_db"), list(file_type="csv"))))
    }
  }
}
#============================================================================





#============================================================================
# filenameGenerator()
#============================================================================

#Developed in outputGenerator.R
filenameGenerator <- function(library_strategy, gene=NULL, antibody=NULL, cell_type=NULL, treatment=NULL, species=NULL, platform=NULL, secondary_library_strategy=NULL, output, file_type){ #Same arguments as testf, as well as output and file_type
  
  argument_list <- c("library_strategy", "gene", "antibody", "cell_type", "treatment", "species", "platform", "secondary_library_strategy", "output") #file_type will be handled separately
  
  #Shorten library_strategy names whenever appropriate
  ls_substitute_list <- list()
  ls_substitute_list[[1]] <- c("RNA-Seq", "ChIP-Seq") #Original names
  ls_substitute_list[[2]] <- c("RNA", "ChIP") #New names
  
  for (s in seq_along(ls_substitute_list[[1]])){
    if (library_strategy == ls_substitute_list[[1]][s]){
      library_strategy <- ls_substitute_list[[2]][s]
    }
  }
  
  #Shorten secondary_library_strategy names
  for (s in seq_along(ls_substitute_list[[1]])){
    for (l in seq_along(secondary_library_strategy)){
      if (secondary_library_strategy[l] == ls_substitute_list[[1]][s]){
        secondary_library_strategy <- ls_substitute_list[[2]][s]
      }
    }
  }
  #Add '2' prefix
  secondary_library_strategy <- paste0("2", secondary_library_strategy)
  
  
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
  
  today <- Sys.Date()
  today <- format(today, format = "%y%m%d")
  
  name <- paste0(name, "_", today)
  
  name <- substr(name, 2, (nchar(name)))
  name <- paste0(name, ".", file_type) #file_type added separately at the end
  
  return(name)
  
}
#============================================================================



#============================================================================
# digitSort()
#============================================================================

#Developed in outputGenerator.R
digitSort <- function(inp){
  # Args: a vector or a list of vectors
  #          of the following form:
  #            [A-Za-z]+[0-9]+
  #          (throws an error if vectors do not conform)
  #
  # Returns: a vector of indices 
  #          ordered according to v1, (v2, v3... )
  #
  # Ordering disregards any letters occurring before the digits
  #          i.e. c("A2", "B1", "C3") will yield 2, 1, 3 
  #
  
  if (class(inp)!="list"){
    check <- grepl("[A-Za-z]+[0-9]+", inp)
    if (sum(check)!=length(inp)){
      stop("Vector needs to be in [A-Za-z]+[0-9]+ format")
    }
    inp_extr <- gsub("([A-Za-z]+)([0-9]+)", "\\2", inp)
    #print(inp_extr)
    inp_order <- do.call(order, list(inp_extr)) #inp_extr needs to be converted to a list
    
  } else {
    inp_extr <- list()
    for (el in seq_along(inp)){
      check <- grepl("[A-Za-z]+[0-9]+", inp[[el]])
      if (sum(check)!=length(inp[[el]])){
        stop("Vector needs to be in [A-Za-z]+[0-9]+ format")
      }
      inp_extr[[el]] <- gsub("([A-Za-z]+)([0-9]+)", "\\2", inp[[el]])
    }
    inp_order <- do.call(order, inp_extr) #inp_extr is already a list
  }
  return(inp_order)
}
#============================================================================




#============================================================================
# universalSampleSheetGenerator()
#============================================================================
#Developed in sampleSheetGenerator.R
universalSampleSheetGenerator <- function(df, library_strategy){
  # Selects correct sample sheet generating function, depending on the library_strategy
  # ===*=== Consider having a list of acceptable strategies and testing if input is valid
  
  if (library_strategy == "ChIP-Seq"){
    sample_sheet <- chipSampleSheetGenerator(df)
  } else if (library_strategy == "RNA-Seq"){
    sample_sheet <- rnaSampleSheetGenerator(df)
  } else {
    #If library strategy IS NOT chip or rna
    sample_sheet <- otherSampleSheetGenerator(df)
  }
  return(sample_sheet)
}


#============================================================================
chipSampleSheetGenerator <- function(df){
  # Arg: df from which sample sheet will be generated
  # Returns: sample_sheet (format compatible with the pipeline)
  
  #List of required columns
  required_columns <- c("run_accession", 
                        "study_accession", 
                        "sa_tissue", 
                        "experiment_title", 
                        "lane", 
                        "mer", 
                        "input",
                        "pairedEnd")
  
  #Check if required columns exist within the data frame
  columnVerifier(df, required_columns)
  
  #Create the sample_sheet
  sample_sheet <- data.frame(ID=df[ , c("run_accession")])
  #sample_sheet$ID <- df[ , c("run_accession")]
  sample_sheet$fileName <- ""
  sample_sheet$fileNamePE <- ""
  sample_sheet$experiment <- df[ , c("study_accession")]
  #sample_sheet$experiment <- df[ , c("series_id")] #Previously used column
  sample_sheet$tissue <- df[ , c("sa_tissue")]
  #sample_sheet$tissue <- df[ , c("source_name_ch1")] #Another option
  sample_sheet$condition <- df[, c("experiment_title")]
  sample_sheet$replicate <- ""
  sample_sheet$lane <- df[ , c("lane")]
  sample_sheet$merge <- df[ , c("mer")]
  sample_sheet$input <- df[ , c("input")]
  sample_sheet$pairedEnd <- df[ , c("pairedEnd")]
  sample_sheet$macsGroup <- ""
  sample_sheet$phredScore <- ""
  sample_sheet$adapter <- ""
  sample_sheet$trimQuality <- ""
  sample_sheet$adapterPE <- ""
  sample_sheet$trimQualityPE <- ""
  
  
  return(sample_sheet)
}
#============================================================================

#============================================================================
rnaSampleSheetGenerator <- function(df){
  # Arg: df from which sample sheet will be generated
  # Returns: sample_sheet (format compatible with the pipeline)
  
  # In comparison to ChIP: does not have tissue, input and macsGroup columns
  
  #List of required columns
  required_columns <- c("run_accession", 
                        "study_accession", 
                        "experiment_title", 
                        "lane", 
                        "mer",
                        "pairedEnd")
  
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
  sample_sheet$condition <- df[, c("experiment_title")]
  sample_sheet$replicate <- ""
  sample_sheet$lane <- df[ , c("lane")]
  sample_sheet$merge <- df[ , c("mer")]
  
  sample_sheet$pairedEnd <- df[ , c("pairedEnd")]
  
  sample_sheet$phredScore <- ""
  sample_sheet$adapter <- ""
  sample_sheet$trimQuality <- ""
  sample_sheet$adapterPE <- ""
  sample_sheet$trimQualityPE <- ""
  
  
  return(sample_sheet)
}
#============================================================================

#============================================================================
otherSampleSheetGenerator <- function(df){
  # Arg: df from which sample sheet will be generated
  # Returns: sample_sheet (format compatible with the pipeline)
  
  # Currently same as rnaSampleSheetGenerator
  # In comparison to ChIP: does not have tissue, input and macsGroup columns
  
  #List of required columns
  required_columns <- c("run_accession", 
                        "study_accession", 
                        "experiment_title", 
                        "lane", 
                        "mer",
                        "pairedEnd")
  
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
  sample_sheet$condition <- df[, c("experiment_title")]
  sample_sheet$replicate <- ""
  sample_sheet$lane <- df[ , c("lane")]
  sample_sheet$merge <- df[ , c("mer")]
  
  sample_sheet$pairedEnd <- df[ , c("pairedEnd")]
  
  sample_sheet$phredScore <- ""
  sample_sheet$adapter <- ""
  sample_sheet$trimQuality <- ""
  sample_sheet$adapterPE <- ""
  sample_sheet$trimQualityPE <- ""
  
  
  return(sample_sheet)
}
#============================================================================

#============================================================================
#============================================================================




#============================================================================
#dbExtractGenerator()
#============================================================================

#Developed in dbExtractGenerator.R
#============================================================================
dbExtractGenerator <- function(df){
  
  #Select columns to be extracted ===*=== Probably more needed!!!
  df_columns <- c("run_accession", 
                  "experiment_accession",
                  "sample_accession", 
                  "study_accession", 
                  "sample",
                  "series_id",
                  "library_strategy",
                  "platform",
                  "library_layout",
                  "pairedEnd",
                  "taxon_id",
                  "sample_name",
                  "experiment_title",
                  "experiment_name",
                  "sample_attribute",
                  "characteristics_ch1",
                  "sa_tissue",
                  "ch1_tissue",
                  "sa_antibody",
                  "ch1_antibody",
                  "sa_gene",
                  "ch1_gene",
                  "sa_treatment",
                  "ch1_treatment"
  )
  
  #Select new column names
  db_extract_columns <- df_columns
  
  #Check if the specified columns exist within the df
  #Not needed, because already done by columnSelector
  #columnVerifier(df, df_columns)
  
  #Create an extract
  db_extract <- columnSelector(df, df_columns = df_columns, out_columns = db_extract_columns)
  
  return(db_extract)
  
}

#============================================================================



#============================================================================
# columnSelector()
#============================================================================


#Developed in dbExtractGenerator.R
columnSelector <- function(df, df_columns, out_columns){
  
  columnVerifier(df, df_columns) #Check if all specified columns exist within the df
  
  if (length(df_columns)!=length(out_columns)){
    stop("Df_columns and out_columns need to be the same length")
  }
  
  out <- data.frame(matrix(nrow=dim(df)[1])) #Initialise the df
  where <- environment()
  
  for (n in seq_along(df_columns)){
    #print(select(df, eval(df_columns[n])))
    where[["out"]][out_columns[n]] <- select(df, eval(df_columns[n]))
  }
  
  out <- out[,-1]
  return(out)
}

#============================================================================



