
#searchForAccession - under construction (20171207)


searchForAccession <- function(acc_list){
  #
  # IN PROGRESS - to find omitted processing steps, see SpideR_ToDo.Rmd
  #
  # Args: a character vector with accessions
  #       (needs to completely match to one accession class; no partial matches or mixed classes allowed)
  #
  # Returns: df from SRA with matches to the acc_list
  #

  x <- unique(acc_list)
  x <- x[digitSort(x)]

  accession_class <- classifyAccession(x)


  output_df <- searchForAccessionAcrossDBs(acc_list = x, sra_columns = "*", geo_columns = "*")

  #output_df <- gsmExtractor(output_df, sampleColumn = TRUE) #Create sample column
  output_df <- gsmExtractor(output_df, sampleColumn = FALSE) #Don't create sample column


  #Instead, rename the native gsm column to sample to match the column naming in searchForTerm
  names(output_df)[names(output_df)=="gsm"] <- "sample"



  output_df <- saExtractor(output_df)
  output_df <- chExtractor(output_df)


  #No input/controlDetector used
  output_df$input <- NA
  output_df$control <- NA

  output_df <- mergeDetector(output_df)

  #No missingRunVerifier used

  output_df <- pairedEndConverter(output_df)

  output_df <- naConverter(output_df)

  outputGenerator_acc(output_df, accession = acc_list)

  return(output_df)


}




searchForAccession_temp <- function(acc_list){
  #
  # IN PROGRESS - to find omitted processing steps, see SpideR_ToDo.Rmd
  #
  # Args: a character vector with accessions
  #       (needs to completely match to one accession class; no partial matches or mixed classes allowed)
  #
  # Returns: df from SRA with matches to the acc_list
  #

  x <- unique(acc_list)
  x <- x[digitSort(x)]

  accession_class <- classifyAccession(x)

  output_df <- searchGEOForGSE(acc_list = x, geo_columns = "*")

  output_df <- chExtractor(output_df)

  output_df$input <- NA
  output_df$control <- NA

  #output_df <- mergeDetector(output_df)

  #output_df <- pairedEndConverter(output_df)


  return(output_df)


}
