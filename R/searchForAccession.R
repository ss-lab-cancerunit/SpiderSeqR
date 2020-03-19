
#searchForAccession - under construction (20171207)

#' Search for Accessions
#' 
#' @param acc_vector A character vector with accession numbers (of one type only)
#' @param file_output A logical indicating whether file output should be produced
#' @return A data frame (and file outputs, as appropriate) with matches to accession numbers
#' 
#' 
#' @examples 
#' startSpiderSeqRDemo()
#' df <- searchForAccession("SRP026280")
#' 
#' @export
#' 
searchForAccession <- function(acc_vector, file_output = TRUE){
    #
    # IN PROGRESS - to find omitted processing steps, see SpideR_ToDo.Rmd
    #
    # Args: a character vector with accessions
    #       (needs to completely match to one accession class; no partial matches or mixed classes allowed)
    #
    # Returns: df from SRA with matches to the acc_vector
    #
    
    x <- unique(acc_vector)
    x <- x[orderAccessions(x)]
    x <- x[!is.na(x)]
    
    accession_class <- classifyAccession(x)
    
    
    output_df <- searchForAccessionAcrossDBsDF(acc_vector = x, sra_columns = "*", geo_columns = "*", gse_columns = "*")
    
    #output_df <- gsmExtractor(output_df, sampleColumn = TRUE) #Create sample column
    output_df <- gsmExtractor(output_df, sampleColumn = FALSE) #Don't create sample column
    
    
    #Instead, rename the native gsm column to sample to match the column naming in searchForTerm
    #names(output_df)[names(output_df)=="gsm"] <- "sample" # sampletogsm ===*===
    
    
    
    output_df <- saExtractor(output_df)
    output_df <- chExtractor(output_df)
    
    
    #No input/controlDetector used
    output_df$input <- NA
    output_df$control <- NA
    
    output_df <- mergeDetector(output_df)
    
    #No missingRunVerifier used
    
    output_df <- pairedEndConverter(output_df)
    
    output_df <- naConverter(output_df)
    
    
    output_df <- renameOTHColumns(output_df)
    
    output_df <- unifyDFFormat(output_df) # Removes SRA_sra_ID column, orders by accession
    
    .GlobalEnv$temp_output_df <- output_df
    
    if (file_output == TRUE){
        outputGenerator_acc(output_df, accession = acc_vector)
    }
    
    
    return(output_df)
    
    
}

