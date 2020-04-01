
#searchForAccession - under construction (20171207)

#' Search for Accessions
#' 
#' @param acc_vector A character vector with accession numbers 
#'     (of one type only)
#' @param file_output A logical indicating whether file output 
#'     should be produced
#' @param call_output A logical indicating whether call output 
#'     should be produced
#' @return A data frame (and file outputs, as appropriate) 
#'      with matches to accession numbers
#' 
#' 
#' @examples 
#' startSpiderSeqRDemo()
#' df <- searchForAccession("SRP026280")
#' 
#' @export
#' 
searchForAccession <- function(acc_vector, 
                                file_output = FALSE, 
                                call_output = FALSE){
    
    x <- unique(acc_vector)
    x <- x[orderAccessions(x)]
    x <- x[!is.na(x)]
    
    accession_class <- classifyAccession(x)
    
    if (call_output){
        callRecordGenerator(file = generateCallFile_SFA(acc_vector))
    }
    
    
    output_df <- searchForAccessionAcrossDBsDF(acc_vector = x, 
                                                sra_columns = "*", 
                                                geo_columns = "*", 
                                                gse_columns = "*")
    
    #Create sample column
    #output_df <- extractGSM(output_df, sampleColumn = TRUE) 
    
    #Don't create sample column
    output_df <- extractGSM(output_df, sampleColumn = FALSE) 
    
    
    #Instead, rename the native gsm column to sample to match the column naming
    # in searchForTerm
    #names(output_df)[names(output_df)=="gsm"] <- "sample" #sampletogsm ===*===
    
    
    
    output_df <- saExtractor(output_df)
    output_df <- chExtractor(output_df)
    
    
    #No detectInputs/Controls used
    output_df$input <- NA
    output_df$control <- NA
    
    output_df <- detectMerges(output_df)
    
    #No verifyMissingRuns used
    
    output_df <- convertPairedEnds(output_df)
    
    output_df <- unifyNAs(output_df)
    
    
    output_df <- renameOTHColumns(output_df)
    
    # [Removes SRA_sra_ID column, orders by accession]
    output_df <- unifyDFFormat(output_df) 
    
    .GlobalEnv$temp_output_df <- output_df
    
    if (file_output == TRUE){
        generateOutput_Accession(output_df, accession = acc_vector)
    }
    
    
    return(output_df)
    
    
}

