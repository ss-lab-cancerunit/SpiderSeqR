
#' Extract characteristics from a vector
#' 
#' @param df Data frame with characteristics_ch1 column
#' @return Data frame with extracted information
#' 
#' This is a wrapper around .universalExtractor, with key words specific 
#' for characteristics_ch1 field
#' 
#' @examples 
#' # .chExtractor(df)
#' 
#' 
#' @keywords internal
#' 
.chExtractor <- function(df){
    .mm("Running .chExtractor", "fn")
    
    
    # Add a clause for tackling GSM_ prefix
    gsm_replace <- FALSE
    if ("GSM_characteristics_ch1" %in% colnames(df)){
        gsm_replace <- TRUE
        colnames(df)[colnames(df) %in% "GSM_characteristics_ch1"] <- 
            "characteristics_ch1"
    }
    
    .verifyColumns(df, "characteristics_ch1")
    
    geo_char_columns <- c("ch1_original", "ch1_remainder", 
                            "ch1_tissue", "ch1_antibody", 
                            "ch1_gene", "ch1_treatment")
    
    
    if (sum(!is.na(df$characteristics_ch1))==0){
        
        df <- createEmptyColumns(df, geo_char_columns[-1])
        
        # Create new columns, except ch1_original
        #if (dim(df)[1]==0){
        #    df[, geo_char_columns[2]] <- character(0)
        #    df[, geo_char_columns[3]] <- character(0)
        #    df[, geo_char_columns[4]] <- character(0)
        #    df[, geo_char_columns[5]] <- character(0)
        #    df[, geo_char_columns[6]] <- character(0)
        #    #df[, geo_char_columns[-1]] <- rep(character(0), 
        #    #                                    length(geo_char_columns[-1]))
        #} else {
        #    df[, geo_char_columns[-1]] <- NA 
        #}
        
        
        
        if (gsm_replace){
            colnames(df)[colnames(df) %in% "characteristics_ch1"] <- 
                "GSM_characteristics_ch1"
        }
        warning("No not-NA sample attributes available")
        .mm(".chExtractor completed", "fn")
        return(df)
    }
    
    #========================================================================
    # Setting up keywords
    #========================================================================
    #===*=== Come back and add more choices to the category names
    
    geo_tissue <- c("tissue", "cell.?type", 
                    "cell.?line", "tissue.?type", "cell", 
                    "cell.?description") #NOT RESEARCHED
    
    geo_tissue <- paste0(geo_tissue, ": ")
    
    geo_antibody <- c("antibody") # NOT RESEARCHED
    geo_antibody <- paste0(geo_antibody, ": ")
    
    geo_gene <- c("genotype") # NOT RESEARCHED
    geo_gene <- paste0(geo_gene, ": ")
    
    geo_treatment <- c("treatment") # NOT RESEARCHED
    geo_treatment <- paste0(geo_treatment, ": ")
    
    #========================================================================
    
    geo_char_keywords <- list(geo_tissue, 
                                geo_antibody, geo_gene, geo_treatment)
    
    #========================================================================
    #========================================================================
    
    geo_sep_split <- ";\t"
    geo_sep_collapse <- ";\t"
    
    #========================================================================
    
    df_geo_char <- 
        plyr::ldply(df$characteristics_ch1, 
                    function(x) .universalExtractor(x, geo_char_keywords, 
                                                    geo_sep_split, 
                                                    geo_sep_collapse))
    
    colnames(df_geo_char) <- geo_char_columns
    
    # Combine extracted columns with geo df (except ch1_original column)
    df <- cbind(df, df_geo_char[, (-1)]) 
    
    #========================================================================
    
    if (gsm_replace){
        colnames(df)[colnames(df) %in% "characteristics_ch1"] <- 
            "GSM_characteristics_ch1"
    }
    
    .mm(".chExtractor completed", "fn")
    return(df)
}




