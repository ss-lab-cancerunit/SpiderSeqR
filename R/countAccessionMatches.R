

#' Count accession matches within a data frame
#' 
#' @param acc_vector A character vector with the accessions 
#' to be found within data frame
#' @param df Data frame to be checked (needs to contain at least 
#' the column corresponding to accession type that is searched for)
#' @param output A character indicating the type of output, 
#' can be \code{"logical"} (returns \code{TRUE} if all 
#' (non-NA, non-empty, unique) acc_vector elements are present 
#' in the data frame) or \code{"numeric"} 
#' (returns a numerical vector with the count of matches and the length of 
#' (non-NA, non-empty, unique) acc_vector)
#' 
#' @return Depends on the output type (see above)
#' 
#' @description 
#' Check whether all accessions are present within a data frame. 
#' Ignores empty strings NAs and repeated elements within the acc_vector.
#' 
#' @keywords internal
#' 
countAccessionMatches <- function(acc_vector, df, output = "logical"){
    
    output_list <- c("logical", "numeric")
    output <- match.arg(output, output_list, several.ok = FALSE)
    
    # Process the accession vector ####
    acc_vector <- acc_vector[!is.na(acc_vector)]
    acc_vector <- acc_vector[acc_vector!=""]
    acc_vector <- unique(acc_vector)
    
    # Need to add a clause about GSEs ===*===
    
    
    # Get acc_class
    acc_class <- classifyAccession(acc_vector)
    
    columnVerifier(df, acc_class)
    
    ind <- grep(acc_class, colnames(df))
    
    # Get the vector with accessions from the df 
    # (special treatment for series_id) ####
    if (acc_class == "series_id"){
        df_acc <- df[, ind]
        df_acc <- unlist(strsplit(df_acc, split = ","))
        df_acc <- unique(df_acc)
    } else {
        df_acc <- df[, ind]
    }
    
    # Count matches ####
    match_count <- sum(acc_vector %in% df_acc)
    missing_acc <- acc_vector[!(acc_vector %in% df_acc)]
    
    if (match_count == length(acc_vector)){
        mm(paste0("All accessions found (", match_count, "/", 
                    length(acc_vector), ")."), "res")
        
    } else {
        mm(paste0("Not all accessions found (", match_count, "/", 
                    length(acc_vector), ")."), "res")
        
        # Do not overflow the console output with lots of accessions
        if (length(missing_acc)<200){ 
            mm(paste0("Missing accessions: ", paste0(missing_acc, 
                                                     collapse = ", ")), "res")
        }
    }
    
    # Give appropriate output ####
    if (output == "logical"){
        return(match_count == length(acc_vector))
    } else if (output == "numeric"){
        out <- c(match_count, length(acc_vector))
        names(out) <- c("Match_Count", "Total")
        return(out)
    }
    
}

