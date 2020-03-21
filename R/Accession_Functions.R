# Accession_Functions.R
# Functions related to searching for accessions
#
# Currently includes:
# [- searchForAccession - main function that returns df from SRA 
# REMOVED (20171207) - building separate version]
#
# - classifyAccession - given an accession vector, classifies it 
# into correct class or returns an error if not all elements are matched 
# to the same class
# 
# - vConditionVerifier - grepl'es on the input vector according to any 
# of the regular expressions (an OR operation)







#----------------------------------------------------------------------------


#' Classify accessions
#' 
#' @param x Vector of accessions
#' @param na.ignore Logical indicating whether NAs are ignored
#' @return Accession class (error if input does not match any)
#' 
#' @examples 
#' classifyAccession("GSM11111")
#' # classifyAccession(c("GSM11111", "GSE2222")) # Will throw error
#' 
#' 
#' 
#' @section Supported accession classes:
#' \itemize{
#'    \item [D|E|S]RP - study_accession
#'    \item [D|E|S]RS - sample_accession
#'    \item [D|E|S]RX - experiment_accession
#'    \item [D|E|S]RR - run_accession
#'    \item GSE - series_id
#'    \item GSM - gsm
#' }
#' 
#' \strong{NOTE:} Input vector needs to contain accessions belonging to only 
#' one type at any given time, otherwise the function will generate an error.
#' 
#' @keywords internal
#'  
classifyAccession <- function(x, na.ignore = FALSE){
    
    accession_regexp <- list(c("[DES]RP"),
                                c("[DES]RS"),
                                c("[DES]RX"),
                                c("[DES]RR"),
                                c("GSE"),
                                c("GSM"))
    
    # Only allow perfect matches to accession numbers
    accession_regexp <- paste0("^", accession_regexp, "\\d+$") 
    
    accession_name <- c("study_accession",
                        "sample_accession",
                        "experiment_accession",
                        "run_accession",
                        "series_id", #A little tricky to deal with
                        "gsm") #A little tricky to deal with
    
    # Note: if length(x)==0, the last class from accession_name is assigned
    if(length(x)==0){
        stop("Accession vector must have length > 0")
    }
    
    if(na.ignore){
        x <- x[!is.na(x)]
    }
    
    accession_class <- NA
    
    for (a in seq_along(accession_regexp)){
        if (sum(vConditionVerifier(accession_regexp[[a]], x))==length(x))
            accession_class <- accession_name[a]
    }
    
    if (is.na(accession_class)){
        t1<-"Input needs to completely match only one of the accession classes"
        stop(t1)
    }
    return(accession_class)
}




#----------------------------------------------------------------------------



#' Verify matches to regular expressions
#'
#' @param regexpr_vector Vector with regular expressions (to be used by grepl)
#' @param x Character string to be checked for matches to regular expressions
#' @return A logical vector with TRUE at positions where there was 
#' a match to at least one of the reqular expressions (OR operation)
#' 
#' @examples 
#' vConditionVerifier("^m$", c("m", "mm", "cm", "M"))
#' 
#' @keywords internal
#' 
#' 
vConditionVerifier <- function(regexpr_vector, x){
    rv <- regexpr_vector
    
    out <- rep(FALSE, length(x))
    for (r in seq_along(rv)){
        out <- grepl(rv[r], x) | out
    }
    
    return(out)
    
}
