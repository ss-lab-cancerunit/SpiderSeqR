# Accession_Functions.R
# Functions related to searching for accessions
#
# Currently includes:
# - searchForAccession - main function that returns df from SRA REMOVED TEMPORARILY (20171207) - building proper version
# - accessionClassifier - given an accession vector, classifies it into correct class or returns an error if not all elements are matched to the same class
# - vConditionVerifier - grepl'es on the input vector according to any of the regular expressions (an OR operation)

# USES:
# - digitSort - sorting by digits, disregarding preceding letters (NOT INCLUDED!)


#USAGE
#searchForAccession(spider_combined$run_accession[c(1,5,22,35)])

#searchForAccession("GSM522267")




accessionClassifier <- function(x){
  accession_regexp <- list(c("[DES]RP"),
                           c("[DES]RS"),
                           c("[DES]RX"),
                           c("[DES]RR"),
                           c("GSE"),
                           c("GSM"))

  accession_name <- c("study_accession",
                      "sample_accession",
                      "experiment_accession",
                      "run_accession",
                      "series_id", #A little tricky to deal with
                      "gsm") #A little tricky to deal with

  accession_class <- NA

  for (a in seq_along(accession_regexp)){
    if (sum(vConditionVerifier(accession_regexp[[a]], x))==length(x))
      accession_class <- accession_name[a]
  }

  if (is.na(accession_class)){
    stop("Input needs to completely match one of the accession classes")
  }
  return(accession_class)
}





vConditionVerifier <- function(regexpr_vector, x){
  rv <- regexpr_vector

  out <- rep(FALSE, length(x))
  for (r in seq_along(rv)){
    out <- grepl(rv[r], x) | out
  }
  return(out)

}
