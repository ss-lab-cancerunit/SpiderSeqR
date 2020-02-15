
context("convertAccession")

#' Testing conversion between databases
#' Things to be verified
#' - commutative - searching in SRA first = searching in GEO first (and vice versa)
#' - associative - searching for accessions individually yields same results as searching for all at the same time
#' - count - count of accessions in the database is equal to count as a result of the conversion





# Setup ####

n_tests <- 5
#demo <- TRUE
demo <- FALSE


if (demo){
  setSpiderSeqROption("quiet", TRUE)
  startSpiderSeqRDemo()
} else {
  startSpiderSeqR("C:/DD/Projects/SpideRs/SpiderSeqR-Auxillaries/Database_Files")
  setSpiderSeqROption("quiet", TRUE)
}



# Helper functions ####



#' Generate random accessions from SRA
#' 
#' @param x Character with accession type
#' @param n Number of accessions desired
#' @return A character vector with the randomly generated accessions
#' 
randomSRA <- function(x, n){
  y <- character()
  y <- DBI::dbGetQuery(sra_con, paste0("SELECT ", x, " FROM sra GROUP BY RANDOM() LIMIT ", n))
  y <- y[,1]
  return(y)
}



# Commutative ####

# SRA first
i <- 0

while (i < n_tests){ # Test up to 20x
  
  # Get random SRP
  if (demo){
    x <- sample(unique(sra_demo$study_accession), 1)
  } else {
    x <- randomSRA("study_accession", 1)
  }
 
  
  sra_input <- convertAccession(x)
  
  if (sum(grepl("GSM\\d\\d\\d+", sra_input$gsm))>0){
    i <- i + 1
    geo_input <- convertAccession(sra_input$gsm)
    test_that("Commutative conversion SRA -> GEO", {
      expect_identical(sra_input, geo_input)
    })
  }
  
}