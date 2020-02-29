
context("Accession Search Functions")


#' Main objectives tested
#' 
#' Search all (searchForAccessionAcrossDBsDF())
#' - wrong or incomplete accession identifiers
#' 
#' 
#' Search SRA / GEO
#' - wrong type of accessions
#' - wrong column names


test_that("searchForAccessionAcrossDBsDF", {
  
  # Wrong / incomplete accession identifiers
  expect_error(searchForAccessionAcrossDBsDF("", "*")) #Should throw error via digitSort
  expect_error(searchForAccessionAcrossDBsDF("abc", "*")) #Should throw error via digitSort
  
  # SRA-like
  expect_error(searchForAccessionAcrossDBsDF("SR", "*")) #Should throw error via digitSort
  expect_error(searchForAccessionAcrossDBsDF("SRO", "*")) #Should throw error via digitSort
  expect_error(searchForAccessionAcrossDBsDF("SR11", "*")) #Should throw error via digitSort
  expect_error(searchForAccessionAcrossDBsDF("SR.", "*")) #Should throw error via digitSort
  expect_error(suppressWarnings(searchForAccessionAcrossDBsDF("SRR11", "*"))) #Should throw error via digitSort
  expect_error(searchForAccessionAcrossDBsDF("SRX11", "*")) #Should throw error via digitSort
  expect_error(searchForAccessionAcrossDBsDF("SRS11", "*")) #Should throw error via digitSort
  expect_error(searchForAccessionAcrossDBsDF("SRP11", "*")) #Should throw error via digitSort

  
  # GEO-like
  expect_error(searchForAccessionAcrossDBsDF("GEO", "*")) #Should throw error via digitSort
  expect_error(searchForAccessionAcrossDBsDF("GS", "*")) #Should throw error via digitSort
  expect_error(searchForAccessionAcrossDBsDF("GSM", "*")) #Should throw error via digitSort
  expect_error(searchForAccessionAcrossDBsDF("GSM11", "*")) #Should throw error via digitSort
  expect_error(searchForAccessionAcrossDBsDF("GE", "*")) #Should throw error via digitSort
  expect_error(searchForAccessionAcrossDBsDF("GSE", "*")) #Should throw error via digitSort
  expect_error(searchForAccessionAcrossDBsDF("GSE11", "*")) #Should throw error via digitSort
  
  
})





# searchSRAForAccession ####

test_that("searchSRAForAccession", {
  
  #Check that non-existent accessions
  # DELETED: 1. throw a warning
  # 2. do not return matches
  # expect_warning(searchSRAForAccession("SRR1", "*"))
  expect_equal(dim(suppressWarnings(searchSRAForAccession("SRR1", "*")))[1], 0)
  
  #Check that other accessions are not accepted
  expect_error(searchSRAForAccession("GSM1000", "*"))
  expect_error(searchSRAForAccession("GSE1000", "*"))
  
  
  #Check that non-existent columns are not accepted and throw an error
  expect_error(searchSRAForAccession("SRR988139", "nth"))
  expect_error(searchSRAForAccession("SRR988139", "sth"))
  
  
  
})


# searchGEOForGSE ####
test_that("searchGEOForGSE", {
  
  # Check that non-existent accessions
  # DELETED: 1. throw a warning
  # 2. do not return matches
  # expect_warning(searchSRAForAccession("SRR1", "*"))
  expect_equal(dim(suppressWarnings(searchGEOForGSE("GSE1", "*", "*")))[1], 0)
  
  # Check that other accessions are not accepted
  expect_error(searchGEOForGSE("GSM1000", "*", "*"))
  expect_error(searchGEOForGSE("SRP1000", "*", "*"))
  expect_error(searchGEOForGSE("SRR1000", "*", "*"))
  
  
  
  # Check that non-existent columns are not accepted and throw an error
  expect_error(searchGEOForGSE("GSE1000", "nth"))
  expect_error(searchGEOForGSE("GSE1000", "sth"))
  
  
})




# searchGEOForGSM ####
test_that("searchGEOForGSM", {
  
  # Check that non-existent accessions
  # DELETED: 1. throw a warning
  # 2. do not return matches
  # expect_warning(searchSRAForAccession("SRR1", "*"))
  expect_equal(dim(suppressWarnings(searchGEOForGSM("GSM1", "*", "*")))[1], 0)
  
  # Check that other accessions are not accepted
  expect_error(searchGEOForGSM("GSE1000", "*", "*"))
  expect_error(searchGEOForGSM("SRP1000", "*", "*"))
  expect_error(searchGEOForGSM("SRR1000", "*", "*"))
  
  
  
  # Check that non-existent columns are not accepted and throw an error
  expect_error(searchGEOForGSM("GSM1000", "nth"))
  expect_error(searchGEOForGSM("GSM1000", "sth"))
  
  
})

