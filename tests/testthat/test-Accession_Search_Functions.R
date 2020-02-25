
context("Accession Search Functions")


test_that("searchSRAForAccession", {
  
  #ERRORS AND INCORRECT INPUTS
  
  #Check that incorrect entries are not accepted and throw an error
  expect_error(searchSRAForAccession("SR", "*")) #Should throw error via digitSort
  expect_error(searchSRAForAccession("SR.", "*")) #Should throw error via digitSort
  
  expect_error(searchSRAForAccession("SRG223", "*")) #Should throw error via accessionClassifier
  
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
  
  
})