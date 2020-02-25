context("Accession_Search_Functions")

#DONE:
#searchSRAForAccesssion

#TO DO:
# - check incorrect inputs
# - check incorrect accessions
# - check incorrect columns
# - check number of rows for a few random entries (generate random entries each time?)
#searchGEOForGSM
#searchGEOForGSE
#searchSRR_GSM

test_that("searchSRAForAccession", {

  #ERRORS AND INCORRECT INPUTS

  #Check that incorrect entries are not accepted and throw an error
  expect_error(searchSRAForAccession("SR", "*")) #Should throw error via digitSort
  expect_error(searchSRAForAccession("SR.", "*")) #Should throw error via digitSort

  expect_error(searchSRAForAccession("SRG223", "*")) #Should throw error via accessionClassifier

  #Check that non-existent accessions
  # 1. throw a warning
  # 2. do not return matches
  expect_warning(searchSRAForAccession("SRR1", "*"))
  expect_equal(dim(suppressWarnings(searchSRAForAccession("SRR1", "*")))[1], 0)

  #Check that other accessions are not accepted
  expect_error(searchSRAForAccession("GSM1000", "*"))
  expect_error(searchSRAForAccession("GSE1000", "*"))


  #Check that non-existent columns are not accepted and throw an error
  expect_error(searchSRAForAccession("SRR988139", "nth"))


  #Check that a correct number of entries is returned for a few random entries
  # - SRP
  expect_equal(dim(searchSRAForAccession("SRP030489", "*"))[1], 7) #7
  expect_equal(dim(searchSRAForAccession("SRP019664", "*"))[1], 4) #4
  expect_equal(dim(searchSRAForAccession("SRP065890", "*"))[1], 68) #68
  # - SRX
  expect_equal(dim(searchSRAForAccession("ERX1210133", "*"))[1], 1) #1
  expect_equal(dim(searchSRAForAccession("SRX021642", "*"))[1], 11) #11
  expect_equal(dim(searchSRAForAccession("SRX2370783", "*"))[1], 5) #5
  # - SRS
  expect_equal(dim(searchSRAForAccession("ERS502184", "*"))[1], 1) #1
  expect_equal(dim(searchSRAForAccession("SRS1236919", "*"))[1], 2) #2
  expect_equal(dim(searchSRAForAccession("SRS1105782", "*"))[1], 2) #2
  # - SRR
  expect_equal(dim(searchSRAForAccession("SRR1802330", "*"))[1], 1)
  expect_equal(dim(searchSRAForAccession("ERR751902", "*"))[1], 1)
  expect_equal(dim(searchSRAForAccession("ERR1305626", "*"))[1], 1)
  expect_equal(dim(searchSRAForAccession("SRR3220978", "*"))[1], 1)
  expect_equal(dim(searchSRAForAccession("DRR061598", "*"))[1], 1)

})
