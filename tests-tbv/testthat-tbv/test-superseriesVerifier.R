
context("superseriesVerifier")




#test1 <- readRDS("tests/testthat/spider_combined_before_ss.Rda") #Df with no superseries
test1 <- readRDS("spider_combined_before_ss.Rda") #Df with no superseries


test2 <- convertAccession("GSE75508") #Superseries with 2 GSEs
out2 <- c("GSE75506", "GSE75507", "GSE75508") #Correct GSE list



test3 <- convertAccession("GSE25933") #Superseries with multiple GSEs (and one other superseries...)
out3 <- c("GSE25933", "GSE22720", "GSE22723", "GSE22740", "GSE22746", "GSE22750", "GSE24241", "GSE24318", "GSE26180", "GSE26200", "GSE26201", "GSE26202", "GSE26203", "GSE26204", "GSE26186")




#Things to check:
#- no superseries
#- no GSEs (all NAs or sth)
#- 2 series
#- 3 series
#- ...


test_that("Checking cases", {

  expect_true(is.null(superseriesVerifier(NULL)))
  expect_true(is.null(superseriesVerifier(rep("NA",5))))
  expect_true(is.null(superseriesVerifier(rep(NA,10))))

  expect_true(is.null(superseriesVerifier(test1$series_id)))


  expect_true(setequal(superseriesVerifier(test2$series_id), out2))

  expect_true(setequal(superseriesVerifier(test3$series_id), out3))


})


