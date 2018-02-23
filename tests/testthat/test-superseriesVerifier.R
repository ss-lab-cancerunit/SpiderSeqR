context("superseriesVerifier")


test1 <- readRDS("tests/testthat/spider_combined_before_ss.Rda") #Df with no superseries

test2 <- convertAccession("GSE75508") #Superseries with 2 GSEs
out2 <- c("GSE75506", "GSE75507", "GSE75508")


#Things to check:
#- no superseries
#- no GSEs (all NAs or sth)
#- 2 series
#- 3 series
#- ...

test_that("Checking cases", {
  expect_true(is.null(superseriesVerifier(test1$series_id)))
  expect_identical(superseriesVerifier(test2$series_id), out2)

})


