
context(".verifySuperseries")



#' Things to check:
#' - no superseries
#' - no GSEs (all NAs or sth)
#' - 2 series
#' - 3 series
#' - ...



test_that("Empty entries", {
  
  expect_true(is.null(.verifySuperseries(NULL)))
  expect_true(is.null(.verifySuperseries(rep("NA",5))))
  expect_true(is.null(.verifySuperseries(rep(NA,10))))
  
  
})


test_that("Real entries", {
  
  # NOTE: specific examples from the DEMO databases
  
  # Non-superseries
  expect_true(is.null(.verifySuperseries(convertAccession("GSE48253")$series_id)))
  expect_true(is.null(.verifySuperseries(convertAccession("GSE69001")$series_id)))
  expect_true(is.null(.verifySuperseries(convertAccession("GSE27360")$series_id)))
  expect_true(is.null(.verifySuperseries(convertAccession("GSE82246")$series_id)))
  expect_true(is.null(.verifySuperseries(convertAccession("GSE76553")$series_id)))
  expect_true(is.null(.verifySuperseries(convertAccession("GSE10309")$series_id)))
  
  
  # Superseries
  
  expect_true(!is.null(.verifySuperseries(convertAccession("GSE36467")$series_id))) # Superseries
  expect_true(!is.null(.verifySuperseries(convertAccession("GSE36466")$series_id))) # Subseries
  expect_true(!is.null(.verifySuperseries(convertAccession("GSE36465")$series_id))) # Subseries
  
  
  
  expect_true(!is.null(.verifySuperseries(convertAccession("GSE80767")$series_id))) # Superseries
  expect_true(!is.null(.verifySuperseries(convertAccession("GSE80766")$series_id))) # Subseries
  expect_true(!is.null(.verifySuperseries(convertAccession("GSE80598")$series_id))) # Subseries
  

})



