context("startSpiderSeqR")

#' Things to check:
#' - can find files in testdata/Mock_Database_Files/
#' - can find files in testdata/Mock_Database_Files 
#'     (both directly within and in the subdirectory)


test_that("Seamless behaviour with all the files", {
    
    expect_true(startSpiderSeqR(
        "testdata/Mock_Database_Files/Duplicate_Location", 
        general_expiry = 10000))
    
})


test_that("Outdated files (fails)", {
    
    expect_true(startSpiderSeqR(
        "testdata/Mock_Database_Files/Duplicate_Location", 
        general_expiry = 0))
    
})
