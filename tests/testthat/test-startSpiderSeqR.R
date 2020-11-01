context("startSpiderSeqR")

#' Things to check:
#' - can find files in testdata/Mock_Database_Files/
#' - can find files in testdata/Mock_Database_Files 
#'     (both directly within and in the subdirectory)
#'     

nev_expiry <- 10000 # 'Never' expiry


test_that("Seamless behaviour with all the files (1)", {
    skip("local_only")
    expect_true(ass_startSpiderSeqR(
        "testdata/Mock_Database_Files/All_Present", 
        general_expiry = nev_expiry))
    
})


test_that("No SRA", {
    #skip("skip")
    
    expect_true(ass_startSpiderSeqR(
        "testdata/Mock_Database_Files/SRA_Missing",
        general_expiry = nev_expiry))
})
