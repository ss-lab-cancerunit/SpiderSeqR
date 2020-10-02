context("startSpiderSeqR")

#' Things to check:
#' - can find files in testdata/Mock_Database_Files/
#' - can find files in testdata/Mock_Database_Files 
#'     (both directly within and in the subdirectory)
#'     

nev_expiry <- 10000 # 'Never' expiry


test_that("Seamless behaviour with all the files (1)", {
    skip("local_only")
    expect_true(startSpiderSeqR(
        "testdata/Mock_Database_Files/All_Present", 
        general_expiry = nev_expiry))
    
})



test_that("Seamless behaviour with all the files (2)", {
    skip("No propagation")
    expect_true(startSpiderSeqR(
        "testdata/Mock_Database_Files/Duplicate_Location", 
        general_expiry = nev_expiry))
    
})


test_that("Outdated files (fails)", {
    skip("skip")
    expect_true(startSpiderSeqR(
        "testdata/Mock_Database_Files/Duplicate_Location", 
        general_expiry = 0))
    
})



# Missing files

test_that("No SRA", {
    skip("skip")

    expect_true(startSpiderSeqR(
        "testdata/Mock_Database_Files/SRA_Missing",
        general_expiry = nev_expiry))
})

test_that("No GEO", {

    skip("skip")
    expect_true(startSpiderSeqR(
        "testdata/Mock_Database_Files/GEO_Missing",
        general_expiry = nev_expiry))
})


test_that("No SpiderSeqR", {
    skip("local_only")
    expect_true(startSpiderSeqR(
        "testdata/Mock_Database_Files/SpiderSeqR_Missing",
        general_expiry = nev_expiry))
})


# Files in subdirectory

test_that("Files in subdirectory", {
    skip("local_only")
    expect_true(startSpiderSeqR(
        "testdata/Mock_Database_Files/Files_in_Subdirectory", 
        general_expiry = nev_expiry))
    
})


test_that("Test and see", {
    skip("Ambiguous directory")

    expect_true(startSpiderSeqR(
        "testdata/Mock_Database_Files/", 
        general_expiry = nev_expiry))
    
})




