
context(".setExpiryParameters")
#' Things to test:
#' - expect warning if all 4 expiry parameters
#' - suppressing warnings, general expiry parameter is ignored when others
#'      are provided
#' - error if non-numerical input
#' - correct value if both general expiry and some of the other parameters
#' 


test_that("Warning if all 4 expiry parameters", {
    expect_warning(.setExpiryParameters(10,10,10,10,3))
})


test_that("General expiry ignored", {
    expect_equal(suppressWarnings(.setExpiryParameters(20,1,2,3,3)$sra_expiry),
                    1)
    expect_equal(suppressWarnings(.setExpiryParameters(20,1,2,3,3)$geo_expiry),
                    2)
    expect_equal(suppressWarnings(
            .setExpiryParameters(20,1,2,3,3)$srr_gsm_expiry), 3)
    
})



test_that("Non-numerical input gives error", {
    expect_error(.setExpiryParameters(10,2,"a", missing_file_number=1))
})



test_that("Correct value assigned", {
    
    # SRA is NULL
    expect_equal(.setExpiryParameters(general_expiry=10, 
                                        sra_expiry=NULL, 
                                        geo_expiry=2,
                                        srr_gsm_expiry=3,
                                        missing_file_number=3)$sra_expiry, 10)
    expect_equal(.setExpiryParameters(general_expiry=10, 
                                      sra_expiry=NULL, 
                                      geo_expiry=2,
                                      srr_gsm_expiry=3,
                                      missing_file_number=3)$geo_expiry, 2)
    expect_equal(.setExpiryParameters(general_expiry=10, 
                                      sra_expiry=NULL, 
                                      geo_expiry=2,
                                      srr_gsm_expiry=3,
                                      missing_file_number=3)$srr_gsm_expiry,3)
    
    
    # GEO is NULL
    expect_equal(.setExpiryParameters(general_expiry=10, 
                                      sra_expiry=1, 
                                      geo_expiry=NULL,
                                      srr_gsm_expiry=3,
                                      missing_file_number=3)$sra_expiry, 1)
    expect_equal(.setExpiryParameters(general_expiry=10, 
                                      sra_expiry=1, 
                                      geo_expiry=NULL,
                                      srr_gsm_expiry=3,
                                      missing_file_number=3)$geo_expiry, 10)
    expect_equal(.setExpiryParameters(general_expiry=10, 
                                      sra_expiry=1, 
                                      geo_expiry=NULL,
                                      srr_gsm_expiry=3,
                                      missing_file_number=3)$srr_gsm_expiry, 3)
    
    # SRR_GSM is NULL
    expect_equal(.setExpiryParameters(general_expiry=10, 
                                      sra_expiry=1, 
                                      geo_expiry=2,
                                      srr_gsm_expiry=NULL,
                                      missing_file_number=3)$sra_expiry, 1)
    expect_equal(.setExpiryParameters(general_expiry=10, 
                                      sra_expiry=1, 
                                      geo_expiry=2,
                                      srr_gsm_expiry=NULL,
                                      missing_file_number=3)$geo_expiry, 2)
    expect_equal(.setExpiryParameters(general_expiry=10, 
                                      sra_expiry=1, 
                                      geo_expiry=2,
                                      srr_gsm_expiry=NULL,
                                      missing_file_number=3)$srr_gsm_expiry,10)
})
