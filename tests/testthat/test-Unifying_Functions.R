context("Unifying functions")

test_that("Column sets", {
    
    x <- as.character(unlist(listValidColumns()))
    
    expect_identical(colnames(.generateEmptyDF()), x)
    expect_identical(colnames(searchAnywhere("TRIMKD")), x)
    expect_identical(colnames(searchForTerm(SRA_library_strategy = "ChIP-Seq", gene = "TRIMKD")), x)
    expect_identical(colnames(searchForAccession(sra_demo$run_accession[1])), x)
    
})


