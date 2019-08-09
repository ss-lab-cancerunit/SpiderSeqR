context("convertCategoriesToLibraryStrategyType")



test_that("Checking errors", {
  expect_error(convertCategoriesToLibraryStrategyType("no match"))
  expect_error(convertCategoriesToLibraryStrategyType(NA))
  expect_error(convertCategoriesToLibraryStrategyType(NULL))
})



test_that("Convert chips", {
  
  #SRA
  expect_true(convertCategoriesToLibraryStrategyType("chip")$sra_library_strategy == "ChIP-Seq")
  expect_true(convertCategoriesToLibraryStrategyType("CHIP")$SRA_library_strategy == "ChIP-Seq")
  expect_true(convertCategoriesToLibraryStrategyType("ChIP")$SRA_library_strategy == "ChIP-Seq")
  expect_true(convertCategoriesToLibraryStrategyType("chip-seq")$SRA_library_strategy == "ChIP-Seq")
  
  #GEO
  expect_true(convertCategoriesToLibraryStrategyType("chip")$GEO_type == "Genome binding/occupancy profiling by high throughput sequencing")
  expect_true(convertCategoriesToLibraryStrategyType("CHIP")$GEO_type == "Genome binding/occupancy profiling by high throughput sequencing")
  expect_true(convertCategoriesToLibraryStrategyType("ChIP")$GEO_type == "Genome binding/occupancy profiling by high throughput sequencing")
  expect_true(convertCategoriesToLibraryStrategyType("chip-seq")$GEO_type == "Genome binding/occupancy profiling by high throughput sequencing")

})



test_that("Convert rnas", {
  
  #SRA
  expect_true(convertCategoriesToLibraryStrategyType("rna")$SRA_library_strategy == "RNA-Seq")
  expect_true(convertCategoriesToLibraryStrategyType("RNA")$SRA_library_strategy == "RNA-Seq")
  expect_true(convertCategoriesToLibraryStrategyType("Rna")$SRA_library_strategy == "RNA-Seq")
  expect_true(convertCategoriesToLibraryStrategyType("rna-seq")$SRA_library_strategy == "RNA-Seq")
  
  #GEO
  expect_true(convertCategoriesToLibraryStrategyType("rna")$GEO_type == "Expression profiling by high throughput sequencing")
  expect_true(convertCategoriesToLibraryStrategyType("RNA")$GEO_type == "Expression profiling by high throughput sequencing")
  expect_true(convertCategoriesToLibraryStrategyType("Rna")$GEO_type == "Expression profiling by high throughput sequencing")
  expect_true(convertCategoriesToLibraryStrategyType("rna-seq")$GEO_type == "Expression profiling by high throughput sequencing")
  
})



test_that("Convert a vector", {
  
  #SRA
  expect_true("ChIP-Seq" %in% convertCategoriesToLibraryStrategyType(c("chip", "rna"))$SRA_library_strategy)
  expect_true("RNA-Seq" %in% convertCategoriesToLibraryStrategyType(c("chip", "rna"))$SRA_library_strategy)
  
  #GEO
  expect_true("Genome binding/occupancy profiling by high throughput sequencing" %in% convertCategoriesToLibraryStrategyType(c("chip", "rna"))$GEO_type)
  expect_true("Expression profiling by high throughput sequencing" %in% convertCategoriesToLibraryStrategyType(c("chip", "rna"))$GEO_type)
  
  
})



test_that("Convert a SRA/GEO only category - expect length 0", {
  
  # SRA only
  expect_true(length(convertCategoriesToLibraryStrategyType(c("DNA NGS"))$GEO_type)==0)
  
  
  # GEO only
  expect_true(length(convertCategoriesToLibraryStrategyType(c("Transcriptome MA"))$SRA_library_strategy)==0)
  
  
})


