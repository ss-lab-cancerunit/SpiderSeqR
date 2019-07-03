context("convertCategoriesToLibraryStrategyType")



test_that("Checking errors", {
  expect_error(convertCategoriesToLibraryStrategyType("no match"))
  expect_error(convertCategoriesToLibraryStrategyType(NA))
  expect_error(convertCategoriesToLibraryStrategyType(NULL))
})



test_that("Convert chips", {
  
  #SRA
  expect_true(convertCategoriesToLibraryStrategyType("chip")$sra_library_strategy == "ChIP-Seq")
  expect_true(convertCategoriesToLibraryStrategyType("CHIP")$sra_library_strategy == "ChIP-Seq")
  expect_true(convertCategoriesToLibraryStrategyType("ChIP")$sra_library_strategy == "ChIP-Seq")
  expect_true(convertCategoriesToLibraryStrategyType("chip-seq")$sra_library_strategy == "ChIP-Seq")
  
  #GEO
  expect_true(convertCategoriesToLibraryStrategyType("chip")$geo_type == "Genome binding/occupancy profiling by high throughput sequencing")
  expect_true(convertCategoriesToLibraryStrategyType("CHIP")$geo_type == "Genome binding/occupancy profiling by high throughput sequencing")
  expect_true(convertCategoriesToLibraryStrategyType("ChIP")$geo_type == "Genome binding/occupancy profiling by high throughput sequencing")
  expect_true(convertCategoriesToLibraryStrategyType("chip-seq")$geo_type == "Genome binding/occupancy profiling by high throughput sequencing")

})



test_that("Convert rnas", {
  
  #SRA
  expect_true(convertCategoriesToLibraryStrategyType("rna")$sra_library_strategy == "RNA-Seq")
  expect_true(convertCategoriesToLibraryStrategyType("RNA")$sra_library_strategy == "RNA-Seq")
  expect_true(convertCategoriesToLibraryStrategyType("Rna")$sra_library_strategy == "RNA-Seq")
  expect_true(convertCategoriesToLibraryStrategyType("rna-seq")$sra_library_strategy == "RNA-Seq")
  
  #GEO
  expect_true(convertCategoriesToLibraryStrategyType("rna")$geo_type == "Expression profiling by high throughput sequencing")
  expect_true(convertCategoriesToLibraryStrategyType("RNA")$geo_type == "Expression profiling by high throughput sequencing")
  expect_true(convertCategoriesToLibraryStrategyType("Rna")$geo_type == "Expression profiling by high throughput sequencing")
  expect_true(convertCategoriesToLibraryStrategyType("rna-seq")$geo_type == "Expression profiling by high throughput sequencing")
  
})



test_that("Convert a vector", {
  
  #SRA
  expect_true("ChIP-Seq" %in% convertCategoriesToLibraryStrategyType(c("chip", "rna"))$sra_library_strategy)
  expect_true("RNA-Seq" %in% convertCategoriesToLibraryStrategyType(c("chip", "rna"))$sra_library_strategy)
  
  #GEO
  expect_true("Genome binding/occupancy profiling by high throughput sequencing" %in% convertCategoriesToLibraryStrategyType(c("chip", "rna"))$geo_type)
  expect_true("Expression profiling by high throughput sequencing" %in% convertCategoriesToLibraryStrategyType(c("chip", "rna"))$geo_type)
  
  
})



test_that("Convert a SRA/GEO only category - expect length 0", {
  
  # SRA only
  expect_true(length(convertCategoriesToLibraryStrategyType(c("DNA NGS"))$geo_type)==0)
  
  
  # GEO only
  expect_true(length(convertCategoriesToLibraryStrategyType(c("Transcriptome MA"))$sra_library_strategy)==0)
  
  
})


