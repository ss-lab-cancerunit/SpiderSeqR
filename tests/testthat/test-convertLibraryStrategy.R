

#convertLibraryStrategy("RNA", input = "syn", output = "can", mismatch.ignore = FALSE)
#convertLibraryStrategy("RNA", input = "syn", output = "can", mismatch.ignore = TRUE)


test_that("Incorrect conversions are not allowed", {
  
  expect_error(convertLibraryStrategy("RNA", input = "can", output = "can"))
  expect_error(convertLibraryStrategy("RNA", input = "short", output = "short"))
  expect_error(convertLibraryStrategy("RNA", input = "syn", output = "syn"))
  
  expect_error(convertLibraryStrategy("RNA", input = "can", output = "syn"))
  expect_error(convertLibraryStrategy("RNA", input = "syn", output = "short"))
  expect_error(convertLibraryStrategy("RNA", input = "short", output = "syn"))
  expect_error(convertLibraryStrategy("RNA", input = "short", output = "can"))
  
  
})



test_that("Conversion: can->short", {
  expect_true("RNA"==convertLibraryStrategy("RNA-Seq", input = "can", output = "short", mismatch.ignore = FALSE))
  expect_true("ChIP"==convertLibraryStrategy("ChIP-Seq", input = "can", output = "short", mismatch.ignore = FALSE))
  
})


test_that("Conversion: syn->can", {
  expect_true("RNA-Seq"==convertLibraryStrategy("RNA-Seq", input = "syn", output = "can", mismatch.ignore = FALSE))
  expect_true("RNA-Seq"==convertLibraryStrategy("RNA", input = "syn", output = "can", mismatch.ignore = FALSE))
  
})



test_that("Mismatch.ignore", {
  
  expect_error(convertLibraryStrategy("RN", input = "syn", output = "can", mismatch.ignore = FALSE))
  expect_true("RN"==convertLibraryStrategy("RN", input = "syn", output = "can", mismatch.ignore = TRUE))
  
  expect_error(convertLibraryStrategy("RN", input = "can", output = "short", mismatch.ignore = FALSE))
  expect_true("RN"==convertLibraryStrategy("RN", input = "can", output = "short", mismatch.ignore = TRUE))
  
  
})


