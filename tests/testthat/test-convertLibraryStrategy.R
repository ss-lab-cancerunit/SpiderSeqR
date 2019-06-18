

#can->short



#syn->can
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

test_that("Can->Short Conversion", {
  expect_true("RNA"==convertLibraryStrategy("RNA-Seq", input = "can", output = "short", mismatch.ignore = FALSE))
  expect_error(convertLibraryStrategy("RN", input = "syn", output = "can", mismatch.ignore = FALSE))

  
  
})

