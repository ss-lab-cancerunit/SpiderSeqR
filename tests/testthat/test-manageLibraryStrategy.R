
setSpideROption("quiet", TRUE)


#manageLibraryStrategy("RNA", input = "syn", output = "can", mismatch.ignore = FALSE)
#manageLibraryStrategy("RNA", input = "syn", output = "can", mismatch.ignore = TRUE)



test_that("Incorrect conversions are not allowed", {
  
  expect_error(manageLibraryStrategy("RNA", input = "can", output = "can"))
  expect_error(manageLibraryStrategy("RNA", input = "short", output = "short"))
  expect_error(manageLibraryStrategy("RNA", input = "syn", output = "syn"))
  
  expect_error(manageLibraryStrategy("RNA", input = "can", output = "syn"))
  expect_error(manageLibraryStrategy("RNA", input = "syn", output = "short"))
  expect_error(manageLibraryStrategy("RNA", input = "short", output = "syn"))
  expect_error(manageLibraryStrategy("RNA", input = "short", output = "can"))
  
  
})



test_that("Conversion: can->short", {
  expect_true("RNA"==manageLibraryStrategy("RNA-Seq", input = "can", output = "short", mismatch.ignore = FALSE))
  expect_true("ChIP"==manageLibraryStrategy("ChIP-Seq", input = "can", output = "short", mismatch.ignore = FALSE))
  
})


test_that("Conversion: syn->can", {
  expect_true("RNA-Seq"==manageLibraryStrategy("RNA-Seq", input = "syn", output = "can", mismatch.ignore = FALSE))
  expect_true("RNA-Seq"==manageLibraryStrategy("RNA", input = "syn", output = "can", mismatch.ignore = FALSE))
  
})



test_that("Mismatch.ignore", {
  
  expect_error(manageLibraryStrategy("RN", input = "syn", output = "can", mismatch.ignore = FALSE))
  expect_true("RN"==manageLibraryStrategy("RN", input = "syn", output = "can", mismatch.ignore = TRUE))
  
  expect_error(manageLibraryStrategy("RN", input = "can", output = "short", mismatch.ignore = FALSE))
  expect_true("RN"==manageLibraryStrategy("RN", input = "can", output = "short", mismatch.ignore = TRUE))
  
  
})

test_that("Canonical check", {
  expect_true(manageLibraryStrategy("ChIP-Seq", task = "check_can"))
  expect_false(manageLibraryStrategy("ChIP", task = "check_can"))
  
  expect_true(manageLibraryStrategy("RNA-Seq", task = "check_can"))
  expect_false(manageLibraryStrategy("RNA", task = "check_can"))
  expect_false(manageLibraryStrategy("sth", task = "check_can"))
  
  expect_true(manageLibraryStrategy("WGS", task = "check_can"))
})


test_that("Canonical check", {

  expect_true(is.list(manageLibraryStrategy(task = "ex")))
  expect_true(is.list(manageLibraryStrategy(task = "ex", input = "can", output = "short")))
  expect_true(is.list(manageLibraryStrategy(task = "ex", input = "syn", output = "can")))
  
  
})





