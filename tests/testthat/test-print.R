context("printing 1")

test_that("printing", {
  print("hi!")
  #print(ls(envir = .GlobalEnv))
  expect_equal(2 * 2, 4)
  expect_equal(2, get("x", envir = .GlobalEnv))
})
