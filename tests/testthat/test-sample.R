context("Sample test")

test_that("Checking how testing works", {
  x <- 6
  y <- 3

  expect_true(TRUE)
  expect_true(1==1)
  expect_true(!(x==y))
  expect_warning(log(-1))
  expect_warning(log(-1), "NaNs produced")
})
