setSpiderSeqROption("quiet", TRUE)


context("orderAccessions (vectors)")


#Ideas for future testing
# - more on warnings and errors?
# - what happens if no numeric remains after subtraction
# - handling NAs, "" and "NA" and introducing some hierarchy?



test_orderAccessions <- function(x){
  #A testing function that scrambles a vector (which was originally ordered) and compares it against the original
  x_sample <- sample(x)
  expect_true(isTRUE(all.equal(x, x_sample[orderAccessions(x_sample)] )))
}



test_that("Basic tests", {

  x1 <- c(1,2,3)
  test_orderAccessions(x1)

  x2 <- c("c1", "b2", "a3")
  test_orderAccessions(x2)

})


test_that("Warnings", {
  expect_error(orderAccessions(c("1", "2", "3", "..")))
  expect_error(orderAccessions(c("1", "2", "3", "4.")))
})



test_that("Simple examples of [a-z][1-9] of length=9", {
  nu <- as.character(1:9)
  alpha <- c("a", "b", "c", "d", "e", "f", "g", "h", "i")

  x3 <- paste0(sample(alpha), nu)
  x4 <- paste0(sample(alpha), nu)
  x5 <- paste0(sample(alpha), nu)

  test_orderAccessions(x3)
  test_orderAccessions(x4)
  test_orderAccessions(x5)

})



test_that("Simple examples of [a-z][1-9] of length=15", {

  nu <- as.character(1:15)
  alpha <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o")

  t1 <- paste0(sample(alpha), nu)
  t2 <- paste0(sample(alpha), nu)
  t3 <- paste0(sample(alpha), nu)


  #test_digitSort(nu);
  #test_digitSort(alpha);
  set.seed(001)
  test_orderAccessions(t1)
  set.seed(002)
  test_orderAccessions(t2)
  set.seed(003)
  test_orderAccessions(t3)
})



test_that("Simple examples of [a-z][1-9] of length=15 with leading zeros", {

  nu <- as.character(1:15)
  zeros <- sample(c(rep("0", 5), rep("", 10)))
  alpha <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o")

  t1 <- paste0(sample(alpha), zeros, nu)
  t2 <- paste0(sample(alpha), zeros, nu)
  t3 <- paste0(sample(alpha), zeros, nu)


  #test_digitSort(nu);
  #test_digitSort(alpha);
  set.seed(001)
  test_orderAccessions(t1)
  set.seed(002)
  test_orderAccessions(t2)
  set.seed(003)
  test_orderAccessions(t3)
})



test_that("Simple examples of [a-z][1-9] of length=15 with leading zeros and NAs", {

  nu <- as.character(1:15)
  zeros <- sample(c(rep("0", 5), rep("", 10)))
  alpha <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o")

  t1 <- paste0(sample(alpha), zeros, nu)
  t2 <- paste0(sample(alpha), zeros, nu)
  t3 <- paste0(sample(alpha), zeros, nu)

  t1 <- c(t1, rep(NA, 5))
  t2 <- c(t2, rep("NA", 5))
  t3 <- c(t3, rep("", 5))


  #test_digitSort(nu);
  #test_digitSort(alpha);
  set.seed(001)
  test_orderAccessions(t1)
  set.seed(002)
  test_orderAccessions(t2)
  set.seed(003)
  test_orderAccessions(t3)
})



test_that("Realistic examples with NAs", {

  x1 <- c("SRP001", "SRP002", "SRP23", "SRP2500", "SRP87889")
  test_orderAccessions(x1)

  x2 <- c("SRP0001", "SRP002", "SRP12", "SRP0120", "SRP01200")
  test_orderAccessions(x2)


  x3 <- c("SRP001", "SRP002", "SRP23", "SRP2500", "SRP87889", "NA")
  test_orderAccessions(x3)

  x4 <- c("SRP001", "SRP002", "SRP23", "SRP2500", "SRP87889", rep("NA", 5))
  test_orderAccessions(x4)


  x5 <- c("SRP001", "SRP002", "SRP23", "SRP2500", "SRP87889", "")
  test_orderAccessions(x5)

  x6 <- c("SRP001", "SRP002", "SRP23", "SRP2500", "SRP87889", rep("", 5))
  test_orderAccessions(x6)


  x7 <- c("SRP001", "SRP002", "SRP23", "SRP2500", "SRP87889", NA)
  test_orderAccessions(x7)

  x8 <- c("SRP001", "SRP002", "SRP23", "SRP2500", "SRP87889", rep(NA, 5))
  test_orderAccessions(x8)




})



test_that("Realistic examples with mixed NA types. Not working", {
  #Hierarchy of NA values not established, so treated in the same way

  x1 <- c("SRP001", "SRP002", "SRP23", "SRP2500", "SRP87889", "NA", "")
  test_orderAccessions(x1)

  x2 <- c("SRP001", "SRP002", "SRP23", "SRP2500", "SRP87889", "NA", "", NA)
  test_orderAccessions(x2)
})






#Some ideas
#x <- c("c1", "b2", NA, "c")
#x <- c("2", "4r", NA, "e", "33")
#x <- c("4", NA, "2")
#x <- c("3", NA, "2", "1", NA)
#x <- c("d3", "f1", "NA", "", "2f4", "g77")
#x <- c("SRP1", "SRP01", "SRP022", "SRP23")

