
context("orderAccessions (lists)")

#TO DO:
#- Develop more tests for more complicated cases in a more systematic way

sample_list <- function(x){
  #Function to sample from lists
  #(treating each sublist as if it was a column of df and rearranging row order)

  i <- seq_along(x[[1]]) #Determine the length of vector (if they are different, order will throw an error anyway...)
  i <- sample(i)

  for (el in seq_along(x)){
    x[[el]] <- x[[el]][i]
  }
  return(x)
}

reorder_list <- function(x, ord){
  #Function to reorder elements in each sublist
  for (el in seq_along(x)){
    x[[el]] <- x[[el]][ord]
  }
  return(x)
}


test_orderAccessions_list <- function(x){
  #A testing function that scrambles a list (which was originally ordered) and compares it against the original
  x_sample <- sample_list(x)

  expect_true(isTRUE(identical(x, reorder_list(x_sample, orderAccessions(x_sample)))))
  #expect_true(isTRUE(identical(x, x_sample[orderAccessions(x_sample)] )))
}








test_that("Basic tests", {
  x1 <- list(a = 1:5, b = c(5,4,3,2,1))
  test_orderAccessions_list(x1)

  x2 <- list(a = c(1,1,1,2,2,2,3,3,3), b = c(1,2,3,4,5,6,7,8,9))
  test_orderAccessions_list(x2)

  x3 <- list(a = c(1,1,1,2,2,2,3,3,3), b = c(8,9,10, 3,4,5, 6,11,12))
  test_orderAccessions_list(x3)

  x4 <- list(a = c(rep("b1",3), rep("c2", 3), rep("a3", 3)), b = c("c3", "a4", "b7", "11", "0120", "h200", "f3", "a33", "b0333"))
  test_orderAccessions_list(x4)

  x5 <- list(a = c(rep("z02", 3), rep("g22",3), rep("0200", 3)), b = c("1", "2", "3", "g4", "jj05", "230", "h55", "zz0133" , NA))
  test_orderAccessions_list(x5)

  #This only works, because only one NA is present for a distinct 'a' element
  x6 <- list(a = c(rep("z02", 3), rep("g22",3), rep("0200", 3)), b = c("1", "2", "", "g4", "jj05", "NA", "h55", "zz0133" , NA))
  test_orderAccessions_list(x6)


})


test_that("Realistic examples", {

  x1 <- list(a = c(rep("SRP001", 3), rep("SRP2",3), rep("SRP200", 3), rep("SRP1000", 3)), b = c("SRR333", "SRR334", "NA", "SRR01",  "SRR10", "SRR100", "SRR05", "SRR50", "", "SRR00", "SRR100", NA))
  test_orderAccessions_list(x1)

})

