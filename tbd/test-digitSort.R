context("digitSort")

#sample(vector)


#test_true <- function(x,y){
#  expect_true(isTRUE(all.equal(x,y)))
#}


test_digitSort <- function(x){
  #A testing function that scrambles a vector (which was originally ordered) and compares it against the original
  x_sample <- sample(x)
  expect_true(isTRUE(all.equal(x, x_sample[digitSort(x_sample)] )))
}



#nu <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
nu <- as.character(1:9)
alpha <- c("a", "b", "c", "d", "e", "f", "g", "h", "i")

t1 <- paste0(sample(alpha), nu)
t2 <- paste0(sample(alpha), nu)
t3 <- paste0(sample(alpha), nu)



test_that("Simple examples of [a-z][1-9] of length=9", {
  #test_digitSort(nu);
  #test_digitSort(alpha);
  set.seed(001)
  test_digitSort(t1)
  set.seed(002)
  test_digitSort(t2)
  set.seed(003)
  test_digitSort(t3)
})


nu <- as.character(1:15)
alpha <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o")

t1 <- paste0(sample(alpha), nu)
t2 <- paste0(sample(alpha), nu)
t3 <- paste0(sample(alpha), nu)


test_that("Simple examples of [a-z][1-9] of length=15. Currently not working", {
  #test_digitSort(nu);
  #test_digitSort(alpha);
  set.seed(001)
  test_digitSort(t1)
  set.seed(002)
  test_digitSort(t2)
  set.seed(003)
  test_digitSort(t3)
})


#To do:

#- leading zeros
#- different alpha-prefixes
#- NAs - of any type - at the end (NA: character, NA)

#t1 <- c("SRP1", "SRP2", "SRP3")
#t2 <- c("SRP0", "SRP1", "SRP09", "SRP10", "SRP101", "SRP1001")
#t3 <- c("SRP1", "ERP2", "SRP3", "SRP4", "DRP5", "SRP10", "ERP100")
#t4 <- c()
