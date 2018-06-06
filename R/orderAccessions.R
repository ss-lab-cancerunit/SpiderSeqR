
#IN PROGRESS - seems to work, but probably bugs exist

#Some little examples for testing
x <- c("2", "4r", NA, "e", "33")
x <- c("4", NA, "2")
x <- c("3", NA, "2", "1", NA)
x <- c("d3", "f1", "NA", "", "2f4", "g77")
x <- c("SRP1", "SRP01", "SRP022", "SRP23")


#=============================================================
# Functions for testing an ordering function
#=============================================================
shuffleList <- function(x){
  # Function for shuffling 2nd levels of lists (designed to work on a list of vectors)
  # Args:
  # x - list to be shuffled
  # Returns:
  # shuffled x (relationships between levels are preserved)
  subs <- length(x)
  ind <- sample(1:length(x[[1]]), length(x[[1]]))
  for (s in 1:subs){
    if (length(ind)!=length(x[[s]])){
      stop("Sublists must have the same length")
    }
    x[[s]] <- x[[s]][ind]
  }
  return(x)
}

#=============================================================

checkOrder <- function(x, order_function){
  # A function which shuffles lists and uses an ordering function to order the list
  # Args:
  # x - list (should be in desired order!)
  # order_function - name of the ordering function to be used
  # Returns:
  # logical (whether ordering led to identical list as provided on input)
  # ALSO: prints results etc.
  #
  print("Original list:")
  print(x)
  
  x_original <- x # Make a copy
  
  x <- shuffleList(x)
  print("Shuffled list:")
  print(x)
  
  x_shuffled <- x # Store shuffled x
  
  for (i in seq_along(x)){
    #print(do.call(order_function, x_shuffled))
    x[[i]] <- x[[i]][do.call(order_function, x_shuffled)]
  }
  
  print("List after ordering:")
  print(x)
  
  outcome <- identical(x_original, x)
  if (outcome){
    print("Ordering was successful!")
  }
  
  return(outcome)

}


#=============================================================
#=============================================================


a1 <- c(rep(1, 3), rep(2, 3), rep(3, 3))
a2 <- c(11:13, 21:23, 31:33)
a <- list(a1, a2)

checkOrder(a, "order") # A demo for order() function

checkOrder(list(1:10), "orderAccessions") # This works

checkOrder(a, "orderAccessions") # Does not seem to work atm





#Modified version for dealing with lists
orderAccessions <- function(x, na.last = TRUE){
  #Function for ordering accessions
  # - ignores alphanumeric strings (removes them for the purpose of ordering)
  # - returns order based on the order of numeric remainder
  # - NAs are treated separately - and returned at the end (unless na.last = FALSE)

  print("Running orderAccessions")

  if(class(x)!="list"){

    #Convert "NA" and "" to NA
    x <- naConverter(x)

    x_i <- rep(0, length(x))

    #Indices of NA
    x_i_na <- which(is.na(x))
    print("x_i_na")
    print(x_i_na)

    #Indices of non-NA values
    x_i_val <- which(!is.na(x))
    print("x_i_val")
    print(x_i_val)

    #Stop if the values do not conform to alphanumeric requirement
    #Might relieve this requirement in the future
    if ( sum(grepl("^[[:alnum:]]*$", x[x_i_val])) != length(x[x_i_val]) ){
      stop("Only alphanumeric characters are allowed")
    }

    #Remove alphanumeric characters and convert remainder to numeric
    x_num <- as.numeric(gsub("[[:alpha:]]", "", x[x_i_val]))
    print("x_num")
    print(x_num)

    #Order numeric remainders
    #x_i_num - vector of ordered
    x_i_num <- order(x_num)
    print("x_i_num")
    print(x_i_num)

    x_i_val <- x_i_val[x_i_num]

    print("x_i_val")
    print(x_i_val)

    if (na.last == TRUE){
      x_i <- c(x_i_val, x_i_na)
    }
    if (na.last == FALSE){
      x_i <- c(x_i_na, x_i_val)
    }
    
  } else {

    print(x)

    #PROCEDURE FOR LISTS

    #INITIALISE SOMETHING...???

    t <- x #Make a copy of x

    for (i in seq_along(x)){

      #Convert "NA" and "" to NA
      x[[i]] <- naConverter(x[[i]])

      x_i <- rep(0, length(x)) #Keeping it as a vector (will get overwritten on each iteration)

      #Indices of NA #Seem unneeded here
      #x_i_na <- which(is.na(x[[i]]))
      #print("x_i_na")
      #print(x_i_na)

      #Indices of non-NA values
      x_i_val <- which(!is.na(x[[i]]))
      print("x_i_val")
      print(x_i_val)

      #Stop if the values do not conform to alphanumeric requirement
      #Might relieve this requirement in the future
      if ( sum(grepl("^[[:alnum:]]*$", x[[i]][x_i_val])) != length(x[[i]][x_i_val]) ){
        stop("Only alphanumeric characters are allowed")
      }

      #Remove alphanumeric characters and convert remainder to numeric
      t[[i]][x_i_val] <- as.numeric(gsub("[[:alpha:]]", "", x[[i]][x_i_val]))
      t[[i]] <- as.numeric(t[[i]])
      print(t[[i]])


    }

    #x_i <- do.call(order, list(x=t, na.last = na.last)) #t is already a list #Does not work
    #x_i <- do.call(order, t) #t is already a list #Minimal version, which does not implement na.last
    x_i <- do.call(order, c(x=t, na.last = na.last))

  }
  print(x_i)



  print("orderAccessions completed")
  return(x_i)
  
}







#Working version for vectors
orderAccessions_prev <- function(x, na.last = TRUE){
  #Function for ordering accessions
  # - ignores alphanumeric strings (removes them for the purpose of ordering)
  # - returns order based on the order of numeric remainder
  # - NAs are treated separately - and returned at the end (unless na.last = FALSE)

  print("Running orderAccessions")

  #Convert "NA" and "" to NA
  x <- naConverter(x)

  x_i <- rep(0, length(x))

  #Indices of NA
  x_i_na <- which(is.na(x))
  print("x_i_na")
  print(x_i_na)

  #Indices of non-NA values
  x_i_val <- which(!is.na(x))
  print("x_i_val")
  print(x_i_val)

  #Stop if the values do not conform to alphanumeric requirement
  #Might relieve this requirement in the future
  if ( sum(grepl("^[[:alnum:]]*$", x[x_i_val])) != length(x[x_i_val]) ){
    stop("Only alphanumeric characters are allowed")
  }

  #Remove alphanumeric characters and convert remainder to numeric
  x_num <- as.numeric(gsub("[[:alpha:]]", "", x[x_i_val]))
  print("x_num")
  print(x_num)

  #Order numeric remainders
  #x_i_num - vector of ordered
  x_i_num <- order(x_num)
  print("x_i_num")
  print(x_i_num)

  x_i_val <- x_i_val[x_i_num]

  print("x_i_val")
  print(x_i_val)

  if (na.last == TRUE){
    x_i <- c(x_i_val, x_i_na)
  }
  if (na.last == FALSE){
    x_i <- c(x_i_na, x_i_val)
  }

  print("orderAccessions completed")
  return(x_i)
}





#digitSort copied here for convenience (remove when done)
#will eventually become obsolete
#remember to replace it with sortAccessions (a wrapper around orderAccessions)

digitSort <- function(inp){
  # Args: a vector or a list of vectors
  #          of the following form:
  #            [A-Za-z]+[0-9]+
  #          (throws an error if vectors do not conform)
  #
  # Returns: a vector of indices
  #          ordered according to v1, (v2, v3... )
  #
  # Ordering disregards any letters occurring before the digits
  #          i.e. c("A2", "B1", "C3") will yield 2, 1, 3
  #
  print("Running digitSort")

  if (class(inp)!="list"){
    check <- grepl("[A-Za-z]+[0-9]+", inp)
    if (sum(check)!=length(inp)){
      stop("Vector needs to be in [A-Za-z]+[0-9]+ format")
    }
    inp_extr <- gsub("([A-Za-z]+)([0-9]+)", "\\2", inp)
    #print(inp_extr)
    inp_order <- do.call(order, list(inp_extr)) #inp_extr needs to be converted to a list

  } else {
    inp_extr <- list()
    for (el in seq_along(inp)){
      check <- grepl("[A-Za-z]+[0-9]+", inp[[el]])
      if (sum(check)!=length(inp[[el]])){
        stop("Vector needs to be in [A-Za-z]+[0-9]+ format")
      }
      inp_extr[[el]] <- gsub("([A-Za-z]+)([0-9]+)", "\\2", inp[[el]])
    }
    inp_order <- do.call(order, inp_extr) #inp_extr is already a list
  }
  print("digitSort completed")
  return(inp_order)
}
