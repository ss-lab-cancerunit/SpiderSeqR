
#IN PROGRESS - seems to work, but probably bugs exist

#Some little examples for testing
x <- c("2", "4r", NA, "e", "33")
x <- c("4", NA, "2")
x <- c("3", NA, "2", "1", NA)





orderAccessions <- function(x, na.last = TRUE){

  x_i <- rep(0, length(x))

  x_i_na <- which(is.na(x))

  x_i_val <- which(!is.na(x))

  x_num <- as.numeric(gsub("[[:alpha:]]", "", x[x_i_val]))
  print(x_num)
  x_i_num <- order(x_num)

  x_i_val <- x_i_val[x_i_num]

  if (na.last == TRUE){
    x_i <- c(x_i_val, x_i_na)
  }
  if (na.last == FALSE){
    x_i <- c(x_i_na, x_i_val)
  }
  return(x_i)
}



#Completely incomplete - wrong function created (rank instead of order...)
orderAccessions <- function(x, na.last = TRUE){
  x_i <- rep(0, length(x))

  if (na.last == TRUE){
    x_i[is.na(x)] <- seq(from=(length(x)+1-sum(is.na(x))), to = length(x))
  }

  if (na.last == FALSE){
    x_i[is.na(x)] <- seq(from=1, to = sum(is.na(x)))
  }



  x_val <- x[!is.na(x)]
  x_val <- gsub("[[:alpha:]]", "", x_val)
  x_val <- as.numeric(x_val)
  x_val_i <- order(x_val)
  print(x_val)
  print(x_val_i)


  if (na.last == FALSE){
    x_val_i <- x_val_i + sum(is.na(x))
  }

  x_i[!is.na(x)] <- x_val_i

  return(x_i)
}




#Initial ideas (WRONG!)
orderAccessions <- function(x, na.last = TRUE){


  #Remove NAs and save their indices
  if (sum(is.na(x))>0){
    x_i_na <- which(is.na(x))
    x_val <- x[!is.na(x)]
  }

  #Stop if the values do not conform to alphanumeric requirement
  #Might relieve this requirement in the future
  if ( sum(grepl("^[[:alnum:]]*$", x_val)) != length(x_val) ){
    stop("Only alphanumeric characters are allowed")
  }

  x_num <- gsub("[[:alpha:]]", "", x_val)

  x_i_num <- order(as.numeric(x_num))

  if (na.last == TRUE){
    x_i <- c(x_i_num, x_i_na)
  }

  if (na.last == FALSE){
    x_i <- c(x_i_na, x_i_num)
  }

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
