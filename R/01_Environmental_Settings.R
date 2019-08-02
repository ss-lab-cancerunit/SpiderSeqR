#' SpideR environment
#' 
spiderEnv <- new.env()




varInSpider <- function(){
  assign("x", 1, spiderEnv)
  print(spiderEnv)
}

setVar <- function(value){
  assign("x", value, spiderEnv)
  print(spiderEnv)
}

getVar <- function(name){
  print(spiderEnv)
  get(name, envir = spiderEnv)
}


printVar <- function(name){
  print(get("x", envir = spiderEnv, inherits = FALSE))
  print(spiderEnv)
}

#> varInSpider()
#> printVar()
#[1] 1
#> spiderEnv$x <- 3
#> printVar()
#[1] 1
#> setVar(4)
#> printVar()
#[1] 4
#> rm(spiderEnv)
#> printVar()
#[1] 4


