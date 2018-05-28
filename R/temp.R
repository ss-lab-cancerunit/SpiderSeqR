

# Should run smoothly

#' @export
testfn <- function(){
  x <- data.frame(a=1:3, b=4:6, c=7:9)
  #x <- x %>% sum(1)
  #x <- x %>% dplyr::mutate(n=seq_along(a))
  x <- x %>% dplyr::group_by(a) %>% dplyr::mutate(n=seq_along(a))
  return(x)
}



# Should give an error (dplyr function not available if not previously loaded)

#' @export
testfn2 <- function(){
  x <- data.frame(a=1:3, b=4:6, c=7:9)
  #x <- x %>% sum(1)
  #x <- x %>% dplyr::mutate(n=seq_along(a))
  x <- x %>% group_by(a) %>% mutate(n=seq_along(a))
  return(x)
}

#Verdict: import seems to be working - can apply these methods to the package functions
