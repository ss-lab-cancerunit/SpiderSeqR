
parameterRecordGenerator <- function(st, file){

  file.create(file)
  y <- st

  for (s in seq_along(y)){

    #Order elements within each category (if multiple exist)
    if (length(y[[s]])>1){
      y[[s]] <- y[[s]][order(y[[s]])]
    }

    #Save names of list elements
    st_out <- names(y)[[s]]

    #Preserve 'NULL'
    if (is.null(y[[s]])){
      #y[[s]] <- "NULL"
      y[[s]] <- ""
    }

    #Append values to the names and collapse
    st_out <- append(st_out, y[[s]])
    st_out <- paste(st_out, collapse = "\t")

    #print(st_out)
    cat(st_out, file = file, sep = "\n", append = TRUE)
  }
}



callRecordGenerator <- function(file){
  c <- match.call(def = sys.function(-1), call = sys.call(-1))
  saveRDS(c, file = file)
}



reproduceResults <- function(file){
  x <- readRDS(file)

  #Using output of callRecordGenerator
  if (class(x)=="call"){
    eval(x)
  }

  #Using output of parameterRecordGenerator (or equivalent for searchForAccession)
  #===*===
}

