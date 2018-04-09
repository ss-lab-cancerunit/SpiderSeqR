
parameterRecordGenerator <- function(st, file, fun_name){

  file.create(file)
  y <- st

  cat(fun_name, file = file, sep = "\n") #Save function name

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

  ext_Rda <- grepl("*.Rda$", file)
  ext_tab <- grepl("*.tab$", file)

  if (ext_Rda & ext_tab){
    stop("Something went wrong - two extensions?")
  }


  #====================================================
  # RDA
  #====================================================

  if (ext_Rda){
    x <- readRDS(file)

    #Using output of callRecordGenerator
    if (class(x)=="call"){
      eval(x)
    }
  }

  #====================================================


  #====================================================
  # TAB
  #====================================================

  if (ext_tab){

    x <- readLines(file)


    #====================================================
    # searchForTerm
    #====================================================
    if (x[[1]]=="searchForTerm"){

      x <- x[-1]

      st_template <- c("library_strategy",
                       "gene",
                       "antibody",
                       "cell_type",
                       "treatment",
                       "species",
                       "platform",
                       "secondary_library_strategy")

      if ((length(st_template))!=length(x)){
        stop("The number of lines in the input file needs to correspond to the number of input variables")
      }


      #rec <- list()
      rec <- rep(list(NULL), length(st_template))
      names(rec) <- st_template

      for (r in seq_along(st_template)){
        temp <- unlist(strsplit(x[[r]], split = "\t"))
        if (st_template[r]!=temp[1]){
          stop("The names need to match between the input and the template")
        }

        if (length(temp)>1){
          rec[[r]] <- temp[-1] #Omit the first element (name)
        }
      }

      do.call(searchForTerm, rec)
    }




    #====================================================
    # searchForAccession (in the future)
    #====================================================

    #===*====
    #To be done

    if (x[[1]]=="searchForAccession"){
      print("Work more...!")
    }


  }

  #====================================================


  #Using output of parameterRecordGenerator (or equivalent for searchForAccession)
  #===*===
}

