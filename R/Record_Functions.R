

#' Generate a record of parameters
#' 
#' @param st A list of arguments
#' @param file A string with the file name
#' @param fun_name A string with the function name
#' @return Nothing. Create a file with the record
#' 
#' @keywords internal
#' 
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



#' Generate record of the call
#' 
#' @param file A string with the file name (needs to be .Rda to work)
#' @return Nothing. Saves the file with call object
#' 
#' @keywords internal
#' 
callRecordGenerator <- function(file){
    c <- match.call(definition = sys.function(-1), call = sys.call(-1))
    saveRDS(c, file = file)
}





#' Generate call file for searchForAccession
#' 
#' @param acc_vector A character vector with accessions searched for
#' @return A character vector with the file name
#' 
#' Current format:
#' 
#' SFA_ACC1_ACCN_Nn_d-a-te.Rda
#' 
#' @keywords internal
#' 
generateCallFile_SFA <- function(acc_vector){
    
    mm("Running generateCallFileName", "fn")
    
    name <- "SFA_"
    acc_vector <- unique(acc_vector)
    
    acc_vector <- acc_vector[orderAccessions(acc_vector)]
    
    
    if (length(acc_vector) > 2){
        acc_name <- paste0(acc_vector[c(1, length(acc_vector))], 
                            collapse = "-")
        acc_name <- paste0(acc_name, "_", length(acc_vector), "n")
        
    } else {
        acc_name <- paste0(acc_vector, collapse = "-")
    }
    
    name <- paste0(name, acc_name)
    
    today <- Sys.time()
    today <- format(today, format = "%Y-%m-%d-%H%M%S")
    
    name <- paste0(name, "_", today)
    
    name <- paste0(name, ".Rda") 
    
    mm("generateCallFileName completed", "fn")
    return(name)
    
}






#' Rerun SpiderSeqR query
#' 
#' @param file Query record generated at the time of query
#' @return Rerun the query
#' 
#' @description 
#' This function allows for repeating the same query without 
#' the need to re-type the parameters over and over again. 
#' The main application of this function is to update results 
#' after the database files have been updated 
#' (i.e. to check whether there are any more results 
#' than last time the query was run).
#' 
#' @examples 
#' # rerunSpiderSeqR("filename.Rda")
#' 
#' @export
#' 
#' 
rerunSpiderSeqR <- function(file){
    
    ext_Rda <- grepl("*.Rda$", file)
    ext_tab <- grepl("*.tab$", file)
    
    if (ext_Rda & ext_tab){
        stop("Something went wrong - two extensions?")
    }
    
    if (!(ext_Rda | ext_tab)){
        stop("Only Rda and tab files are accepted")
    }
    
    
    #====================================================
    # RDA
    #====================================================
    
    if (ext_Rda){
        x <- readRDS(file)
        
        #Using output of callRecordGenerator
        if (methods::is(x, "call")){
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
                stop(paste0("The number of lines in the input file ",
                "needs to correspond to the number of input variables"))
            }
            
            
            #rec <- list()
            rec <- rep(list(NULL), length(st_template))
            names(rec) <- st_template
            
            for (r in seq_along(st_template)){
                temp <- unlist(strsplit(x[[r]], split = "\t"))
                if (st_template[r]!=temp[1]){
                    stop(paste0("The names need to match between ",
                    "the input and the template"))
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
    
    
    #Using output of parameterRecordGenerator 
    # (or equivalent for searchForAccession)
    #===*===
}

