
#' Console Messages
#' 
#' @param string A character to be displayed
#' @param type A type of message
#' @return Console message
#' 
#' @keywords internal
#' 
.mm <- function(string, type){
    type_list <- c("fn", # Beginnings and ends of function calls 
                    "res", # Numbers of samples, etc.
                    "adverse", # Warnings about some adverse events
                    "query", # SQLite query
                    "search", # Search parameters
                    "prog") # Progress
    type <- match.arg(type, type_list, several.ok = FALSE)
    
    
    if(!getSpiderSeqROption("quiet")){
        print(string)
    }
}




#invisible()



#' Assign local variables to global environment (development mode)
#' 
#' @param name A string with the name of the local variable
#' @param value The local variable
#' @return Nothing. Create the variable in the global environment
#' 
#' @keywords internal
#' 
.vex <- function(name, value){
    
    #assign(x = name, value = value, get(".GlobalEnv"))
    
    #assign(x = paste0(name, "_1"), value = value, .GlobalEnv)
    #assign(x = paste0(name, "_2"), value = value, get(".GlobalEnv"))
    #print(get(".GlobalEnv"))
    
    if (getSpiderSeqROption("internal")){
        assign(x = name, value = value, envir = get(".GlobalEnv"))
    }
}




