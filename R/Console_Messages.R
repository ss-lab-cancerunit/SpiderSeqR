
#' Console Messages
#' 
#' @param string A character to be displayed
#' @param type A type of message
#' @return Console message
#' 
#' @keywords internal
#' 
mm <- function(string, type){
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


