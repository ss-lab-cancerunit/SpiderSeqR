
#' Console Messages
#' 
#' @param string A character to be displayed
#' @param type A type of message
#' @return Console message
#' 
#' @examples
#' ## Equivalents
#' # .mm(cli::rule(), "comm")
#' # cat(cli::rule(col = "magenta", "\n"))
#'  
#' # .mm(cli::rule(col = "magenta", left = "SEARCH DETAILS"), "search")
#' 
#' @keywords internal
#' 
.mm <- function(string, type){
    type_list <- c("adverse", # Messages about some adverse events ALWAYS
                    "qn", # Question to the user (requires action) ALWAYS
                    "comm",# Communicating to the user RECOMMENDED
                    "search", # Search parameters USEFUL
                    "res", # Numbers of samples, etc. HELPFUL
                    "prog", # Progress HELPFUL
                    "query", # SQLite query UNNECESSARY
                    "diag", # Diagnostic messages for the developer
                    "dev", # Information for developers
                    "fn") # Beginnings and ends of function calls 
    
    type <- match.arg(type, type_list, several.ok = FALSE)
    
    string <- paste0(string, "\n")
    
    if (type == "adverse"){
        adst <- crayon::combine_styles(crayon::red, crayon::bold)
        cat(adst("NOTE: ", string))
    } else if (type == "qn"){
        qnst <- crayon::combine_styles(crayon::magenta, crayon::bold)
        cat(qnst(string))
    } else if (type == "comm"){
        cat(crayon::magenta(string))
    } else if (type == "search"){
        srst <- crayon::combine_styles(crayon::blue, crayon::bold)
        cat(srst(string))
    } else if (type == "res"){
        cat(crayon::cyan(string))
    } else if (type == "prog"){
        cat(crayon::magenta(string))
    } else if (type == "query"){
        # do nothing
    } else if (type == "diag") {
        # do nothing
        # cat(crayon::yellow(string))
    } else if (type == "dev"){
        # do nothing
    } else if (type == "fn") {
        # do nothing
    } else if(!getSpiderSeqROption("quiet")){
        cat(string)
    }
}








#invisible()



#' Assign local variables to global environment (development mode)
#' 
#' @param name A string with the name of the local variable
#' @param value The local variable
#' @return Nothing. Create the variable in the global environment
#' 
#' @description This function is for development use only; 
#' it assigns variables from many of SpiderSeqR internal functions 
#' to the Global Environment for ease of code development.
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




