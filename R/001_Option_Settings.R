# Default settings for SpiderSeqR options


#' Environment with default values of the package options
#' 
#' @description 
#' Environment for storing values determining certain aspects 
#' of package functioning.
#' To view the current value, please use \code{getSpiderSeqROption()}, 
#' to alter the current value, please use \code{setSpiderSeqROption()}.
#' 
#' 
SpiderSeqREnv <- new.env()


local({
    assign("file_output", TRUE)
    assign("output_columns", NULL) # Amend
    assign("quiet", FALSE)
    assign("internal", FALSE)
    assign("testing", FALSE)
}, SpiderSeqREnv)




#' Set options for functioning of the package
#' 
#' @param name A character with the name of a variable
#' @param value Value to be assigned to the variable
#' @return Nothing. Assign the value to a variable.
#' 
#' @description 
#' Set options for global behaviour of the package functions.
#' 
#' @section Available options:
#' \itemize{
#'     \item file_output - Logical - If TRUE, the search functions will produce 
#'     file outputs. If FALSE, no files will be created. Defaults to TRUE
#'    \item output_columns - Character vector - A set of columns to be used 
#'    for generating file outputs. 
#'    Defaults to one of the \code{listColumnSets()}.
#'    \item quiet - Whether any messages should be printed in the console
#'    \item testing - (for developer use) whether unit tests are being run
#'        
#' }
#' 
#' @examples 
#' startSpiderSeqRDemo()
#' setSpiderSeqROption("file_output", FALSE)
#' setSpiderSeqROption("output_columns", c("run_accession", "gsm"))
#' setSpiderSeqROption("output_columns", listColumnSets()$Accession)
#' 
#' @export
setSpiderSeqROption <- local({function(name, value){
    
    if (name == "output_columns"){
        if (!all(value %in% as.character(unlist(listValidColumns())))){
            stop(paste0("Output columns must be within ",
            "the set from listValidColumns()"))
        }
    }
    
    if (name %in% c("file_output", "testing")){
        if (!value %in% c(TRUE, FALSE) ){
            stop("File output needs to be logical")
        }
    }
    
    assign(name, value = value, envir = parent.env(environment()))
}}, SpiderSeqREnv)



#' Inspect the value of package options
#' 
#' @param name A character denoting name of the variable
#' @return Value of the variable
#' 
#' @examples 
#' getSpiderSeqROption("file_output")
#' 
#' @export
#' 
getSpiderSeqROption <- local({function(name){
    get(name)
}}, SpiderSeqREnv)

#-----

