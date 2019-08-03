# Default settings for SpideR options


#' Environment with default values of the package options
#' 
#' @description 
#' Environment for storing values determining certain aspects of package functioning.
#' To view the current value, please use \code{getSpideROption()}, to alter the current value, please use \code{setSpideROption()}.
#' 
#' 
spideREnv <- new.env()


local({
  assign("file_output", TRUE)
  assign("output_columns", NULL) # Amend
  assign("internal", FALSE)
  }, spideREnv)




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
#'     \item file_output - Logical - If TRUE, the search functions will produce file outputs. If FALSE, no files will be created. Defaults to TRUE
#'    \item output_columns - Character vector - A set of columns to be used for generating file outputs. Defaults to one of the \code{listColumnSets()}.
#' }
#' 
#' @examples 
#' setSpideROption("file_output", FALSE)
#' setSpideROption("output_columns", c("run_accession", "gsm"))
#' setSpideROption("output_columns", listColumnSets()$Accession)
#' 
#' @export
setSpideROption <- local({function(name, value){
  
  if (name == "output_columns"){
    if (!all(value %in% as.characterunlist(listValidColumns()))){
      stop("Output columns must be within the set from listValidColumns()")
    }
  }
  
  if (name == "file_output"){
    if (!value %in% c(TRUE, FALSE) ){
      stop("File output needs to be logical")
    }
  }
  
  assign(name, value = value, envir = parent.env(environment()))
}}, spideREnv)



#' Inspect the value of package options
#' 
#' @param name A character denoting name of the variable
#' @return Value of the variable
#' 
#' @examples 
#' getSpideROption("file_output")
#' 
#' @export
#' 
getSpideROption <- local({function(name){
  get(name)
}}, spideREnv)

#-----

