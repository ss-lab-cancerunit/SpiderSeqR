
# to be included on setup
#list.files(path=path, recursive=TRUE)


#' Remove new files
#' 
#' @param path Path to be searched within
#' @param previous_files A character vector with the file list within path
#' @return Nothing. Prints the names of new files and removes them.
#' 
#' @keywords internal
#' 
.clearUpDBFiles <- function(path, previous_files){
    current_files <- list.files(path=path, recursive=TRUE)
    
    new_files <- current_files[!(current_files %in% previous_files)]
    print(new_files)
    file.remove(new_files)
    
}