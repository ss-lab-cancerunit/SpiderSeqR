

#test_input_list <- list(sra_file_choice=TRUE,
#        geo_file_choice=NULL,
#        srr_file_choice=NULL,
#        sra_download=NULL,
#        geo_download=NULL,
#        srr_create=NULL)

test_input_list <- list(sra_file_choice=NULL,
                        geo_file_choice=NULL,
                        srr_gsm_file_choice=NULL,
                        download_file=1)



#' test
setSpiderSeqROption("test_input", test_input_list)


#' Some function that will use .tmenu
#.someFunction <- function(){
#    menu_out<- .tmenu(choices = c(1,2,3), menu_name = "sra_file_choice")
#    print(menu_out)
#}



#' Menu function with testing interface
#' 
#' @param choices A character vector of choices
#' @param menu_name A character with the menu name 
#'      (for access of pre-set options for testing, under 'test_input' option)
#' @param title Menu title (defaults to NULL)
#' @return Chosen menu option, either by the users, or by accessing 
#' pre-set options
#' 
#' @keywords internal
.tmenu <- function(choices, menu_name, title = NULL){
    
    if (isTRUE(getSpiderSeqROption("testing"))){
        
        out <- getSpiderSeqROption("test_input")
        out <- out[[menu_name]]
        
    } else {
        out <- utils::menu(choices=choices, title=title)
        
    }
    return(out)
}

