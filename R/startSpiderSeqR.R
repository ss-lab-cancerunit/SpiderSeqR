
# - startSpiderSeqR
# - findFiles



#' Prepare the environment to run SpiderSeqR
#' 
#' \code{startSpiderSeqR} prepares the environment so that 
#' other SpiderSeqR functions can be used. 
#' Run \code{startSpiderSeqR} every time you begin with a clear environment 
#' and want to use any of the other SpiderSeqR functions.
#' In particular, the function does the following:
#' \itemize{
#'     \item Ensure that SRAmetadb.sqlite is downloaded and up to date
#'     \item Ensure that GEOmetadb.sqlite is downloaded and up to date
#'     \item Ensure that SRR_GSM.sqlite is created and up to date
#'     \item Set up database connections to the above files 
#'         in the \code{.GlobalEnv}
#' } 
#' 
#' 
#' Depending on the contents of the specified directory, 
#' \code{startSpiderSeqR} may download/create the database files. 
#' It will always create the database connections in the global environment.
#' 
#' It is necessary to fulfil all the above requirements; 
#' without them the package will not work.
#' The first two database files are relatively large in size 
#' (32 GB and 9 GB at the time of writing), so please ensure that you have 
#' adequate internet connection and sufficient disk space.
#' 
#' It is recommended that the newest version of the databases is used. 
#' However, it is possible to ignore this requirement by manually setting 
#' the expiry date of the files (or by running default settings 
#' of the function and selecting the option not to download the newer files).
#' 
#' 
#' 
#' @section When to run \code{startSpiderSeqR}?:
#' 
#' Run \code{startSpiderSeqR} every time you start with a fresh environment. 
#' There is no harm in running it too many times.
#' 
#' 
#' 
#' @section Which options to choose from \code{startSpiderSeqR} menu options?:
#' 
#' Should you have any missing or outdated files in the specified directory, 
#' \code{startSpiderSeqR} will offer to download/create the files. 
#' 
#' Please note that it is \emph{required} to have all the three database files;
#' if you choose not to download/create them, it will not be possible 
#' to run other SpiderSeqR functions. 
#' 
#' However, it is \emph{not required} for all the files to be up-to-date; 
#' \code{startSpiderSeqR} will suggest re-downloading/re-creating the files, 
#' but you can still use SpiderSeqR, even if you do not agree 
#' to re-download/re-create the files.
#' 
#' 
#' 
#' @section Time and space requirements:
#' 
#' The following files must be present in order to run other SpiderSeqR 
#' functions; \code{startSpiderSeqR} will download/create them if necessary:
#' \itemize{
#'   \item SRAmetadb.sqlite (from SRAdb package)
#'   \item GEOmetadb.sqlite (from GEOmetadb package)
#'   \item SRR_GSM.sqlite (custom-made at the time of running 
#'       \code{startSpiderSeqR})
#' }
#' 
#' 
#' They take approximately 40 GB of disk space 
#' (32 GB, 9 GB and 100 MB respectively at the time of writing), 
#' so please ensure that you have adequate internet connection 
#' and sufficient disk space. 
#' The two downloaded files are downloaded compressed, 
#' so the download size is smaller than the final file size. 
#' In order to save disk space, the previous files will be overwritten 
#' when downloading a newer version, so if you would like to keep them, 
#' please rename them before updating.
#' 
#' 
#' Running \code{startSpiderSeqR} for the first time will inevitably 
#' take some time, because large database files need to be downloaded 
#' and custom database created. However, once all the files are present, 
#' the function should take an order of \emph{seconds} to complete 
#' (it is a matter of setting up database connections).
#' 
#' 
#' 
#' @examples 
#' 
#' ## Database files are stored (or will be downloaded) 
#' ##    in the working directory
#' # startSpiderSeqR(path = getwd()) 
#' 
#' ## Use the following if you would like to download 
#' ##   the newest database files
#' # startSpiderSeqR(path = getwd(), general_expiry = 0) 
#' 
#' ## Use the following if you have old database files
#' ##   that you do not wish to re-download on this occasion
#' # startSpiderSeqR(path = getwd(), general_expiry = 365) 
#' 
#' ## Use the following if you only wish to ignore 
#' ##    an old SRAmetadb.sqlite file, 
#' ##    but get reminders to re-download the other files
#' # startSpiderSeqR(path = getwd(), sra_expiry = 365) 
#' 
#' ## Use the following if you would like to locate
#' ##   the database files in a few directory levels above
#' # startSpiderSeqR(path = getwd(), recurse_levels = 4)
#' 
#' 
#' @param path Directory where database files will be stored
#' @param general_expiry Maximum number of days since creation 
#'     of all database files
#' @param sra_expiry Maximum number of days since creation 
#'     of SRAmetadb.sqlite file
#' @param geo_expiry Maximum number of days since creation 
#'     of GEOmetadb.sqlite file
#' @param srr_gsm_expiry Maximum number of days since creation 
#'     of SRR_GSM.sqlite file
#' 
#' @return Nothing. If necessary, it may download/create database files. 
#' Sets up database connections in the global environment.
#' 
#' @family Setup functions
#' 
#' 
#'
#'
#' @export
#' 
startSpiderSeqR <- function(path, 
                            general_expiry=90, 
                            sra_expiry=NULL, 
                            geo_expiry=NULL, 
                            srr_gsm_expiry=NULL){
    
    
    .mm(cli::rule(), "comm")
    .mm("Welcome to SpiderSeqR!!!", "qn")
    .mm(paste0("Please wait while the database files and connections ",
               "are being configured..."), "comm")
    .mm(cli::rule(), "comm")
    
    path <- normalizePath(path)
    
    file_list <- .findDBFiles(path=path)
    
    sra_file <- file_list$sra_file
    geo_file <- file_list$geo_file
    srr_gsm_file <- file_list$srr_gsm_file
    
    missing_logical <- do.call(.missingFileCheck, c(file_list))
    
    #==========================================================
    
    #Setup:
    # - SRAmetadb
    # - GEOmetadb
    # - SRR_GSM (custom database for converting between the ids 
    #     of SRA and GSMs (GEO))
    #print("Setting up SpiderSeqR")
    
    # Logic:
    # - SRAmetadb:
    #    * if does not exist - download (else: stop)
    #    * if exists and not up to date - offer to re-download (else: message)
    # - GEOmetadb:
    #    * if does not exist - download (else: stop)
    #    * if exists and not up to date - offer to re-download (else: message)
    # - SRR_GSM:
    #    * if file exists
    #        - if in date - do nth
    #        - if not up to date - offer to re-create (else: message)
    #    * if does not exist - create (else: stop)
    
    
    raw_expiry_parameters <- list(general_expiry=general_expiry,
                                    sra_expiry=sra_expiry,
                                    geo_expiry=geo_expiry,
                                    srr_gsm_expiry=srr_gsm_expiry,
                                    missing_file_number=sum(missing_logical))
    
    
    expiry_parameters <- do.call(.setExpiryParameters,c(raw_expiry_parameters))
    
    .mm(cli::rule(), "comm")
    
    #return(file_list)
    
    .GlobalEnv$file_list <- file_list
    .GlobalEnv$expiry_parameters <- expiry_parameters
    
    # Check (or download) SRA and GEO files
    for (i in 1:2){
        
        .checkDBFile(path=path, 
                        db_file=file_list[[i]], 
                        db_file_name=.DBNames()[i], 
                        db_expiry=expiry_parameters[[i]])
        
        .mm(cli::rule(), "comm")
        
    }
    
    
    #==========================================================
    # RE-CHECK THE PRESENCE OF FILES (have they downloaded successfully???)
    #==========================================================
    if (!(file.exists(sra_file) & file.exists(geo_file))){
        warning(paste0("Something went wrong. Some database files might ",
                       "be missing. Please run this function again."))
    } else {
        .mm(paste0(
            "Both db files are present (remember not to (re)move them!)\n",
            "Proceeding to the final step ",
            "(checking for the custom database)."), 
            "comm")
    }
    #==========================================================
    
    
    .mm(cli::rule(), "comm")
    
    .checkDBFile(path=path, 
                 db_file=file_list[[3]], 
                 db_file_name=.DBNames()[3], 
                 db_expiry=expiry_parameters[[3]])
    
    
    if (!isTRUE(getSpiderSeqROption("testing"))){
        .setDBConnections(sra_file=sra_file, 
                                    geo_file=geo_file, 
                                    srr_gsm_file=srr_gsm_file)

    } else {
        #setwd(ori_wd)
        # Early exit if testing (no real database connections established)
        return(TRUE)
    }
    
    #setwd(ori_wd)
    
    con_names <- c("sra_con", "geo_con", "srr_gsm")
    
    # Display metaInfo tables from the databases
    for (i in seq_along(con_names)){
        .mm(cli::rule(), "comm")
        .getFurtherDBInfo(db_file_name = .DBNames()[i], 
                            database_name = con_names[i])
    }
    
    
    .mm(cli::rule(), "comm")
    .mm("SpiderSeqR setup complete", "qn")
    
    
}


#' Find files (a wrapper around list.files)
#' 
#' @param path A path to be searched
#' @param pattern Regular expression pattern to search for (passed to dir())
#' @return A full path with the matching file(s)
#' 
#' @examples 
#' #.findFiles(getwd(), "*.sqlite")
#' 
#' @keywords internal
#' 
.findFiles <- function(path, pattern){
    
    .mm(paste0("Searching for ", pattern, " files in: ", path), "dev")
    
    matches <- list.files(path = path, pattern = pattern, 
                   recursive = TRUE, full.names = TRUE)
    
    return(matches)
}




#' Find files (wrapper around dir allowing to search a few levels up)
#' 
#' @param pattern Regular expression pattern to search for (passed to dir())
#' @param recurse_levels Integer with the number of directory levels 
#'     (above current working directory) to search in
#' @return A full path with the matching file(s)
#' 
#' @examples 
#' #.findFiles("*.sqlite", 5)
#' 
#' @keywords internal
#' 
.findFiles_Ori <- function(pattern, recurse_levels){
    ori_wd <- getwd()
    
    # recurse_levels patterns:
    # rl 0 "."
    # rl 1 ".."
    # rl 2 "../.."
    # rl n "..[n-1](/..)
    
    if (recurse_levels == 0){
        path <- "."
    } else {
        path <- ".."
        if ((recurse_levels-1)>0){
            path <- paste0(path, 
                           paste0(rep("/..", recurse_levels-1), collapse = ""))
        }
    }
    
    setwd(path)
    #print(getwd())
    .mm(paste0("Searching for ", pattern, " files in: ", getwd()), "dev")
    
    matches <- dir(path = getwd(), pattern = pattern, 
                   recursive = TRUE, full.names = TRUE)
    
    
    on.exit(setwd(ori_wd))
    #print(matches)
    
    return(matches)
}




#' Set database connections
#' @param sra_file SRA file name (including path)
#' @param geo_file GEO file name (including path)
#' @param srr_gsm_file SRR_GSM file name (including path)
#' 
#' @return Set up database connections in the global environment
.setDBConnections <- function(sra_file, geo_file, srr_gsm_file){
    
    .GlobalEnv$sra_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = sra_file)
    .GlobalEnv$geo_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = geo_file)
    .GlobalEnv$srr_gsm <- 
        DBI::dbConnect(RSQLite::SQLite(), dbname = srr_gsm_file)
}





#' Generate a progress bar scaled by width
#' 
#' @param i Current stage
#' @param n Total number of stages
#' @param width Number of characters for progress display 
#'     (the whole bar will have 2 extra ones used for marking both ends)
#' @return Nothing. Output progress to console
#' 
#' @examples 
#' 
#' #for (i in 1:10){
#' #    .progressBar(i, 10, 100)
#' #    Sys.sleep(1)
#' #}
#' 
#' @keywords internal
#' 
.progressBar <- function(i, n, width=78){
    
    if (i > n) i <- n
    
    x <- (i*width) %/% n
    
    
    #cat("PROGRESS:\n")
    string <- paste0("[", paste0(rep("|", x), collapse = ""), 
                     paste0(rep(" ", width-x), collapse = ""), "]")
    cat(string, " \r")
    utils::flush.console()
}




