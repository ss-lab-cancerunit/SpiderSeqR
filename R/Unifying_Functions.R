

#----------------------------------------------------------------------------
# unifyyDFFormat
#----------------------------------------------------------------------------

#' Unify formats of data frames
#' 
#' @param df Data frame to be processed
#' @return Data frame in a unified format
#' 
#' Unifies the format of a data frame. The following transformations are made:
#' \itemize{
#'     \item Order columns
#'     \item Remove duplicate rows (\code{unique(df)})
#'     \item Order rows (and resetting row names)
#'     \item Convert data types (mostly to character, with some exceptions converted to integer)
#' }
#' 
#' @keywords internal
unifyDFFormat <- function(df){
    
    #col_order <- c("sra", "gsm", "gse", "other", "added")
    
    
    # Order columns ####
    valid_columns <- listValidColumns()
    
    
    sra_ind <- which(colnames(df) %in% valid_columns$SRA[!valid_columns$SRA=="SRA_sra_ID"]) # Exclude SRA_sra_ID (not present in sra_ft)
    gsm_ind <- which(colnames(df) %in% valid_columns$GSM)
    gse_ind <- which(colnames(df) %in% valid_columns$GSE)
    other_ind <- which(colnames(df) %in% valid_columns$Other)
    
    
    df <- df[, c(sra_ind, gsm_ind, gse_ind, other_ind)]
    
    # Remove duplicates
    df <- unique(df)
    
    # Order rows according to accession and reset row names ####
    df <- orderDFAccessions(df)
    
    # Convert column data type ####
    # Not used atm
    numeric_cols <- c("SRA_spots", "SRA_bases", 
                      "SRA_study_ID", "SRA_sample_ID", "SRA_experiment_ID", "SRA_submission_ID",
                      "SRA_number_of_levels", "SRA_taxon_id", 
                      "GSM_data_row_count", "GSM_channel_count", "GSM_ID",
                      #"gsm_check", 
                      "OTH_n", "OTH_lane")
    
    for (i in seq(1, dim(df)[2])){
        #if (!colnames(df)[i] %in% numeric_cols){
        #  df[ , i] <- as.character(df[, i])
        #} else {
        #  df[ , i] <- as.numeric(df[ ,i])
        #}
        
        df[, i] <- as.character(df[,i])
        
    }
    row.names(df) <- NULL
    
    return(df)
}
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------



#----------------------------------------------------------------------------
# orderDFAccessions
#----------------------------------------------------------------------------

#' Order df rows in accession order
#' 
#' @param df Data frame to be ordered (must contain columns corresponding to types specified in \code{acc_order}, i.e. study_accession, sample_accession, experiment_accession, run_accession, gsm).
#' @param acc_order A character vector with accession levels to be used for ordering (their order specifies which accession level is taken into account first). Default value lists all possible elements.
#' @param na.last Logical denoting whether NA values should be placed last. Defaults to TRUE.
#' @return Data frame with rows ordered accordingly. The row names are reset.
#' 
#' 
orderDFAccessions <- function(df, acc_order = c("study", "sample", "experiment", "run", "gsm"), na.last = TRUE){
    
    acc_cols <- list()
    acc_cols[["study"]] <- "study_accession"
    acc_cols[["sample"]] <- "sample_accession"
    acc_cols[["experiment"]] <- "experiment_accession"
    acc_cols[["run"]] <- "run_accession"
    acc_cols[["gsm"]] <- "gsm"
    
    # Check df contains necessary columns
    verifyColumns(df, acc_cols)
    
    col_ind <- integer() # For storing column indices
    
    # Access like: acc_cols[["study"]]
    for (i in seq_along(acc_order)){
        col_ind[i] <- which(colnames(df) %in% acc_cols[[acc_order[i]]])
        #print(col_ind[i])
        #print(acc_cols[[acc_order[i]]])
        if (length(col_ind[i])>1){
            stop("Multiple matching columns")
        }
    }
    
    order_columns <- list()
    for (a in seq_along(col_ind)){
        order_columns <- c(order_columns, list(df[, col_ind[a]]))
    }
    #print(order_columns)
    
    df <- df[orderAccessions(order_columns, na.last = TRUE), ]
    
    rownames(df) <- NULL
    
    return(df)
}

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------









#----------------------------------------------------------------------------
# checkValidColumns
#----------------------------------------------------------------------------
#' 
#' Check if column names of df are within allowed set
#' 
#' @param df Data frame to be checked
#' @return Nothing - give an error or a warning
#' 
#' Current checks
#' \itemize{
#'     \item Argument is a data frame
#'     \item All columns within Valid Columns list
#' }
#' 
#' @keywords internal
checkValidColumns <- function(df){
    
    if (!class(df) %in% "data.frame"){
        stop("Argument is not a data frame")
    }
    
    # Get a vector with allowed columns
    allowed_columns <- as.character(unlist(listValidColumns()))
    
    if (sum(colnames(df) %in% allowed_columns)==length(colnames(df))){
        message("All columns have valid names")
    } else {
        wrong_columns <- colnames(df)[!colnames(df) %in% allowed_columns]
        stop(paste0("Some columns are not allowed: ", paste0(wrong_columns, collapse = ", ")))
    }
}

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------



#----------------------------------------------------------------------------
# listValidColumns
#----------------------------------------------------------------------------
#' List valid columns within the data frames
#' 
#' @return A list with column names grouped into categories
#' 
#' @examples 
#' listValidColumns() # List all columns
#' listValidColumns()$SRA # List columns from sra table
#' listValidColumns()$GSM # List columns from gsm table
#' listValidColumns()$GSE # List columns from gse table
#' listValidColumns()$Other # List other columns
#' 
#' @section Column Categories:
#' The following categories are available:
#' \itemize{
#'     \item SRA - columns from the sra table (SRA database) with added 'SRA_' prefix
#'     \item GSM - columns from the gsm table (GEO database) with added 'GSM_' prefix
#'     \item GSE - columns from the gse table (GSE database) with added 'GSE_' prefix
#'     \item Other - other columns (created within the pacakge) with added 'OTH_' prefix
#' }
#' 
#' 
#' @export
#' 
listValidColumns <- function(){
    
    # Database connections ####
    sra_database_name <- "sra_con"
    geo_database_name <- "geo_con"
    database_env <- ".GlobalEnv"
    
    # Columns that are not prefixed ####
    sra_acc <- c("run_accession", "experiment_accession", "sample_accession", "study_accession", "submission_accession")
    geo_acc <- c("gsm", "series_id")
    
    # List of other columns ####
    oth_columns <- c("input", "control", 
                     "sa_tissue", "sa_antibody", "sa_gene", "sa_treatment", "sa_remainder", 
                     "ch1_tissue", "ch1_antibody", "ch1_gene", "ch1_treatment", "ch1_remainder",
                     "lane", "mer", "pairedEnd", "n")
    oth_columns <- paste0("OTH_", oth_columns)
    
    
    
    # Create lists of db columns ####
    sra_columns <- DBI::dbListFields(get(sra_database_name, envir = get(database_env)), "sra")
    gsm_columns <- DBI::dbListFields(get(geo_database_name, envir = get(database_env)), "gsm")
    gse_columns <- DBI::dbListFields(get(geo_database_name, envir = get(database_env)), "gse")
    
    sra_columns <- sra_columns[!sra_columns %in% "sra_ID"] # Remove sra_ID
    #sra_columns <- sra_columns[!sra_columns %in% "run_ID"] # Remove run_ID column ===*===
    gse_columns <- gse_columns[!gse_columns %in% "gse"] # Remove gse column
    
    sra_columns[!sra_columns %in% sra_acc] <- paste0("SRA_", sra_columns[!sra_columns %in% sra_acc])
    #sra_columns <- c(sra_acc, sra_columns) # NOTE: order not preserved
    
    gsm_columns[!gsm_columns %in% geo_acc] <- paste0("GSM_", gsm_columns[!gsm_columns %in% geo_acc])
    #gsm_columns <- c(geo_acc, gsm_columns) # NOTE: order not preserved
    
    gse_columns[!gse_columns %in% geo_acc] <- paste0("GSE_", gse_columns[!gse_columns %in% geo_acc])
    #gse_columns <- c(geo_acc, gse_columns) # NOTE: order not preserved
    
    db_columns <- list(SRA = sra_columns, GSM = gsm_columns, GSE = gse_columns, Other = oth_columns)
    return(db_columns)
    
}
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------

