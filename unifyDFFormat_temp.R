unifyDFFormat <- function(df){
  
  #col_order <- c("sra", "gsm", "gse", "other", "added")
  
  
  # Order columns ####
  valid_columns <- listValidColumns()
  

  sra_ind <- which(colnames(df) %in% valid_columns$sra[!valid_columns$sra=="SRA_sra_ID"]) # Exclude SRA_sra_ID (not present in sra_ft)
  gsm_ind <- which(colnames(df) %in% valid_columns$gsm)
  gse_ind <- which(colnames(df) %in% valid_columns$gse)
  other_ind <- which(colnames(df) %in% valid_columns$other)
  added_ind <- which(colnames(df) %in% valid_columns$added)
  
  df <- df[, c(sra_ind, gsm_ind, gse_ind, other_ind, added_ind)]
  
  
  # Order rows according to accession and reset row names ####
  # ===*===
  
  # Convert column data type ####
  numeric_cols <- c("SRA_spots", #CHECKED
                    "SRA_bases", #TO BE CHECKED
                    "SRA_number_of_levels", "SRA_taxon_id", "GSM_data_row_count", "GSM_channel_count", "gsm_check", "OTH_n", "OTH_lane")
  for (i in 1:dim(df)[2]){
    if (!colnames(df)[i] %in% numeric_cols){
      df[ , i] <- as.character(df[, i])
    } else {
      df[ , i] <- as.numeric(df[ ,i])
    }

  }
  
  
  
  
  
}

# Set order
#col_order <- c("sra", "gsm", "gse", "other")

# List gsm columns
# List gse columns

# List sra_columns
# List sra_ft columns
# Remove columns from sra that are not in sra ft (throw error if more than one)

# List other columns


# Find which columns are present in df
# Output text of which are missing

# Find column indices for sra, gsm, gse, other

# Find remainder columns (not fitting anywhere)

# as.data.frame

# Convert all columns to character form


#database_name <- "geo_con"
#database_env <- ".GlobalEnv"

#get(database_name, envir = get(database_env))


#----------------------------------
chExtractor()
checkValidColumns()

