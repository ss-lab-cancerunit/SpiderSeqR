unifyDFFormat <- function(df){
  
  col_order <- c("sra", "gsm", "gse", "other")
  
  database_name_sra <- "sra_con"
  database_name_geo <- "geo_con"
  database_env <- ".GlobalEnv"
  
  
  # Set reference column vectors ####
  sra_columns <- DBI::dbListFields(get(database_name_sra, envir = get(database_env)), "sra")
  sra_ft_columns <- DBI::dbListFields(get(database_name_sra, envir = get(database_env)), "sra_ft")
  gsm_columns <- DBI::dbListFields(get(database_name_geo, envir = get(database_env)), "gsm")
  gse_columns <- DBI::dbListFields(get(database_name_geo, envir = get(database_env)), "gse")
  other_columns <- c('sa_remainder', 'sa_tissue', 'sa_antibody', 'sa_gene', 'sa_treatment', 'ch1_remainder', 'ch1_tissue', 'ch1_antibody', 'ch1_gene', 'ch1_treatment', 'input', 'control', 'n', 'lane', 'mer', 'pairedEnd')
  
  
  if (length(sra_columns)!=(length(sra_ft_columns)+1)) stop("Double check whether available columns haven't changed")
  sra_columns <- sra_columns[sra_columns %in% sra_ft_columns]
  
  

  
  
  
  sra_ind <- which(colnames(df) %in% sra_columns)
  gsm_ind <- which(colnames(df) %in% gsm_columns)
  gse_ind <- which(colnames(df) %in% paste0("GSE_", gse_columns)) # Correction made for GSE_ prefix
  other_ind <- which(colnames(df) %in% other_columns)
  
  colnames(gsm_ind)[gsm_ind] <- paste0("GSM_", colnames(gsm_ind)[gsm_ind]) 
  
  
  
  
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

