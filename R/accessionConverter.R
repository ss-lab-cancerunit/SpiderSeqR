#Convert accessions

#Very Much In Progress

#accessionConverter <- function(accession_vector)

#x <- "SRX1441669"
#x <- "GSE75250"
#accession_vector <- "GSM1947436"
accession_vector <- "ERP010604"

x <- unique(accession_vector)
x <- x[digitSort(x)]


database_name <- "sra_con"

accession_class <- accessionClassifier(x)
search_count <- 0
accession_df <- data.frame()


if (accession_class %in% c("run_accession", "experiment_accession", "sample_accession", "study_accession")){

  for (a in seq_along(x)){
    query <- paste0("SELECT run_accession, experiment_accession, sample_accession, study_accession, run_alias, experiment_attribute, study_name, study_alias FROM sra WHERE ", accession_class, " = '", x[a], "'")
    print(query)
    chunk <- dbGetQuery(get(database_name), query)
    search_count <- search_count + as.integer(dim(chunk)[1]>=1)
    accession_df <- rbind(accession_df, chunk)
  }

} else if (accession_class == "gsm"){

  for (a in seq_along(x)){
    query <- paste0("SELECT run_accession, experiment_accession, sample_accession, study_accession, run_alias, experiment_attribute, study_name, study_alias FROM sra WHERE run_alias", " LIKE '%", x[a], "%' OR experiment_attribute LIKE '%", x[a], "%'")
    print(query)
    chunk <- dbGetQuery(get(database_name), query)
    search_count <- search_count + as.integer(dim(chunk)[1]>=1)
    accession_df <- rbind(accession_df, chunk)
  }

} else if (accession_class == "series_id"){

  for (a in seq_along(x)){
    query <- paste0("SELECT run_accession, experiment_accession, sample_accession, study_accession, run_alias, experiment_attribute, study_name, study_alias FROM sra WHERE study_name", " LIKE '%", x[a], "%' OR study_alias LIKE '%", x[a], "%'")
    print(query)
    chunk <- dbGetQuery(get(database_name), query)
    search_count <- search_count + as.integer(dim(chunk)[1]>=1)
    accession_df <- rbind(accession_df, chunk)
  }

}

#EXTRACTION

#GSMs
#Find entries which have GSM\\d\\d\\d+ structure (gsm_indices)
#Create GSM column (NA)
#Perform gsub on gsm_indices fields

#GSEs
#Same as GSMs



#GEO SEARCH
#   - if SRA as input: search for GSMs that exist in the accession_df (ignore NAs)
#   - if GEO as input: search for input (GSM or GSE)

#Merge by GSM, unless there were no GSMs - then by GSE???



