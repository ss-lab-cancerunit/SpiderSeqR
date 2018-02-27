#
#
#NEW
#searchForAccessionAcrossDBs - initial accessionConverter with more function arguments (i.e. table columns for sra and geo)
#
#CHANGED
#convertAccession (previously accessionConverter) - a wrapper around searchForAccessionAcrossDBs
#Converts a list of accessions (of one type) into all possible accessions within SRA and GEO




searchForAccessionAcrossDBs <- function(acc_list, sra_columns, geo_columns){

  accession_class <- accessionClassifier(acc_list)

  if (!(accession_class %in% c("gsm", "series_id", "run_accession", "experiment_accession", "sample_accession", "study_accession"))){
    stop("Accession needs to belong to one of the supported classes")
  }

  #=============================================================
  # GEO accession as input
  #=============================================================


  if (accession_class %in% c("gsm", "series_id")){

    #GEO data frame
    if (accession_class == "gsm"){
      geo_df <- searchGEOForGSM(acc_list, geo_columns)
    }
    if (accession_class == "series_id"){
      geo_df <- searchGEOForGSE(acc_list, geo_columns)
    }

    #saveRDS(geo_df, "geo_df.Rda")

    #SRR_GSM data frame
    srr_gsm_df <- searchSRR_GSM(geo_df$gsm)
    #saveRDS(srr_gsm_df, "srr_gsm_df.Rda")

    #SRA data frame
    sra_df <- searchSRAForAccession(srr_gsm_df$run_accession, sra_columns)
    #saveRDS(sra_df, "sra_df.Rda")


    #Merge
    #DOUBLE CHECK IF WANT ALL OR ALL.X ===*===
    geo_srr_gsm_df <- merge(geo_df, srr_gsm_df, by.x = "gsm", by.y = "gsm", all = TRUE)
    geo_srr_gsm_sra_df <- merge(geo_srr_gsm_df, sra_df, by.x = "run_accession", by.y = "run_accession", all = TRUE)

    #Rename data frame
    output_df <- geo_srr_gsm_sra_df

  }
  #=============================================================





  #=============================================================
  # SRA accession as input
  #=============================================================
  if (accession_class %in% c("run_accession", "experiment_accession", "sample_accession", "study_accession")){

    #SRA data frame
    sra_df <- searchSRAForAccession(acc_list, sra_columns)

    #TEMP
    .GlobalEnv$temp_sra_df <- sra_df
    #...

    #SRR_GSM data frame
    srr_gsm_df <- searchSRR_GSM(sra_df$run_accession)

    #TEMP
    .GlobalEnv$temp_srr_gsm_df <- srr_gsm_df
    #...

    #GEO data frame
    geo_df <- searchGEOForGSM(srr_gsm_df$gsm, geo_columns)

    #Create an empty data frame with appropriate column names
    #if(dim(geo_df)[1]==0){
      #geo_gsm_columns <- dbListFields(get("geo_con"), "gsm")
    #}

    #TEMP
    .GlobalEnv$temp_geo_df <- geo_df
    #...

    #Merge
    #DOUBLE CHECK IF WANT ALL OR ALL.X ===*===
    sra_srr_gsm_df <- merge(sra_df, srr_gsm_df, by.x = "run_accession", by.y = "run_accession", all = TRUE)

    #TEMP
    .GlobalEnv$temp_sra_srr_gsm_df <- sra_srr_gsm_df
    #...


    sra_srr_gsm_geo_df <- merge(sra_srr_gsm_df, geo_df, by.x = "gsm", by.y = "gsm", all = TRUE)

    #TEMP
    .GlobalEnv$temp_sra_srr_gsm_geo_df <- sra_srr_gsm_geo_df
    #...

    output_df <- sra_srr_gsm_geo_df

  }

  #=============================================================

  return(output_df)

}




convertAccession <- function(acc_list){

  geo_columns <- c("gsm", "series_id")
  #srr_gsm_columns <- c("gsm", "gsm_check", "run_accession")
  sra_columns <- c("run_accession", "experiment_accession", "sample_accession", "study_accession")

  output_df <- searchForAccessionAcrossDBs(acc_list, geo_columns = geo_columns, sra_columns = sra_columns)

  output_df <- output_df[ , c("run_accession", "experiment_accession", "sample_accession", "study_accession", "gsm", "series_id", "gsm_check")]

  return(output_df)

}




