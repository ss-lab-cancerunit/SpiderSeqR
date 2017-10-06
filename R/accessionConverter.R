
#accessionConverter.R
#Converts a list of accessions (of one type) into all possible accessions within SRA and GEO

#------------------------------------------------------
#------------------------------------------------------
accessionConverter <- function(acc_list){

  geo_columns <- c("gsm", "series_id")
  #srr_gsm_columns <- c("gsm", "gsm_check", "run_accession")
  sra_columns <- c("run_accession", "experiment_accession", "sample_accession", "study_accession")


  accession_class <- accessionClassifier(acc_list)

  if (!(accession_class %in% c("gsm", "series_id", "run_accesion", "experiment_accession", "sample_accession", "study_accession"))){
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

    #SRR_GSM data frame
    srr_gsm_df <- searchSRR_GSM(geo_df$gsm)

    #SRA data frame
    sra_df <- searchSRAForAccession(srr_gsm_df$run_accession, sra_columns)


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

    #SRR_GSM data frame
    srr_gsm_df <- searchSRR_GSM(sra_df$run_accession)

    #GEO data frame
    geo_df <- searchGEOForGSM(srr_gsm_df$gsm, geo_columns)

    #Merge
    #DOUBLE CHECK IF WANT ALL OR ALL.X ===*===
    sra_srr_gsm_df <- merge(sra_df, srr_gsm_df, by.x = "run_accession", by.y = "run_accession", all = TRUE)
    sra_srr_gsm_geo_df <- merge(sra_srr_gsm_df, geo_df, by.x = "gsm", by.y = "gsm", all = TRUE)
    output_df <- sra_srr_gsm_geo_df

  }

  #=============================================================

  output_df <- output_df[ , c("run_accession", "experiment_accession", "sample_accession", "study_accession", "gsm", "series_id", "gsm_check")]

  return(output_df)

}
#------------------------------------------------------
#------------------------------------------------------
