## code to prepare `Demo_DBs` dataset




# Setup ####

devtools::load_all()
#startSpiderSeqR(getwd())
startSpiderSeqR("C:\\DD\\Projects\\SpideRs\\SpiderSeqR-Auxillaries\\Database_Files")
library(DBI)




# Accessions of interest ####

# Find total number of records
#gse_tot <- as.numeric(dbGetQuery(geo_con, "SELECT count(DISTINCT gse) FROM gse")) # 114406
#srp_tot <- as.numeric(dbGetQuery(sra_con, "SELECT count(DISTINCT study_accession) FROM sra")) # 160612


#gses <- DBI::dbGetQuery(geo_con, "SELECT gse FROM gse GROUP BY RANDOM() LIMIT 10")$gse
#srps <- DBI::dbGetQuery(sra_con, "SELECT study_accession FROM sra GROUP BY RANDOM() LIMIT 10")$study_accession

#Original Accessions generated (some problematic due to db issues)
#gses <- c('GSE48253', 'GSE69001', 'GSE114512', 'GSE27360', 'GSE82246', 'GSE80852', 'GSE36467', 'GSE76553', 'GSE119855', 'GSE10309')
#srps <- c('SRP042189', 'SRP119890', 'SRP134708', 'ERP004444', 'ERP010785', 'SRP029441', 'SRP018785', 'SRP100699', 'SRP065899', 'SRP120023')


gses <- c('GSE48253', 'GSE69001', 'GSE27360', 'GSE82246', 'GSE36467', 'GSE76553', 'GSE10309', 'GSE80767')
srps <- c('SRP134708', 'DRP003157', 'SRP061795', 'SRP029758', 'SRP076433', 'SRP148363', 'SRP062911')






# Functions ####

searchGSEs <- function(gses){
  gse_df <- data.frame()
  for (g in seq_along(gses)){
    gse_chunk <- dbGetQuery(geo_con, paste0("SELECT * FROM gse WHERE gse = '", gses[g], "'"))
    gse_df <- rbind(gse_df, gse_chunk)
  }
  return(gse_df)
}


searchGSEsInGSM <- function(gses){
  gsm_df <- data.frame()
  for (g in seq_along(gses)){
    query <- paste0("SELECT * FROM gsm WHERE series_id LIKE '%", gses[g], "' OR series_id LIKE '%", gses[g], ",%'")
    print(query)
    gsm_chunk <- dbGetQuery(geo_con, query)
    gsm_df <- rbind(gsm_df, gsm_chunk)
  }
  return(gsm_df)
}

searchGSMsInGSM <- function(gsms){
  gsm_df <- data.frame()
  for (g in seq_along(gsms)){
    gsm_chunk <- dbGetQuery(geo_con, paste0("SELECT * FROM gsm WHERE gsm = '", gsms[g], "'"))
    gsm_df <- rbind(gsm_df, gsm_chunk)
  }
  return(gsm_df)
}



searchGSMsInSRRGSM <- function(gsms){
  srr_df <- data.frame()
  for (g in seq_along(gsms)){
    srr_chunk <- dbGetQuery(srr_gsm, paste0("SELECT * FROM srr_gsm WHERE gsm = '", gsms[g], "'"))
    srr_df <- rbind(srr_df, srr_chunk)
  }
  return(srr_df)
}

searchSRPsInSRRGSM <- function(srps){
  srr_df <- data.frame()
  for (g in seq_along(srps)){
    srr_chunk <- dbGetQuery(srr_gsm, paste0("SELECT * FROM srr_gsm WHERE study_accession = '", srps[g], "'"))
    srr_df <- rbind(srr_df, srr_chunk)
  }
  return(srr_df)
}


searchSRRsInSRA <- function(srrs){
  sra_df <- data.frame()
  for (s in seq_along(srrs)){
    sra_chunk <- dbGetQuery(sra_con, paste0("SELECT * FROM sra WHERE run_accession = '", srrs[s], "'"))
    sra_df <- rbind(sra_df, sra_chunk)
  }
  return(sra_df)
}

searchSRPsInSRA <- function(srps){
  sra_df <- data.frame()
  for (s in seq_along(srps)){
    sra_chunk <- dbGetQuery(sra_con, paste0("SELECT * FROM sra WHERE study_accession = '", srps[s], "'"))
    sra_df <- rbind(sra_df, sra_chunk)
  }
  return(sra_df)
}





# Search for GSEs ####

# Search in GEO

# Search for original matches
gse_df <- searchGSEs(gses) # gse
gsm_df <- searchGSEsInGSM(gse_df$gse) #gsm


# Extract extra gses from the df
extra_geo_gses <- unique(unlist(strsplit(gsm_df$series_id, split = ",")))
extra_geo_gses <- extra_geo_gses[!(extra_geo_gses %in% gses)] # Only use gses not found before


# Search for extra gses and their corresponding gses
gsm_df <- rbind(gsm_df, searchGSEsInGSM(extra_geo_gses))
gsm_df <- unique(gsm_df)

extra_geo_gses <- unique(unlist(strsplit(gsm_df$series_id, split = ",")))
extra_geo_gses <- extra_geo_gses[!(extra_geo_gses %in% gses)]


gse_df <- rbind(gse_df, searchGSEs(extra_geo_gses))
gse_df <- unique(gse_df)



# Search in conversion db
srr_df <- searchGSMsInSRRGSM(gsm_df$gsm)

print(unique(srr_df$study_accession))
# Search in SRA
sra_df <- searchSRPsInSRA(srr_df$study_accession)





# Search for SRPs ####


sra_df <- rbind(sra_df, searchSRPsInSRA(srps))


srr_df_sra <- rbind(srr_df, searchSRPsInSRRGSM(srps)) # search for srps to avoid repetition
srr_df <- rbind(srr_df, srr_df_sra)


# Search GSMs
gsm_df_sra <- searchGSMsInGSM(srr_df_sra$gsm)

# Find all GSEs
gses_sra <- unique(unlist(strsplit(gsm_df_sra$series_id, split = ",")))

gsm_df_sra <- rbind(gsm_df_sra, searchGSEsInGSM(gses_sra))
gsm_df_sra <- unique(gsm_df_sra)
gsm_df <- rbind(gsm_df, gsm_df_sra)

gses_sra_2 <- unique(unlist(strsplit(gsm_df_sra$series_id, split = ",")))

gse_df <- rbind(gse_df, searchGSEs(gses_sra_2))





# Tidy up ####

gse_df <- unique(gse_df)
gsm_df <- unique(gsm_df)
srr_df <- unique(srr_df)
sra_df <- unique(sra_df)


gse_demo <- gse_df
gsm_demo <- gsm_df
srr_demo <- srr_df
sra_demo <- sra_df


#-----------------------------------------------------------------------------

# Metadata dataframes (metaInfo)

sra_metadata <- DBI::dbGetQuery(sra_con, "SELECT * FROM metaInfo")
geo_metadata <- DBI::dbGetQuery(geo_con, "SELECT * FROM metaInfo")
srr_gsm_metadata <- DBI::dbGetQuery(srr_gsm, "SELECT * FROM metaInfo")


#-----------------------------------------------------------------------------
# SRA GEO Category Conversion ####

## code to prepare `SRA_GEO_Category_Conversion` dataset 

SRA_GEO_Category_Conversion <- read.csv(
    "data-raw/SRA_GEO_Category_Conversion.csv", stringsAsFactors = FALSE)

# OPTION 1 (i)
usethis::use_data(SRA_GEO_Category_Conversion, compress = "xz")

#-----------------------------------------------------------------------------

# Export data #####


# OPTION 1 (ii)
usethis::use_data(gse_demo, compress = "xz")
usethis::use_data(gsm_demo, compress = "xz")
usethis::use_data(srr_demo, compress = "xz")
usethis::use_data(sra_demo, compress = "xz")



# OPTION 2
#save(gse_demo, gsm_demo, srr_demo, sra_demo, SRA_GEO_Category_Conversion, 
#        file = "SpiderSeqR.RData")




