descriptive_columns <- c("title",
                         "source_name_ch1",
                         "characteristics_ch1",
                         "treatment_protocol_ch1",
                         "extract_protocol_ch1",
                         "characteristics_ch2",
                         "source_name_ch2",
                         "treatment_protocol_ch2",
                         "description",
                         "data_processing")



tic()
df1a <- dbGetQuery(sra_con, "SELECT * FROM sra_ft WHERE sra_ft MATCH 'stat3'")
toc()
tic()
df1b <- dbGetQuery(sra_con, "SELECT run_accession FROM sra_ft WHERE sra_ft MATCH 'stat3'")
toc()



tic()
df2a <- dbGetQuery(geo_ft, "SELECT * FROM gsm_ft WHERE gsm_ft MATCH 'stat3' ")
toc()
tic()
df2b <- dbGetQuery(geo_ft, "SELECT gsm FROM gsm_ft WHERE gsm_ft MATCH 'stat3' ")
toc()


searchForAccessionAcrossDBs()

searchForAccession()

searchSRATablesForAccession()

