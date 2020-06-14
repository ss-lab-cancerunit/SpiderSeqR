
#' Create Mock Database Files
#' The aim of these files is to test the functioning of the startSpiderSeqR()
#' function, for testing other functions, mock databases


startSpiderSeqR("..")
sra_db <- DBI::dbGetQuery(sra_con, "SELECT * FROM metaInfo")
geo_db <- DBI::dbGetQuery(geo_con, "SELECT * FROM metaInfo")
srr_gsm_db <- DBI::dbGetQuery(srr_gsm, "SELECT * FROM metaInfo")





sra_mock <- DBI::dbConnect(RSQLite::SQLite(), 
            "tests/testthat/testdata/Mock_Database_Files/SRAmetadb.sqlite")
DBI::dbWriteTable(sra_mock, name='metaInfo', value=sra_db)


geo_mock <- DBI::dbConnect(RSQLite::SQLite(), 
            "tests/testthat/testdata/Mock_Database_Files/GEOmetadb.sqlite")
DBI::dbWriteTable(geo_mock, name='metaInfo', value=geo_db)


srr_gsm_mock <- DBI::dbConnect(RSQLite::SQLite(), 
            "tests/testthat/testdata/Mock_Database_Files/SRR_GSM.sqlite")
DBI::dbWriteTable(srr_gsm_mock, name='metaInfo', value=srr_gsm_db)


