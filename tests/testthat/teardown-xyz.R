
#print(ls(envir = .GlobalEnv))
#rm("x", envir = .GlobalEnv)
print(ls(envir = .GlobalEnv))



print(q)
setSpiderSeqROption("quiet", q)

setSpiderSeqROption("testing", testing)



# print(environment())

DBI::dbDisconnect(get("sra_con", envir = get(".GlobalEnv")))
DBI::dbDisconnect(get("geo_con", envir = get(".GlobalEnv")))
DBI::dbDisconnect(get("srr_gsm", envir = get(".GlobalEnv")))

rm("sra_con", envir = .GlobalEnv)
rm("geo_con", envir = .GlobalEnv)
rm("srr_gsm", envir = .GlobalEnv)


if (preserve_connection){
  if (exists("temp_sra_con", envir = .GlobalEnv)){
    assign("sra_con", get("temp_sra_con", envir = .GlobalEnv), envir = .GlobalEnv)
    rm("temp_sra_con", envir = .GlobalEnv)
  }
  
  if (exists("temp_geo_con", envir = .GlobalEnv)){
    assign("geo_con", get("temp_geo_con", envir = .GlobalEnv), envir = .GlobalEnv)
    rm("temp_geo_con", envir = .GlobalEnv)
  }
  
  if (exists("temp_srr_gsm", envir = .GlobalEnv)){
    assign("srr_gsm", get("temp_srr_gsm", envir = .GlobalEnv), envir = .GlobalEnv)
    rm("temp_srr_gsm", envir = .GlobalEnv)
  }
  
  
}


if (getSpiderSeqROption("local_run")){
  # For the sanity check of the deletion method:
  #write.csv(c(1:10), "testdata/Mock_Database_Files/sham.csv")
  
  #' Clean up the added db files
  all_files <- list.files("testdata/Mock_Database_Files", 
                          recursive = TRUE, full.names = TRUE)
  new_files <- all_files[!(all_files %in% getSpiderSeqROption("db_files"))]
  
  print("NEW FILES:")
  print(new_files)
  
  if (length(new_files) > 0){
    file.remove(new_files)
  }
  
}



