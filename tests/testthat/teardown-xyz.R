
#print(ls(envir = .GlobalEnv))
rm("x", envir = .GlobalEnv)
print(ls(envir = .GlobalEnv))



print(q)
setSpiderSeqROption("quiet", q)
DBI::dbDisconnect(get("sra_con", envir = get(".GlobalEnv")))
DBI::dbDisconnect(get("geo_con", envir = get(".GlobalEnv")))
DBI::dbDisconnect(get("srr_gsm", envir = get(".GlobalEnv")))

