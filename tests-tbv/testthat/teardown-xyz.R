#DBI::dbDisconnect(geo_con)
#DBI::dbDisconnect(sra_con)
#DBI::dbDisconnect(srr_gsm)

print("disconnected")

DBI::dbDisconnect(get("sra_con", envir = get(".GlobalEnv")))
DBI::dbDisconnect(get("geo_con", envir = get(".GlobalEnv")))
DBI::dbDisconnect(get("srr_gsm", envir = get(".GlobalEnv")))

ls(envir = get(".GlobalEnv"))