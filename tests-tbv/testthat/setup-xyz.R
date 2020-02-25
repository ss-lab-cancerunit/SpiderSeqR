
print("setup")
q <- getSpiderSeqROption("quiet")

setup({
  print("indiv setup")
  print(q)
  setSpiderSeqROption("quiet", TRUE)
  startSpiderSeqRDemo()
})

teardown({
  print(q)
  print("indiv teardown")
  setSpiderSeqROption("quiet", q)
  DBI::dbDisconnect(get("sra_con", envir = get(".GlobalEnv")))
  DBI::dbDisconnect(get("geo_con", envir = get(".GlobalEnv")))
  DBI::dbDisconnect(get("srr_gsm", envir = get(".GlobalEnv")))
})