
q <- getSpiderSeqROption("quiet")

setup({
  print(q)
  setSpiderSeqROption("quiet", TRUE)
})

teardown({
  print(q)
  setSpiderSeqROption("quiet", q)
})