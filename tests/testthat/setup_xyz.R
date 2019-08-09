
q <- getSpideROption("quiet")

setup({
  print(q)
  setSpideROption("quiet", TRUE)
})

teardown({
  print(q)
  setSpideROption("quiet", q)
})