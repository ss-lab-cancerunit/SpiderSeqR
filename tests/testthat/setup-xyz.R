


print("setup")


assign("x", 2, envir = .GlobalEnv)
print(ls(envir = .GlobalEnv))


q <- getSpiderSeqROption("quiet")

setSpiderSeqROption("quiet", TRUE)
startSpiderSeqRDemo()

