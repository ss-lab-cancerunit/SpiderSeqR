#Copy of pryr's where function (couldn't load pryr)
where <- function(name, env = parent.frame()) {
    if (identical(env, emptyenv())) {
      # Base case
      stop("Can't find ", name, call. = FALSE)

    } else if (exists(name, envir = env, inherits = FALSE)) {
      # Success case
      env

    } else {
      # Recursive case
      where(name, parent.env(env))

    }
}

.onLoad <- function(libname, pkgname){
  #print("Setting up SpideR")

  if(!file.exists("GEOmetadb.sqlite")){
    #if(1==1){
    #if(!file.exists("test.txt")){
    print("The file GEOmetadb.sqlite was not found in the current working directory")
    print("Would you like to download the file now?")
    geo_menu <- menu(c("yes", "no"))
    if (geo_menu == 1){
      print("Downloading the file")
    } else {
      stop("GEOmetadb.sqlite is necessary to initiate package operation")
    }
  }

  if(!file.exists("SRAmetadb.sqlite")){
    #if(!file.exists("test.txt")){
    print("The file SRAmetadb.sqlite was not found in the current working directory")
    print("Would you like to download the file now?")
    geo_menu <- menu(c("yes", "no"))
    if (geo_menu == 1){
      print("Downloading the file")
    } else {
      stop("SRAmetadb.sqlite is necessary to initiate package operation")
    }
  }


  if (file.exists("SRAmetadb.sqlite") & file.exists("GEOmetadb.sqlite")){

    print("Both db files are present (remember not to remove them!). Ready to proceed")


    #===*=== Remove when the time comes

    #This didn't work
    #sra_con <- dbConnect(SQLite(), dbname = 'SRAmetadb.sqlite')
    #geo_con <- dbConnect(SQLite(),'GEOmetadb.sqlite')


    #This did get the connections to the global environment, but the functions were not able to access it
    #Two options:
    # - change the functions to search for sra_con and geo_con in global environment
    # - establish db connection within each function
    #.GlobalEnv$sra_con <- dbConnect(SQLite(), dbname = 'SRAmetadb.sqlite')
    #.GlobalEnv$geo_con <- dbConnect(SQLite(),'GEOmetadb.sqlite')

    #print(where("sra_con"))
  }

}

.onAttach <- function(libname, pkgname){
  packageStartupMessage("Welcome to SpideR")
}
