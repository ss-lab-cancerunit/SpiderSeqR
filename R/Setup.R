#These need to be included in the environment setup (currently can do them manually)

library(plyr)
library(dplyr) #PLYR needs to be loaded BEFORE dplyr
library(SRAdb)
library(GEOmetadb)


#Download database files
#===*=== Double check the lines below

if(!file.exists("GEOmetadb.sqlite")){
  geofile <- getSQLiteFile(destfile = "GEOmetadb.sqlite.gz")
}

srafile <- 'SRAmetadb.sqlite'
if(!file.exists("SRAmetadb.sqlite"))srafile <<- getSRAdbFile()



geo_con = dbConnect(SQLite(), dbname = "GEOmetadb.sqlite")
sra_con = dbConnect(SQLite(), dbname = "SRAmetadb.sqlite")
