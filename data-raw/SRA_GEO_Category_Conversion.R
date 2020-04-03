## code to prepare `SRA_GEO_Category_Conversion` dataset 

SRA_GEO_Category_Conversion <- read.csv("data-raw/SRA_GEO_Category_Conversion.csv", stringsAsFactors = FALSE)

usethis::use_data(SRA_GEO_Category_Conversion, compress = "xz")


