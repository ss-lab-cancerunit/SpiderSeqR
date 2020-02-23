
context("convertAccession")

#' Testing conversion between databases
#' Things to be verified
#' - (demo/real) commutative - searching in SRA first = searching in GEO first (and vice versa)
#' - (demo) associative - searching for accessions individually yields same results as searching for all at the same time
#' - (demo) count - count of accessions in the database is equal to count as a result of the conversion
#' ===*=== further expand 





# Setup ####

n_tests <- 5
demo <- TRUE

#n_tests <- 1
#demo <- FALSE # only commutative atm


if (demo){
  setSpiderSeqROption("quiet", TRUE)
  startSpiderSeqRDemo()
} else {
  ori_wd <- getwd()
  startSpiderSeqR("C:/DD/Projects/SpideRs/SpiderSeqR-Auxillaries/Database_Files", general_expiry = 100000)
  setwd(ori_wd)
  setSpiderSeqROption("quiet", TRUE)
}



# Helper functions ####




#' Generate random accessions from SRA
#' 
#' @param x Character with accession type
#' @param n Number of accessions desired
#' @param max_runs Maximum number of runs per accession allowed (will keep iterating until accession within the maximum is found)
#' @return A character vector with the randomly generated accessions
#' 
randomSRA <- function(x, n = 1, max_runs = 100){
  z <- character(length = n)
  for (i in 1:n){
    y <- character()
    y <- DBI::dbGetQuery(sra_con, paste0("SELECT ", x, " FROM sra GROUP BY RANDOM() LIMIT ", 1))
    y <- y[,1]
    acc_number <- as.numeric(DBI::dbGetQuery(sra_con, paste0("SELECT count(*) FROM sra WHERE ", x, " = '", y, "'")))
    print(y)
    z[i] <- y
    
    while(acc_number > max_runs){
      y <- DBI::dbGetQuery(sra_con, paste0("SELECT ", x, " FROM sra GROUP BY RANDOM() LIMIT ", 1))
      y <- y[,1]
      acc_number <- as.numeric(DBI::dbGetQuery(sra_con, paste0("SELECT count(*) FROM sra WHERE ", x, " = '", y, "'")))
      print(y)
      z[i] <- y
    }
  }
  
  return(z)
}


randomGEO <- function(x = c("gsm", "gse"), n = 1, max_gsms = 100){
  x <- match.arg(x)
  
  if (x == "gsm"){
    z <- DBI::dbGetQuery(geo_con, paste0("SELECT gsm FROM gsm GROUP BY RANDOM() LIMIT ", n))
    z <- z[,1]
  }
  
  if (x == "gse"){
    z <- character(length = n)
    for (i in 1:n){
      y <- DBI::dbGetQuery(geo_con, paste0("SELECT gse FROM gse GROUP BY RANDOM() LIMIT 1"))
      y <- y[,1]
      
      gsm_count <- DBI::dbGetQuery(geo_con, paste0("SELECT count(*) FROM gsm WHERE series_id LIKE '%",  y, "' OR series_id LIKE '%", y, ",%'" ))
      gsm_count <- as.numeric(gsm_count)
      
      while (gsm_count > 100){
        y <- DBI::dbGetQuery(geo_con, paste0("SELECT gse FROM gse GROUP BY RANDOM() LIMIT 1"))
        y <- y[,1]
        print(y)
        
        gsm_count <- DBI::dbGetQuery(geo_con, paste0("SELECT count(*) FROM gsm WHERE series_id LIKE '%",  y, "' OR series_id LIKE '%", y, ",%'" ))
        gsm_count <- as.numeric(gsm_count)
        
      }
      
      z[i] <- y
      
      
    }
    
  }
  
  
  return(z)
}




# Commutative ####




# SRA first

i <- 0
while (i < n_tests){ # Test up to n times
  
  # Get random SRP
  if (demo){
    x <- sample(unique(sra_demo$study_accession), 1)
  } else {
    x <- randomSRA("study_accession", 1)
  }
  
  
  sra_input <- convertAccession(x)
  
  if (sum(grepl("GSM\\d\\d\\d+", sra_input$gsm))>0){
    i <- i + 1
    geo_input <- convertAccession(sra_input$gsm)
    test_that("Commutative conversion SRA -> GEO", {
      expect_identical(sra_input, geo_input)
    })
  }
  
}




# GEO first

i <- 0
while (i < n_tests){ # Test up to n times
  
  if (demo){
    #x <- sample(unique(gse_demo$gse), 1)
    x <- sample(unique(unlist(strsplit(gsm_demo$series_id, split = ","))), 1)
  } else {
    x <- randomGEO("gse", 1)
  }
  
  
  
  geo_input <- convertAccession(x)
  
  if (sum(grepl("SRR\\d\\d\\d+", geo_input$run_accession))>0){
    i <- i + 1
    sra_input <- convertAccession(geo_input$run_accession)
    test_that("Commutative conversion GEO -> SRA", {
      expect_identical(geo_input, sra_input)
      
    })
  }
}





# Associative ####

# SRA first

for (i in 1:n_tests){
  x <- sample(unique(sra_demo$study_accession), 3)
  
  x1 <- convertAccession(x[1])
  x2 <- convertAccession(x[2])
  x3 <- convertAccession(x[3])
  
  x_123 <- rbind(x1, x2, x3)
  
  x_123 <- unifyDFFormat(x_123)
  
  x_all <- convertAccession(x)
  
  
  test_that("Associative SRA -> GEO", {
    expect_identical(x_123, x_all)
  })
}



# GEO first

for (i in 1:n_tests){
  x <- sample(unique(gse_demo$gse), 3)
  
  x1 <- convertAccession(x[1])
  x2 <- convertAccession(x[2])
  x3 <- convertAccession(x[3])
  
  x_123 <- rbind(x1, x2, x3)
  
  x_123 <- unifyDFFormat(x_123)
  
  x_all <- convertAccession(x)
  
  
  test_that("Associative GEO -> SRA", {
    expect_identical(x_123, x_all)
  })
}






# Count check ####

# SRA (study_accession ONLY)


for (i in 1:n_tests){
  x <- sample(sra_demo$study_accession, 1)
  
  x_count <- as.numeric(DBI::dbGetQuery(sra_con, paste0("SELECT count(DISTINCT run_accession) FROM sra WHERE study_accession = '", x, "'")))
  
  x_conv <- convertAccession(x)
  
  test_that("Equal count SRA", {
    expect_equal(x_count, length(unique(x_conv$run_accession)))
    
  })
  
}




# GEO (series_id ONLY)


for (i in 1:n_tests){
  x <- sample(gse_demo$gse, 1)
  
  x_count <- as.numeric(DBI::dbGetQuery(geo_con, paste0("SELECT count(DISTINCT gsm) FROM gsm WHERE series_id LIKE '%", x, ",%' OR series_id LIKE '%", x, "'")))
  
  x_conv <- convertAccession(x)
  test_that("Equal count GEO", {
    expect_equal(x_count, length(unique(x_conv$gsm)))
  })
}








