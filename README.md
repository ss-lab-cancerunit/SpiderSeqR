# SpiderSeqR

## Overview
SpiderSeqR accelerates and simplifies genomic research by offering a new framework for searching and filtering metadata from SRA and GEO databases.

It builds upon the database files from SRAdb and GEOmetadb packages (which contain metadata of genomic datasets from NCBI SRA and GEO databases respectively) and enhances them by integrating them and adding tools for ease of search and retrieval.

Our aim was to make publically available genomic datasets more accessible to enable re-use of existing datasets for high-throughput analysis.

Some of the key features of SpiderSeqR include:  
- simulatenous search in both databases with a single query  
- manipulation of the results (including filtering and subsetting)  
- creating a record of previous searches (for future re-use)  
- conversion between (and within) SRA and GEO database entries  

## Functions
Main SpiderSeqR functions:
- searchForTerm - search for a specific term (tailored to finding particularly ChIP-Seq and RNA-Seq and identifying their inputs/controls)
- searchAnywhere - search for a specific term. Broader scope, but less specific than searchForTerm
- searchForAccession - search for specific SRA/GEO accession numbers
- convertAccession - convert between all the SRA and GEO accession types
- filterByTerm, filterByTermByAccessionLevel - filter results
- rerunSpiderSeqR - run a query again


## Documentation
For further information use:
- the vignette:
    * [Quick_SpiderSeqR.Rmd](vignettes/Quick_SpiderSeqR.Rmd) has basic usage examples
- help pages (e.g. help("SpiderSeqR"), package?SpiderSeqR, ?startSpiderSeqR, ?searchForTerm, ?searchAnywhere, ?convertAccession, etc.)
