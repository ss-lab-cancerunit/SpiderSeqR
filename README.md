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

## Installation
Please follow the current installation instructions in the vignette ([Quick_SpiderSeqR.Rmd](vignettes/Quick_SpiderSeqR.Rmd))

## Documentation
For further information use:
- the vignette:
    * [Quick_SpiderSeqR.Rmd](vignettes/Quick_SpiderSeqR.Rmd) has basic usage examples
- help pages (e.g. help("SpiderSeqR"), package?SpiderSeqR, ?startSpiderSeqR, ?searchForTerm, ?searchAnywhere, ?convertAccession, etc.)

## Citation
Cite: Anna M. Sozanska,  Charles Fletcher,  Dóra Bihary,  Shamith A. Samarajiwa. "SpiderSeqR: an R package for crawling the web of high-throughput multi-omic data repositories for data-sets and annotation". 2020, bioRxiv, doi: https://doi.org/10.1101/2020.04.13.039420

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
