#Helper_Functions.R
#Functions used by searchForTerm()
#(and which will eventually be used by searchForAccession as well)




#CONNECTED AND MOVED
#searchSRA()
#verifyConditions()
#writeQuery()
#withOut()
#extractGSM()
#searchForSRPChildren()
#rbindUniqueCols()
#unifyNAs()
#universalExtractor()
#detectInputs()
#verifyColumns()
#detectControls()
#detectMerges()
#verifyMissingRuns()
#parQuery()
#convertPairedEnds()
#geoFinder()
#verifySuperseries()

#ALSO REQUIRED:
#Search_Functions.R (searchForTerm)

#Functions for output generation (outputGenerator and all associated functions)
#Output_Functions.R


#THINGS PREVIOUSLY TO BE CONNECTED (which will not be used):
#superseriesFinder()
###keyword_indexExtractor()
###extr()
###characteristicsExtractor()



#----------------------------------------------------------------------------

#============================================================================
#Functions (in the order of being called)
#===*=== Make sure this is actually the case
#============================================================================



#----------------------------------------------------------------------------
#Developed in searchForTerm5.R

#' 
#' An internal function for searchForTerm
#' 
#' @param SRA_library_strategy Character vector specifying experiment approach
#' @param gene ...and...
#' @param antibody ...and...
#' @param cell_type ...and...
#' @param treatment ...and...
#' @param species ...and...
#' @param platform Character vectors 
#' with information to search within SRA
#' @return A data frame with results
#' 
#' @keywords internal
searchSRA <- function(SRA_library_strategy, gene, antibody, 
                        cell_type, treatment, species, platform){
    mm("Running searchSRA", "fn")
    
    #------------------------------------------------
    #------------------------------------------------
    #TECHNICALITIES:
    #------------------------------------------------
    #------------------------------------------------
    database_name <- "sra_con"
    database_env <- ".GlobalEnv"
    sra_table <- "sra"
    
    # sra_columns <- c("experiment_name", "run_attribute", 
    #                "experiment_accession", "experiment_url_link", 
    #                "experiment_title", "library_strategy", "library_layout", 
    #                "sample_alias", "taxon_id", 
    #                "library_construction_protocol", "run_accession", 
    #                "study_accession", "run_alias", "experiment_alias", 
    #                "sample_name", "sample_attribute", "experiment_attribute")
    #sra_columns <- c("experiment_title")
    
    sra_columns <- "*"
    #------------------------------------------------
    library_strategy <- SRA_library_strategy
    #------------------------------------------------
    
    
    
    #LIST OF SELECTED COLUMNS 
    # (usually contain informative text about the sample)
    informative_fields <- c("run_alias", "experiment_name", 
                            "experiment_alias", "experiment_title", 
                            "sample_name", "library_name", 
                            "experiment_attribute", "sample_alias", 
                            "sample_attribute") 
    #RESEARCHED (based on p53 and STAT1, doxorubicin) 
    # ===*=== Consider making more investigations
    # ===*=== Be careful with sample_attribute 
    # (exclude from df verification step)
    
    
    #------------------------------------------------
    #GENE
    #------------------------------------------------
    ##OPTION 1: Simple search
    #gene_fields <- c("run_accession", "sample_attribute")
    #gene_prefixes <- list("[^a-z]*", c("antibody:([^\\|]*[^\\|a-z]+|\\s*)", 
    #                                   "genotype:([^\\|]*[^\\|a-z]+|\\s*)"))
    #gene_suffixes <- c("", "")
    
    #OPTION 2: Conditional search
    cat_gene <- c("genotype", "ArrayExpress.Genotype", 
                    "genotype/variation", "target.gene", 
                    "genetic.background", "host.genotype", "Plant.genotype", 
                    "genetic.modification", "transgene", "gene.id", 
                    "myd88.genotype", "gene.perturbation.type", 
                    "genetic.condition", "cytogenetics", 
                    "concise.genotype.name", "genspecies.abbr", 
                    "melanoma.genetic.conditions", "marker.gene", "gene", 
                    "strain/genotype", "genotype/variation", 
                    "knockout", "knockdown", "hgn") 
    #RESEARCHED (based on sa_categories)
    
    gene_fields <- informative_fields 
    #RESEARCHED (based on p53 and STAT1) 
    # ===*=== Consider making more investigations
    # ===*=== Be careful with sample_attribute 
    # (exclude from df verification step)
    # SAME AS ANTIBODY_FIELDS
    
    
    
    #------------------------------------------------
    # ANTIBODY
    #------------------------------------------------
    # NOTE:
    # When using conditional search, sample_attribute will be included anyway, 
    # so unless want to search throughout non-specific categories, 
    # don't include it in antibody_fields
    
    cat_antibody <- c("chip.antibody", "antibody", 
                        "ArrayExpress.Immunoprecipitate", "ip.antibody", 
                        "rip.antibody", "medip.antibody", "clip.antibody", 
                        "frip.antibody", "chip-seq.antibody") #RESEARCHED WELL
    
    antibody_fields <- informative_fields 
    # RESEARCHED (based on p53 and STAT1)
    # ===*=== Consider making more investigations
    # ===*=== Be careful with sample_attribute 
    # (exclude from df verification step)
    
    
    # Not in use if conditional search employed
    # Amended to include capital letters as well
    # antibody_prefixes <- rep("[^A-Za-z]*", length(antibody_fields)) 
    # antibody_prefixes <- 
    #    list("[^a-z]", c("antibody:([^\\|]*[^\\|a-z]+|\\s*)", 
    #                     "immunoprecipitate:([^\\|]*[^\\|a-z]+|\\s*)"))
    # antibody_suffixes <- rep("", length(antibody_fields))
    
    
    #------------------------------------------------
    #CELL_TYPE
    #------------------------------------------------
    cell_type_fields <- informative_fields #RESEARCHED
    
    
    
    #------------------------------------------------
    #TREATMENT
    #------------------------------------------------
    cat_treatment <- c("treatment", "ArrayExpress.Treatment", 
                        "treated.with", "treatment.description", 
                        "drug.treatment", "treatment.protocol", 
                        "Vaccine.Treatment", "experimental.treatment", 
                        "diet.treatment", "treatment.group") #RESEARCHED
    
    treatment_fields <- informative_fields 
    #RESEARCHED (so far based on doxorubicin only) ===*===
    
    
    
    
    #------------------------------------------------
    #LIBRARY_STRATEGY
    #------------------------------------------------
    
    #------------------------------------------------
    #------------------------------------------------
    #OBTAIN THE DF VIA SQL QUERY
    #------------------------------------------------
    #------------------------------------------------
    mm("Creating an SQL query", "fn")
    
    #COLLAPSE COLUMNS
    sra_columns <- paste(sra_columns, collapse = ", ")
    
    #ENLIST ALL SEARCH TERMS AND FIELDS
    search <- list(gene, antibody, cell_type, treatment, 
                    species, library_strategy, platform)
    fields <- list(gene_fields, antibody_fields, cell_type_fields, 
                    treatment_fields, "taxon_id", 
                    "library_strategy", "platform")
    
    if (length(search)!=length(fields)){
        warning("Search terms and search fields differ in number")
    }
    
    #COMPOSE THE QUERY
    query <- paste0("SELECT ", sra_columns, " FROM ", 
                    sra_table, " WHERE ", sep=" ")
    for (s in seq_along(search)){
        if (length(search[[s]]!=0)){ #Only include non-empty search term types
            query <- paste0(query, "(", 
                            writeQuery(search[[s]], fields[[s]]), " ) AND ")
            #print(query)
        }
    }
    query <-substr(query, 1, (nchar(query)-4)) #Remove the final "AND "
    query <- paste0(query, "") #Can add a parenthesis here if necessary
    mm(query, "query")
    
    
    #GET THE QUERY
    output_list <- DBI::dbGetQuery(get(database_name, 
                                        envir = get(database_env)), query)
    
    #STOP IF NO RESULTS
    if ( (dim(output_list)[1]) == 0 ) {
        stop(paste0("No results found for input symbols, check ",
        "symbols/synonyms entered or whether such entries exist on ncbi."))
    }
    
    mm("SQL query completed", "fn")
    
    message(paste0("Found ", dim(output_list)[1], " results")) #===*===
    .GlobalEnv$spider_output_list_sra_unfiltered <- output_list
    
    #------------------------------------------------
    #------------------------------------------------
    #CHECK FOR VALIDITY OF OUTPUT_LIST
    #------------------------------------------------
    #------------------------------------------------
    
    mm("Begining verification of the data frame", "fn")
    
    #------------------------------------------------
    # NOTES ON REGEXP:
    #------------------------------------------------
    #Something along the lines of:
    #grepl("antibody:[^|]+STAT1", i_gen2$sample_attribute)
    
    #INITIAL ATTEMPT (not very good!)
    #template of regexp for space between title and keyword:
    #   ([^\\|]*[^\\|a-z]+|\\s*)
    
    #REGEXP FOR SEARCHING:
    #grep("(^|\\|\\| )chip.antibody:(|[^\\|:]*?[^A-Za-z])p53", 
    # "chip antibody: sth p53")
    #Features: matches empty string or any string 
    #(as long, as it does not contain letters as the last element)
    #E.g. MATCHES: chip_antibody:p53, chip_antibody: something, something-p53
    #     DOESN'T MATCH: chip_antibody:ap53, chip_antibody: somethingp53
    
    #VERSION 2:
    #grep("(^|\\|\\| )chip.antibody:(|[^\\|:]*?[^A-Za-z\\|:])p53", 
    #"chip_antibody: :p53")
    #Features: prevents | and : from immediately preceding p53
    
    #VERSION 3:
    #grep("(^|\\|\\| )chip.antibody:(|[^\\|]*?[^A-Za-z\\|])p53", 
    #"chip_antibody: p53")
    #Features: removed : from prohibited characters 
    #(only | is prohibited and letters immediately preceding p53)
    
    
    #Consider using a generic prefix where appropriate: [^a-z] ===*===
    #------------------------------------------------
    
    
    
    #------------------------------------------------
    #GENE
    #------------------------------------------------
    
    #------------------------------------------------
    #simple search:
    #gene_indices <- verifyConditions(output_list, gene, 
    #gene_fields, gene_prefixes, gene_suffixes)
    #------------------------------------------------
    
    #------------------------------------------------
    #Conditional search (based on antibody)
    
    #GENE - LOGIC:
    #- if any of the gene categories exist within sample_attribute field, 
    # search within those
    #- if none of the gene categories exist within sample_attribute field, 
    #search within gene_fields (except sample_attribute)
    #NOTE: gene cannot be preceded by a letter (lowercase or uppercase)
    
    if (length(gene)!=0){
        #Find rows which contain categories in sample_attribute field
        cat_gene_indices <- verifyConditions(output_list, cat_gene, 
                                                "sample_attribute", 
                                                "(^|\\|\\| )", ":" )
        
        #Initialise vector for rows with matches to gene query
        gene_indices <- rep(NA, nrow(output_list))
        
        #Wrap regular expression around categories
        cat_gene_prefixes <- paste0("(^|\\|\\| )", cat_gene, 
                                    ":(|[^\\|]*?[^A-Za-z\\|])") 
        #Use a non-greedy quantifier and alternation
        
        #Search within sample_attribute field 
        #(when matches to categories occur)
        gene_indices[cat_gene_indices] <- 
            verifyConditions(output_list[cat_gene_indices, ], gene, 
                                "sample_attribute", list(cat_gene_prefixes))
        
        
        #Search within other fields (if there were no matches to categories)
        gene_indices[!cat_gene_indices] <- 
            verifyConditions(output_list[!cat_gene_indices, ], 
                            gene, withOut("sample_attribute", gene_fields), 
                                "(^|[^A-Za-z])") 
        #Noletter prefix as a separate input
        
        mm(paste0(sum(gene_indices), " out of ", dim(output_list)[1], 
                    " initial entries comply with the gene criteria"), "res")
    } else {
        gene_indices <- rep(TRUE, nrow(output_list))
        mm("No genes specified. Returned all TRUE", "adverse")
    }
    
    
    #------------------------------------------------
    
    
    
    ##------------------------------------------------
    ##'Test' to visualise the effects:
    #testg <- data.frame(output_list$sample_attribute, 
    #output_list$run_alias, output_list$experiment_title, 
    #output_list$experiment_alias, gene_indices)
    #testg$extract <- NA
    
    #gen_ind <- grep("(^|\\|\\| )genotype:", output_list$sample_attribute)
    #testg$extract[gen_ind] <-  gsub("(^|^.*\\|\\| )(genotype:[^\\|:]+).*$", 
    #"\\2", output_list$sample_attribute[gen_ind])
    
    #h_gen_ind <- grep("(^|\\|\\| )host.genotype:", 
    #output_list$sample_attribute)
    #testg$extract[h_gen_ind] <-  
    #gsub("(^|^.*\\|\\| )(host.genotype:[^\\|:]+).*$", 
    #"\\2", output_list$sample_attribute[h_gen_ind])
    ##------------------------------------------------
    
    
    
    
    
    #------------------------------------------------
    #ANTIBODY
    #------------------------------------------------
    
    #------------------------------------------------
    #simple search:
    #antibody_indices <- verifyConditions(output_list, antibody, 
    #antibody_fields, antibody_prefixes, antibody_suffixes)
    #------------------------------------------------
    
    
    ##------------------------------------------------
    #Conditional search 
    #(if any of the category synonyms exist, only search within them)
    #INITIAL VERSIONS
    
    ##cat_antibody_indices <- verifyConditions(output_list, list("antibody"), 
    #"sample_attribute", list(rep("(^|\\|\\| )", nrow(output_list))), 
    #list(rep(":", nrow(output_list))) ) 
    #the size of prefixes/suffixes is unnecessary
    
    ##Find rows which contain categories in sample_attribute field
    
    ##cat_antibody_indices <- verifyConditions(output_list, 
    #list("antibody", "chip antibody"), "sample_attribute", 
    #"(^|\\|\\| )", ":" ) #worked
    ##cat_antibody_indices <- verifyConditions(output_list, 
    #c("antibody", "chip antibody"), "sample_attribute", "(^|\\|\\| )", ":" ) 
    #also worked
    
    #cat_antibody_indices <- verifyConditions(output_list, 
    #cat_antibody, "sample_attribute", "(^|\\|\\| )", ":" )
    
    ##Initialise vector for rows with matches to antibody query
    #antibody_indices <- rep(NA, nrow(output_list))
    
    
    
    ##OPTION 1: Search just for antibody
    #cat_antibody_prefixes <- paste0("(^|\\|\\| )", cat_antibody, ":[^\\|:]+")
    #antibody_indices[cat_antibody_indices] <- 
    #verifyConditions(output_list[cat_antibody_indices, ], antibody, 
    #"sample_attribute", list(cat_antibody_prefixes))
    
    #antibody_indices[!cat_antibody_indices] <- 
    #verifyConditions(output_list[!cat_antibody_indices, ], antibody, 
    #antibody_fields, antibody_prefixes, antibody_suffixes)
    
    
    ##OPTION 2: Search for antibody not immediately preceded by any letters
    #cat_antibody_prefixes <- paste0("(^|\\|\\| )", 
    #cat_antibody, ":[^\\|:]*?") #Use a non-greedy quantifier
    #antibody_noletter <- paste0("(^|[^A-Za-z])", antibody)
    ##antibody_noletter <- antibody
    #antibody_indices[cat_antibody_indices] <- 
    #verifyConditions(output_list[cat_antibody_indices, ], 
    #antibody_noletter, "sample_attribute", list(cat_antibody_prefixes))
    
    ##antibody_indices[!cat_antibody_indices] <- 
    #verifyConditions(output_list[!cat_antibody_indices, ], 
    #antibody_noletter, antibody_fields, antibody_prefixes, antibody_suffixes) 
    #Deleting prefixes and suffixes since antibody_noletter 
    #already includes that information
    
    ##antibody_indices[!cat_antibody_indices] <- 
    #verifyConditions(output_list[!cat_antibody_indices, ], 
    #antibody_noletter, withOut("sample_attribute", antibody_fields)) 
    #special variable used - antibody_noletter
    #antibody_indices[!cat_antibody_indices] <- 
    #verifyConditions(output_list[!cat_antibody_indices, ], 
    #antibody, withOut("sample_attribute", antibody_fields), "(^|[^A-Za-z])") 
    #Noletter prefix
    ##------------------------------------------------
    
    
    #------------------------------------------------
    #NEW VERSION (conditional search)
    
    #ANTIBODY - LOGIC:
    #- if any of the antibody categories exist within sample_attribute field, 
    #search within those
    #- if none of the antibody categories exist within sample_attribute field, 
    #search within antibody_fields (except sample_attribute)
    #NOTE: antibody cannot be preceded by a letter (lowercase or uppercase)
    
    if (length(antibody)!=0){
        #Find rows which contain categories in sample_attribute field
        cat_antibody_indices <- verifyConditions(output_list, cat_antibody, 
                                                "sample_attribute", 
                                                "(^|\\|\\| )", ":" )
        
        #Initialise vector for rows with matches to antibody query
        antibody_indices <- rep(NA, nrow(output_list))
        
        #Wrap regular expression around categories
        cat_antibody_prefixes <- paste0("(^|\\|\\| )", 
                                        cat_antibody, 
                                        ":(|[^\\|]*?[^A-Za-z\\|])") 
        #Use a non-greedy quantifier
        
        #Search within sample_attribute field(when matches to categories occur)
        antibody_indices[cat_antibody_indices] <- 
            verifyConditions(output_list[cat_antibody_indices, ], antibody, 
                            "sample_attribute", list(cat_antibody_prefixes))
        
        #Search within other fields (if there were no matches to categories)
        antibody_indices[!cat_antibody_indices] <- 
            verifyConditions(output_list[!cat_antibody_indices, ], 
                                antibody, withOut("sample_attribute", 
                                                antibody_fields), 
                                "(^|[^A-Za-z])") 
        #Noletter prefix as a separate input
        
        mm(paste0(sum(antibody_indices), " out of ", dim(output_list)[1], 
                " initial entries comply with the antibody criteria"), "res")
    } else {
        antibody_indices <- rep(TRUE, nrow(output_list))
        mm("No antibodies specified. Returned all TRUE", "adverse")
    }
    
    #------------------------------------------------
    
    
    
    ##--------------------------------------------------------
    ##'Test' to visualise the effects:
    #test2 <- data.frame(output_list$sample_attribute, 
    #output_list$run_alias, output_list$experiment_title, 
    #output_list$experiment_alias, antibody_indices)
    #test2$extract <- NA
    
    #ab_ind <- grep("(^|\\|\\| )antibody:", output_list$sample_attribute)
    #test2$extract[ab_ind] <-  gsub("(^|^.*\\|\\| )(antibody:[^\\|:]+).*$", 
    #"\\2", output_list$sample_attribute[ab_ind])
    #
    #ch_ab_ind <- grep("(^|\\|\\| )chip antibody:", 
    #output_list$sample_attribute)
    #test2$extract[ch_ab_ind] <-  
    #gsub("(^|^.*\\|\\| )(chip antibody:[^\\|:]+).*$", "\\2", 
    #output_list$sample_attribute[ch_ab_ind])
    ##--------------------------------------------------------
    
    
    
    
    
    #------------------------------------------------
    #CELL_TYPE
    #------------------------------------------------
    cell_type_indices <- verifyConditions(output_list, 
                                            cell_type, cell_type_fields) 
    #There is a very extensive list of categories with tissue information
    
    #Conditional search will not be used for now
    mm(paste0(sum(cell_type_indices), " out of ", dim(output_list)[1], 
            " initial entries comply with the cell type criteria"), "res")
    
    #------------------------------------------------
    #TREATMENT
    #------------------------------------------------
    
    ##------------------------------------------------
    ##Simple search
    #treatment_indices <- verifyConditions(output_list, 
    #treatment, treatment_fields)
    ##------------------------------------------------
    
    #------------------------------------------------
    #Conditional search (based on antibody)
    
    #TREATMENT - LOGIC:
    #- if any of the treatment categories exist within sample_attribute field, 
    #search within those
    #- if none of the treatment categories exist within sample_attribute field, 
    #search within treatment_fields (INCLUDING sample_attribute)
    #NOTE: treatment CAN be preceded by a letter (lowercase or uppercase)
    
    #Treatment-specific changes:
    #- Letters allowed before treatment
    #    [cat_treatment_prefixes changed]
    #    [treatment_indices[!cat_treatment_indices] - removed prefixes]
    #- Alternative search can also take place in sample_attribute field
    
    if (length(treatment)!=0){
        #Find rows which contain categories in sample_attribute field
        cat_treatment_indices <- verifyConditions(output_list, cat_treatment,
                                                    "sample_attribute", 
                                                    "(^|\\|\\| )", ":" )
        
        #Initialise vector for rows with matches to treatment query
        treatment_indices <- rep(NA, nrow(output_list))
        
        #Wrap regular expression around categories
        cat_treatment_prefixes <- paste0("(^|\\|\\| )", 
                                        cat_treatment, ":(|[^\\|]*?)") 
        #Use a non-greedy quantifier and alternation
        
        
        #Search within sample_attribute field 
        #(when matches to categories occur)
        treatment_indices[cat_treatment_indices] <- 
            verifyConditions(output_list[cat_treatment_indices, ], 
                                treatment, "sample_attribute", 
                                list(cat_treatment_prefixes))
        
        #Search within other fields (if there were no matches to categories)
        treatment_indices[!cat_treatment_indices] <- 
            verifyConditions(output_list[!cat_treatment_indices, ], 
                                treatment, treatment_fields) 
        #Noletter prefix as a separate input
        
        mm(paste0(sum(treatment_indices), " out of ", dim(output_list)[1], 
                " initial entries comply with the treatment criteria"), "res")
    } else {
        treatment_indices <- rep(TRUE, nrow(output_list))
        mm("No treatment specified. Returned all TRUE", "adverse")
    }
    
    #------------------------------------------------
    
    
    
    
    
    #------------------------------------------------
    #SPECIES
    #------------------------------------------------
    species_indices <- verifyConditions(output_list, species, 
                                            "taxon_id")
    mm(paste0(sum(species_indices), " out of ", dim(output_list)[1], 
                " initial entries comply with the species criteria"), "res")
    
    #------------------------------------------------
    #LIBRARY_STRATEGY
    #------------------------------------------------
    library_strategy_indices <- verifyConditions(output_list, 
                                                library_strategy, 
                                                "library_strategy")
    
    mm(paste0(sum(library_strategy_indices), " out of ", dim(output_list)[1], 
                " initial entries comply with the library strategy criteria"), 
        "res")
    
    
    #------------------------------------------------
    #PLATFORM
    #------------------------------------------------
    platform_indices <- verifyConditions(output_list, platform, "platform")
    mm(paste0(sum(platform_indices), " out of ", dim(output_list)[1], 
                " initial entries comply with the platform criteria"), "res")
    
    
    #------------------------------------------------
    #------------------------------------------------
    #COMBINE CONDITIONS AND PROVIDE OUTPUT
    #------------------------------------------------
    #------------------------------------------------
    
    output_indices <- gene_indices & antibody_indices & 
        cell_type_indices & treatment_indices & species_indices & 
        library_strategy_indices & platform_indices
    
    output_list <- output_list[output_indices,] #Only leave the matching rows
    
    # Rename columns
    #output_list <- renameSRAColumns(output_list)
    
    message(paste0(dim(output_list)[1], 
                    " results passed verification phase")) # ===*===
    
    .GlobalEnv$spider_output_list_sra_filtered <- output_list
    
    if (dim(output_list)[1]==0) {
        stop("No results passed the verification phase")
    }
    mm("Number of entries returned:", "res")
    mm(dim(output_list)[[1]], "res")
    mm("searchSRA completed", "fn")
    return(output_list)
    
}

#----------------------------------------------------------------------------





#----------------------------------------------------------------------------
#Developed in searchForTerm5.R
#Newer version: will allow to have atomic prefixes/suffixes vector, 
#which will get repeated accordingly
#Now prints regexp and the name of the column being searched
#Enabled perl
#------------------------------------------------


#' Verify whether conditions are met in a data frame
#' 
#' @param df Data frame to be verified
#' @param keywords Vector of strings to be searched for
#' @param columns Vector with column names (within df) to be searched for
#' @param prefixes (Optional) Vector or list with prefixes to keywords 
#' (its first dimension needs to be the same as columns)
#' @param suffixes (Optional) Vector or list with suffixes to keywords
#' (its first dimension needs to be the same as columns)
#' 
#' @return Logical vector (same length as df) 
#' with TRUE indicating matches to the conditions
#' 
#' @description Verify if the specified columns within the data frame 
#' match specified conditions. 
#' Match is performed using grepl (case insensitive, with perl set to TRUE).
#' Any given row will be returned as TRUE if ANY of its columns 
#' matched to ANY of the keywords with ANY of the prefixes 
#' and ANY of the suffixes.
#' 
#' @keywords internal
#' 
verifyConditions <- function(df, keywords, columns, prefixes, suffixes){
    mm("Running verifyConditions", "fn")
    
    if (missing(prefixes)){
        prefixes <- rep("", length(columns))
    }
    if (missing(suffixes)){
        suffixes <- rep("", length(columns))
    }
    
    if (length(prefixes)!=length(columns)){
        if (length(prefixes)==1){
            prefixes <- rep(prefixes, length(columns))
            warning("INFO: Replicated prefixes to match the number of columns")
        } else {
            stop("Prefix vector needs to be the same size as columns vector")
        }
    }
    
    if (length(suffixes)!=length(columns)){
        if (length(suffixes)==1){
            suffixes <- rep(suffixes, length(columns))
        } else {
            stop(paste0("INFO: Suffix vector needs to be the same size ",
                        "as columns vector"))
        }
    }
    
    
    
    if (length(keywords)!=0){
        
        row_matches <- rep(FALSE, nrow(df))
        
        for (c in seq_along(columns)){
            for (k in seq_along(keywords)){
                
                
                columns_index <- grep(paste0("^", columns[c], "$"), 
                                        colnames(df))
                #print(colnames(df))
                #print(colnames(df)[columns_index])
                
                if (length(columns_index)>1){
                    warning("Multiple columns match to the same name")
                } else if (length(columns_index)==0){
                    warning("No matches to the specified column")
                }
                for (p in seq_along(prefixes[[c]])){
                    for (s in seq_along(suffixes[[c]])){
                        
                        keywords_regexp <- paste0(prefixes[[c]][p], 
                                                keywords[k], suffixes[[c]][s])
                        mm(paste(keywords_regexp, "IN", 
                                    colnames(df)[columns_index]), "query")
                        #print(colnames(df)[columns_index])
                        #print(keywords_regexp)
                        row_matches <- row_matches | grepl(keywords_regexp, 
                                                        df[,columns_index], 
                                                        ignore.case = TRUE, 
                                                        perl = TRUE)
                    }
                }
            }
        }
        
    } else {
        row_matches <- rep(TRUE, nrow(df))
        message("No keywords specified. All TRUE returned")
    }
    mm("verifyConditions completed", "fn")
    
    return(row_matches)
    
}
#------------------------------------------------





#------------------------------------------------
#term <- c("term1", "term2")
#fields <- c("field", "field2", "field3")


#' Automate Writing SQLite Queries
#' 
#' @param term Character vector with strings of interest
#' @param fields character vector with columns of interest
#' @param sql_before SQLite query string before term
#' @param sql_after SQLite query string after term
#' 
#' @return Part of the  SQLite query pertaining to row selection criteria 
#' (i.e. that comes after specifying the table and which columns to select)
#' 
#' @keywords internal
#' 
writeQuery <- function(term, fields, sql_before=" LIKE '%", sql_after="%'"){

    #if (is.na(sql_before)){
    #  sql_before <- " LIKE '%"
    #}
    
    #if (is.na(sql_after)){
    #  sql_after <- "%'"
    #}
    mm("Running writeQuery", "fn")
    
    query <- character()
    for (t in term){
        for (f in fields){
            query <- paste0(query, " (", f, sql_before, t, sql_after, ") OR")
        }
    }
    
    #Remove the last "OR" (which is redundant)
    query <- substr(query, 1, nchar(query)-3) 
    mm("writeQuery completed", "fn")
    return(query)
}
#------------------------------------------------





#------------------------------------------------

#' Remove matching elements from vector
#' 
#' @param names Character vector with values to be removed
#' @param vector Character vector from which elements matching 
#' to names will be removed
#' 
#' @return Original vector without specified elements
#' 
#' @keywords internal
withOut <- function(names, vector){
    mm("Running withOut", "fn")
    names <- unique(names)
    for (n in seq_along(names)){
        ind <- grep(names[n], vector)
        if (length(ind)!=0){
            vector <- vector[-ind]
        }
    }
    mm("withOut completed", "fn")
    return(vector)
}

#------------------------------------------------
#----------------------------------------------------------------------------




#----------------------------------------------------------------------------
#Developed in gsmExtractor.R 
#which also contains previous versions of the function
#New version of gsmExtractor - based on gsub tagging system


#' Extract GSM from the SRA_experiment_title column
#' 
#' @param output_list Data frame with (SRA_)experiment_title column
#' @param sampleColumn Logical indicating whether a new column 
#' should be added with extracted GSM numbers
#' 
#' @return Original data frame with extracted GSMs from (SRA_)experiment_title
#' column and (if sampleColumn is set to TRUE) a new column 
#' with extracted GSM numbers
#' 
#' @keywords internal
#' 
extractGSM <- function(output_list, sampleColumn = TRUE){
    mm("Running extractGSM", "fn")
    #Find indices of rows which contain GSMs
    
    # Rename SRA_experiment_title
    rename_experiment_title <- FALSE
    if (sum(grepl("SRA_experiment_title", colnames(output_list)))==1){
        rename_experiment_title <- TRUE
        colnames(output_list)[grepl(
                                "SRA_experiment_title", 
                                colnames(output_list))] <- "experiment_title"
    }
    
    
    #Safer option, but not strictly necessary, 
    #because GSM is always followed by ": ".
    gsm_indices <- grep("^GSM\\d\\d\\d+: ", output_list$experiment_title)
    #gsm_indices <- grep("^GSM\\d\\d\\d+", output_list$experiment_title) 
    
    
    if (sampleColumn == TRUE){
        #Create a new column
        output_list$gsm <- NA # sampletogsm ===*===
        
        #Extract the GSMs to sample column
        output_list$gsm[gsm_indices] <- 
            gsub("^(GSM\\d\\d\\d+).*$", "\\1", 
                    output_list$experiment_title[gsm_indices]) 
        # sampletogsm ===*===
    }
    
    
    #Remove the GSMs from experiment_title column
    output_list$experiment_title[gsm_indices] <- 
        gsub("^GSM\\d\\d\\d+: ", "", output_list$experiment_title[gsm_indices])
    
    
    # Rename experiment_title back to SRA_experiment_title
    if (rename_experiment_title){
        colnames(output_list)[
            grepl("experiment_title", colnames(output_list))] <- 
                                                        "SRA_experiment_title"
    }
    
    mm("extractGSM completed", "fn")
    return(output_list)
}
#----------------------------------------------------------------------------




#----------------------------------------------------------------------------
#[Developed in getSRP_indev.R]
#This is the most up to date copy


#' Search for missing SRRs within SRPs
#' 
#' @param srp_list Character vector with SRPs to search for
#' @param srp_columns Character vector with names of columns 
#' (within sra table of SRA) to be retrieved from the database
#' NOTE: can also be "*", i.e. all columns
#' @return Data frame with all the SRRs belonging to specified SRPs
#' 
#' @description Search for SRPs within SRA database to retrieve 
#' their contents (i.e. all SRRs) as a data frame.
#' 
#' @keywords internal
searchForSRPChildren <- function(srp_list, srp_columns){
    #Aims:
    #- Find all the rows in the database containing relevant SRPs 
    #(This is equivalent to finding all SRRs belonging to a given SRP)
    #- Extract relevant columns from the sra table
    # ===*=== COMPARE PERFORMANCE AGAINST PARAMETRISED QUERY
    mm("Running searchForSRPChildren", "fn")
    
    database_name <- "sra_con"
    database_env <- ".GlobalEnv"
    
    srp_all <- data.frame()
    srp_columns_collapsed <- paste(srp_columns, collapse = ", ")
    
    for (srp in srp_list){
        srp_query <- paste0("SELECT ", srp_columns_collapsed, 
                            " FROM sra WHERE study_accession = '", srp, "'")
        srp_entry <- DBI::dbGetQuery(
                                get(database_name, envir = get(database_env)), 
                                srp_query)
        
        srp_all <- rbind(srp_all, srp_entry)
    }
    
    # Rename SRA columns
    #srp_all <- renameSRAColumns(srp_all)
    
    mm("searchForSRPChildren completed", "fn")
    return(srp_all)
}
#----------------------------------------------------------------------------



#----------------------------------------------------------------------------
#Developed in rbindUniqueCols.R
#Replaced rbindUnique (which worked, but added an input column within itself)
#New features:
# - rbinds two dfs based on all columns with the exception of disregard_columns


#' Bind unique entries from two dfs disregarding some of the columns
#' 
#' @param x, y Data frames to be merged (need to have the same columns; 
#' entries that are duplicated among the dfs will be removed from y)
#' @param disregard_columns Character vector with names of columns 
#' which will be ignored when finding unique rows 
#' of combined x and y data frame
#' @return Data frame with all unique rows from x and y, 
#' when considered disregarding specified columns 
#' (only those rows of y that do not exist in x will be preserved)
#' 
#' @keywords internal
#' 
rbindUniqueCols <- function(x, y, disregard_columns){

    mm("Running rbindUniqueCols", "fn")
    
    if (!setequal(colnames(x), colnames(y))){
        stop("Column names need to match between the two data frames")
    }
    
    #Check for presence of disregard_columns in x and y
    verifyColumns(x, disregard_columns)
    verifyColumns(y, disregard_columns)
    
    x_dc_indices <- c()
    y_dc_indices <- c()
    
    #Get indices of columns within x and y
    for (c in seq_along(disregard_columns)){
        x_dc_indices <- append(x_dc_indices, grep(paste0("^", 
                                                        disregard_columns[c], 
                                                        "$"), 
                                                colnames(x)))
        
        y_dc_indices <- append(y_dc_indices, grep(paste0("^", 
                                                        disregard_columns[c], 
                                                        "$"), 
                                                colnames(y)))
    }
    
    
    #Order and eliminate duplicates
    x_dc_indices <- unique(x_dc_indices[order(x_dc_indices)])
    y_dc_indices <- unique(y_dc_indices[order(y_dc_indices)])
    
    #print(x_dc_indices)
    #print(y_dc_indices)
    
    if (length(x_dc_indices)>=dim(x)[2]){
        stop("Cannot disregard more columns than there are in the data frame")
    }
    
    if (length(y_dc_indices)>=dim(y)[2]){
        stop("Cannot disregard more columns than there are in the data frame")
    }
    
    #Get indices of y that are not duplicated within x 
    #(ignoring disregard_columns)
    #indices <- (!duplicated(rbind(x[ , -(x_dc_indices)], 
    #y[ , -(y_dc_indices)])) )[-(1:nrow(x))]
    
    x_colnames <- colnames(x)
    x_only_relevant <-as.data.frame(x[ , -(x_dc_indices)])
    colnames(x_only_relevant) <- x_colnames[-x_dc_indices]
    #print(colnames(x_only_relevant))
    
    y_colnames <- colnames(y)
    y_only_relevant <- as.data.frame(y[ , -(y_dc_indices)])
    colnames(y_only_relevant) <- y_colnames[-y_dc_indices]
    #print(colnames(y_only_relevant))
    
    #This was a bug!
    #indices <- (!duplicated(rbind(y_only_relevant, x_only_relevant)) )[
    #-(1:nrow(x))]
    
    indices <- (!duplicated(rbind(x_only_relevant, y_only_relevant)) )[
                                                            -seq(1, nrow(x))]
    
    #DOES NOT WORK IF ONLY A SINGLE COLUMN REMAINS
    #indices <- (!duplicated(rbind(x[ , -(x_dc_indices)], 
    #y[ , -(y_dc_indices)])) )[-(1:nrow(x))]
    
    #THIS WORKS FOR VECTORS ONLY
    #indices <- (!duplicated(append(x[ , -(x_dc_indices)], 
    #y[ , -(y_dc_indices)])) )[-(1:nrow(x))]
    #append(x[ , -(x_dc_indices)], y[ , -(y_dc_indices)])
    
    #THIS DOES NOT WORK (df naming issues) - but was almost there
    #indices <- (!duplicated(rbind(as.data.frame(x[ , -(x_dc_indices)]), 
    #as.data.frame(y[ , -(y_dc_indices)]))) )[-(1:nrow(x))]
    
    #print(x[,-(x_dc_indices)])
    #print(y[,-(y_dc_indices)])
    
    #print(indices)
    
    xy <- rbind(x, y[indices, ])
    rownames(xy) <- NULL
    mm("rbindUniqueCols completed", "fn")
    
    return(xy)
}
#----------------------------------------------------------------------------




#----------------------------------------------------------------------------
#From characteristics3.R
#Needed for universalExtractor()

#----------------------------------------------------------------------------
# Testing in progress (seems to leave out character NAs)

#df1 <- data.frame(a=1:3, b=4:6)
#df2 <- data.frame(x=c(6,5,4), y = c(1,2,3))

#df1b <- df1
#df1b[1,3] <- NA
#df1b[1,2] <- ""
#df1b[1,1] <- "NA"


#View(unifyNAs(df1b))

#class(df1b[1,1])
#class(df1b[1,2])
#class(df1b[1,3])

#is.na(df1b)

#----------------------------------------------------------------------------

#' Unify handling of NA values
#' 
#' @param x data frame
#' @return Data frame with "" and "NA" values converted to NA
#' 
unifyNAs <- function(x){
    mm("Running unifyNAs", "fn")
    
    if (length(x)==0){
        mm("unifyNAs completed", "fn")
        return(x)
    }
    
    #if (("data.frame" %in% class(x)) & dim(x)[1]==0){
    #    mm("unifyNAs completed", "fn")
    #    return(x)
    #}
    
    is.na(x) <- x == ""
    is.na(x) <- x == "NA"
    
    mm("unifyNAs completed", "fn")
    return(x)
}
#----------------------------------------------------------------------------



#----------------------------------------------------------------------------
#From characteristics3.R
#Newer version - with extra if statements to check if 
#the newly added extract doesn't already exist in the extract
#(i.e. to eliminate the cases when the same information 
#is added multiple times)
#
#Learning points: be careful about initialisation of lists
#In theory, could also do: rep(list(character()), length(key_words))
#instead of rep(list(""), length(key_words))
#However, this would also require a different course 
#of action for collapse procedures...
#
#Conditional loop problem solved
#
#Newest version - variable names changed
#Print statements deleted
#
#gsub statement now deletes anything before the key_word as well 
#(e.g. "chip antibody: " as opposed to "antibody: ", 
#which would leave chip with the extracted part)
#grep and gsub changed to case insensitive


#' Universal function for extracting from strings
#' 
#' @param characteristics Separable string (must be a string)
#' @param sep_split A character denoting splitting character
#' @param key_words A list of key words to be searched for (and removed); 
#' each of the k list levels contains synonyms for the same category
#' @return A vector with:
#' \itemize{
#'     \item characteristics (original string)
#'     \item char_extract[1] - extract of key_word[[1]]
#'     \item ...
#'     \item char_extract[k] - extract of key_word[[k]]
#' }
#' 
#' Extracts parts of a separable string which contains key_words 
#' (without the key_words) and the remainder after all the subtractions
#' 
#' 
#' @keywords internal
#' 
universalExtractor <- 
    function(characteristics, key_words, sep_split, sep_collapse){
    #print("Running universalExtractor")
    
    #Split the string
    char_split <- unlist(strsplit(characteristics, sep_split))
    
    #Initiate variables for storing results
    char_indices <- numeric()
    char_extract <- rep(list(character(0)), length(key_words))
    
    for (k in seq_along(key_words)){
        for (i in seq_along(key_words[[k]])){
            
            #Get indices of matches to current key_word
            char_curr_indices <- 
                grep(key_words[[k]][i], char_split, ignore.case = TRUE) 
            
            #Proceed with the next steps if there were matches
            if (length(char_curr_indices)!=0){ 
                
                #Get extract (without the key_word preceding it)
                char_curr_extract <- gsub(paste0("^.*", key_words[[k]][i]), "",
                                        char_split[char_curr_indices], 
                                        ignore.case = TRUE) 
                
                #Append indices to previous ones
                char_indices <- append(char_indices, char_curr_indices) 
                
                for (t in seq_along(char_curr_extract)){
                    
                    #Current extract is nonempty
                    if(char_curr_extract[t]!=""){ 
                        if (length(char_extract[[k]])==0) {
                            #Append current extract
                            char_extract[[k]] <- append(char_extract[[k]], 
                                                        char_curr_extract[t]) 
                        } else if (length(grep(char_curr_extract[t], 
                                                char_extract[[k]],
                                                #ignore.case = TRUE,
                                                fixed = TRUE))==0) {
                            #Append current extract
                            char_extract[[k]] <-
                                append(char_extract[[k]], 
                                        char_curr_extract[t]) 
                        }
                    }
                }
            }
        }
    }
    
    
    #Collapse the vectors within char_extract list
    for (kk in seq_along(char_extract)){
        #if (length(char_extract[[kk]]>1)){
        if (length(char_extract[[kk]]!=0)){
            #char_extract[[kk]] <- char_extract[[kk]][-1]
            char_extract[[kk]] <- paste(char_extract[[kk]], collapse = " -;- ")
        } else {
            char_extract[[kk]] <- NA
        }
    }
    
    char_extract <- unlist(char_extract)
    
    #Find unique ordered char_indices and use them to extract remainder
    char_indices <- unique(char_indices)
    char_indices <- char_indices[order(char_indices)]
    
    if (length(char_indices)!=0){
        #===*=== Consider an if statement assigning an empty string or NA...
        char_remainder <- char_split[-char_indices] 
    } else {
        char_remainder <- char_split
    }
    
    #Return to the original state (i.e. separated by sep_collapse)
    char_remainder <- paste(char_remainder, collapse = sep_collapse) 
    
    #output <- append(char_remainder, char_extract)
    output <- append(characteristics, char_remainder)
    output <- append(output, char_extract)
    
    
    output <- unifyNAs(output) #Replace "" and "NA" with NA
    
    #if (dim(output)[1]!=0){
    #    output <- unifyNAs(output) #Replace "" and "NA" with NA
    #}
    
    
    #This didn't work...
    #output <- as.data.frame(output, stringsAsFactors = FALSE)
    #colnames(output) <- c("characteristics", 
    #"char_remainder", "char_extract1", "char_extract2")
    
    #output <- as.data.frame(t(append(char_remainder, char_extract)), 
    #stringsAsFactors = FALSE)
    
    #print("universalExtractor completed")
    return(output)
}
#----------------------------------------------------------------------------



#----------------------------------------------------------------------------

#NEW VERSION OF INPUTDETECTOR()
#New features:
#- removed the necessary condition of "check" label within input column 
# (now samples can also be re-labelled as input)
#- only searches in the sa_antibody column if it is not empty
#- uses verifyConditions() for its operation (simplifies the code...)
#- uses verifyColumns() to check whether the specified columns 
#    exist within the df

#Developed in inputDetector5.R (also based on previous versions)





#' Detect inputs in a sample sheet
#' @param df Data frame to be checked (with a subset of sra columns)
#' 
#' @return Data frame with input column entries labelled as 'input' 
#' where appropriate
#' 
#' @section Logic of input detection:
#' 
#' The entries are labelled as inputs based on the following rules. 
#' They strictly need to fulfill NECESSARY conditions as well as:
#' \itemize{
#'     \item 1A: have matches to ANTIBODY conditions
#'     \item 1B: have matches to MATCH conditions, 
#'           provided no ANTIBODY information was provided
#'     \item 2: the whole SRP had no matches to ANTIBODY or MATCH conditions, 
#'           but there is a match to OTHERWISE conditions
#' }
#' 
#' 
#' Further details on the conditions:
#'  
#' 
#' \itemize{
#'     \item NECESSARY - these string matches are required 
#'     (entries not fulfilling all necessary conditions 
#'         will not be considered for labelling)
#'     \item ANTIBODY - rows with non-empty sa_antibody column 
#'         will only be labelled based on that field
#'     \item MATCH - one of these string matches are sufficient 
#'         to label an entry (provided it fulfils NECESSARY conditions 
#'         and that it has an EMPTY (NA) sa_antibody field)
#'     \item OTHERWISE - one of these string matches is sufficient 
#'         to label an entry, but ONLY if none of the SRP members 
#'         is labelled as input
#' }
#' 
#' 
#' @keywords internal
#' 
detectInputs <- function(df){
    mm("Running detectInputs", "fn")
    
    
    #==============================================================
    #--------------------------------------------------------------
    # SPECIFYING COLUMNS AND VALUES AS THE CRITERIA
    #--------------------------------------------------------------
    #==============================================================
    
    
    #-------------------------------
    #NECESSARY (all of the conditions are necessary)
    #-------------------------------
    necessary_names <- list() #Names to be searched for
    necessary_columns <- list() #Columns where search will be undertaken
    
    necessary_names[[1]] <- "ChIP-Seq"
    #necessary_names[[2]] <- "check" 
    #Currently the samples can be re-labelled as inputs as well
    
    necessary_columns[[1]] <- "library_strategy"
    #necessary_columns[[2]] <- "input"
    #-------------------------------
    
    
    #-------------------------------
    # ANTIBODY (antibody_indices: 
    # rows with one of the input synonyms within sa_antibody column)
    #-------------------------------
    antibody_names <- c("none", "no antibody", "input", "igg", "wce")
    antibody_columns <- "sa_antibody"
    #-------------------------------
    
    
    #-------------------------------
    #MATCH (match_indices: non-filled sa_antibody column, 
    # but at least one of the other columns contains input synonyms)
    #-------------------------------
    match_names <- list() #Names to be searched for
    match_columns <- list() #Columns where search will be undertaken
    
    match_names[[1]] <- "input"
    match_names[[2]] <- "igg"
    match_names[[3]] <- "wce"
    match_names[[4]] <- c("none", "no antibody", "input", "igg")
    
    # ===*===Check columns to be searched 
    # ('%input%' has already been thoroughly checked)
    match_columns[[1]] <- c("run_alias", 
                            "experiment_name", 
                            "experiment_alias", 
                            "experiment_title", 
                            "sample_name", 
                            "experiment_attribute", 
                            "sample_alias") 
    #Consider removing library_name... UPDATE 20170802: removed library_name
    #Removed sample_attribute
    
    #===*=== Check if there are not any more extracts to be made 
    # from sample_attribute that could be used for input detection
    
    match_columns[[2]] <- c("experiment_title", "experiment_alias")
    match_columns[[3]] <- c("experiment_title")
    match_columns[[4]] <- c("sa_antibody")
    #-------------------------------
    
    
    #-------------------------------
    # OTHERWISE conditions (if none of the SRP are labelled 
    # according to previous conditions)
    #-------------------------------
    otherwise_names <- list() #Names to be searched for
    otherwise_columns <- list() #Columns where search will be undertaken
    
    otherwise_names[[1]] <- "control"
    
    #===*===Check columns to be searched 
    # ('%control%' has already been thoroughly checked)
    otherwise_columns[[1]] <-  c("run_alias", 
                                "experiment_name", 
                                "run_attribute", 
                                "experiment_alias", 
                                "experiment_title", 
                                "sample_name", 
                                "experiment_attribute", 
                                "sample_alias", 
                                "sample_attribute") 
    #Thoroughly investigated fields (20170724)
    #-------------------------------
    
    
    #==============================================================
    #--------------------------------------------------------------
    # VERIFYING THE VALIDITY OF THE SETUP
    # (check if columns exist and if columns and names are the same length)
    #--------------------------------------------------------------
    #==============================================================
    
    
    #-------------------------------
    #Check whether lengths of inputs match
    var_columns <- c("necessary_columns", 
                    "antibody_columns", 
                    "match_columns", 
                    "otherwise_columns")
    
    var_names <- c("necessary_names", 
                    "antibody_names", 
                    "match_names", 
                    "otherwise_names")
    
    for (v in seq_along(var_columns)){
        if (methods::is(get(var_columns[v]), "list") | 
            methods::is(get(var_names[v]), "list")){
            if (length(get(var_columns[v])) != length(get(var_names[v]))){
                mm(paste0(
                        "The following columns and names differ in length: ", 
                        paste(get(var_columns[v]), collapse = ", "), 
                        " and ", 
                        paste(get(var_names[v]), collapse = ", ")), 
                    "adverse")
                
                warning("Columns and names differ in length")
            }
        }
    }
    
    #Check if specified columns exist within the data frame
    verifyColumns(df, necessary_columns)
    verifyColumns(df, antibody_columns)
    verifyColumns(df, match_columns)
    verifyColumns(df, otherwise_columns)
    
    #-------------------------------
    
    
    
    
    #==============================================================
    #--------------------------------------------------------------
    # FIND INDICES OF ROWS MATCHING THE CRITERIA
    # NOTE: these are not final row indices! 
    # They will be based on combinations of criteria
    #--------------------------------------------------------------
    #==============================================================
    
    
    #-------------------------------
    #NOTE: changed to AND operation 
    # (all necessary conditions need to be satisfied)
    necessary_indices <- rep(TRUE, nrow(df))
    for (n in seq_along(necessary_columns)){
        necessary_indices <- 
            necessary_indices & verifyConditions(df, 
                                                necessary_names[[n]], 
                                                necessary_columns[[n]])
    }
    #-------------------------------
    
    
    #-------------------------------
    antibody_indices <- verifyConditions(df, antibody_names, antibody_columns)
    #-------------------------------
    
    
    #-------------------------------
    match_indices <- rep(FALSE, nrow(df))
    for (m in seq_along(match_columns)){
        match_indices <- 
            match_indices | verifyConditions(df, 
                                            match_names[[m]], 
                                            match_columns[[m]])
    }
    #-------------------------------
    
    
    #-------------------------------
    otherwise_indices <- rep(FALSE, nrow(df))
    for (ot in seq_along(otherwise_columns)){
        otherwise_indices <- 
            otherwise_indices | verifyConditions(df, 
                                                otherwise_names[[ot]], 
                                                otherwise_columns[[ot]])
    }
    #-------------------------------
    
    
    
    #==============================================================
    #--------------------------------------------------------------
    # LABEL INPUTS
    # cond1a - fulfil necessary conditions and antibody conditions
    # cond1b - fulfil necessary conditions, don't have antibody information,
    #               but fulfil match conditions from other fields
    #--------------------------------------------------------------
    #==============================================================
    
    #-------------------------------
    #LABEL INPUTS
    #-------------------------------
    #====*=== Be careful!!! Specific column name used
    antibody_filled <- !is.na(df$sa_antibody) 
    
    cond1a <- antibody_filled & antibody_indices & necessary_indices
    cond1b <- (!antibody_filled) & match_indices & necessary_indices
    
    cond1 <- cond1a | cond1b
    
    df$input[cond1] <- "input"
    #-------------------------------
    
    
    
    
    #==============================================================
    #--------------------------------------------------------------
    # CHECK FOR INPUT LABELS WITHIN SRPs
    #  i.e. for each sample get a boolean value corresponding
    #        to presence of labelled inputs within the same SRP
    #--------------------------------------------------------------
    #==============================================================
    
    
    #-------------------------------
    #Find the indices of SRP members where 
    #no successful match occurred in the entire SRP
    #(these will be taken into account for 'otherwise' conditions)
    #-------------------------------
    #Code from the previous version of the inputDetector function 
    #- see older versions for alternative approaches using only two columns 
    #or for sorting not from last (requires the use of xtfrm)
    detected <- data.frame(df$run_accession, 
                            df$study_accession, 
                            cond1, 
                            stringsAsFactors = FALSE)
    
    colnames(detected) <- c("run_accession", "study_accession", "cond1")
    
    #Order by SRP and boolean value (input or not)
    detected <- detected[order(detected$study_accession, detected$cond1),] 
    
    #Remove duplicates
    detected <- detected[!duplicated(detected$study_accession, 
                                        fromLast = TRUE), ] 
    
    #Merge to obtain one boolean value per SRP
    detected <- merge(df[,c("run_accession", "study_accession")], 
                        detected[,c("study_accession", "cond1")]) 
    #-------------------------------
    
    
    
    #==============================================================
    #--------------------------------------------------------------
    # LABEL INPUTS
    # cond2 - fulfil necessary conditions,
    #             none of the samples within SRP were labelled (by cond1a & b)
    #             fulfil otherwise conditions
    #--------------------------------------------------------------
    #==============================================================
    
    
    #-------------------------------
    #Label entries with no match within SRP (!detected$cond1) 
    #and which satisfy 'otherwise' and necessary conditions
    #-------------------------------
    cond2 <- necessary_indices & (!detected$cond1) & otherwise_indices
    df$input[cond2] <- "input"
    #-------------------------------
    mm("detectInputs completed", "fn")
    
    return(df)
    
}



#================================================

#' Verify columns within a data frame
#' 
#' @param data frame (with named columns)
#' @param column_list A character vector with column names to be checked
#' 
#' @return Nothing but a printed message on the matching 
#' between colnames(df) and column_list
#' 
#' NOTE: Current version requires perfect matching (identical strings). 
#' Methods which acutally check the indices might be more flexible 
#' (e.g. case insensitive).
#' 
#' 
#' @keywords internal
#' 
verifyColumns <- function(df, column_list){
    
    mm("Runing verifyColumns", "fn")
    
    # Check that df is a data frame
    if (!"data.frame" %in% class(df)){
        stop("Argument is not a data frame")
    }
    
    
    df_cols <- colnames(df)
    if (methods::is(column_list, "list")){
        column_list <- unlist(column_list)
    }
    
    if (length(setdiff(column_list, df_cols)) != 0){
        warning("Not all specified columns can be found in the data frame")
        mm(paste0("The following columns are missing from the data frame: ", 
                    paste(setdiff(column_list, df_cols), collapse = ",") ), 
            "adverse")
    } else {
        mm("All specified columns are within the data frame", "adverse")
        mm(paste(column_list, collapse = ", "), "adverse")
    }
    mm("verifyColumns completed", "fn")
}

#----------------------------------------------------------------------------




#----------------------------------------------------------------------------
#Developed in detectControls.R (based on inputDetector.R)
#mostly inputDetector4.R (?)


#' Detect controls in a sample sheet
#' @param df Data frame to be checked (with a subset of sra columns)
#' 
#' @return Data frame with control column entries labelled as 'control' 
#' where appropriate
#' 
#' @section Logic of control detection:
#' \itemize{
#'     \item NECESSARY - these string matches are required 
#'     (entries not fulfilling all necessary conditions 
#'         will not be considered for labelling)
#'     \item ANTIBODY - rows with non-empty sa_antibody column 
#'         will only be labelled based on that field
#'     \item MATCH - one of these string matches are sufficient 
#'         to label an entry (provided it fulfils NECESSARY conditions 
#'         and that it has an EMPTY (NA) sa_antibody field)
#'     \item OTHERWISE - one of these string matches is sufficient 
#'         to label an entry, but ONLY if none of the SRP members 
#'         is labelled as input
#' }
#' 
#' @keywords internal
#' 
detectControls <- function(df){
    
    mm("Running detectControls", "fn")
    
    
    #---------------------------------------------------------------
    # Variables for necessary conditions (all need to be fulfilled)
    #---------------------------------------------------------------
    necessary_names <- list() #Names to be searched for
    necessary_columns <- list() #Columns where search will be undertaken
    necessary_col_ind <- list() #Indices of columns
    
    necessary_names[[1]] <- "RNA-Seq"
    
    necessary_columns[[1]] <- "library_strategy"
    #---------------------------------------------------------------
    
    
    
    #---------------------------------------------------------------
    # Variables for match conditions 
    # (at least one of the conditions needs to be fulfilled)
    #---------------------------------------------------------------
    match_names <- list() #Names to be searched for
    match_columns <- list() #Columns where search will be undertaken
    match_col_ind <- list() #Indices of columns
    
    match_names[[1]] <- "control"
    
    #===*===Check columns to be searched
    #match_columns[[1]] <- c("run_alias", "experiment_name", 
    # "experiment_alias", "experiment_title", "sample_name", 
    # "experiment_attribute", "sample_alias", "sample_attribute") 
    #===*=== Quite an arbitrary list taken from input's case
    
    match_columns[[1]] <-  c("run_alias", 
                            "experiment_name", 
                            "run_attribute", 
                            "experiment_alias", 
                            "experiment_title", 
                            "sample_name", 
                            "experiment_attribute", 
                            "sample_alias", 
                            "sample_attribute") 
    #Thoroughly investigated fields (20170724), 
    # but in relation to ChIP-Seq experiments
    
    #---------------------------------------------------------------
    
    
    #---------------------------------------------------------------
    # Variables for 'otherwise' conditions 
    # (if none of the SRP are labelled according to previous conditions)
    #---------------------------------------------------------------
    otherwise_names <- list() #Names to be searched for
    otherwise_columns <- list() #Columns where search will be undertaken
    otherwise_col_ind <- list() #Indices of columns
    
    #---------------------------------------------------------------
    
    
    #---------------------------------------------------------------
    #Get indices of columns
    #Check if columns exist in the original data frame
    #Order column indices
    
    for (i in seq_along(necessary_columns)){
        
        necessary_col_ind[[i]] <- 
            unique(unlist(lapply(necessary_columns[[i]], 
                                function(x) grep(x, colnames(df)))))
        
        necessary_col_ind[[i]] <- 
            necessary_col_ind[[i]][order(necessary_col_ind[[i]])]
        
        if (length(necessary_col_ind[[i]])!=length(necessary_columns[[i]])){
            warning(paste0(
                "Columns specified for detecting '", 
                paste(necessary_names[[i]], collapse=", "), 
                "' do not match the df"))
        }
    }
    
    
    for (i in seq_along(match_columns)){
        match_col_ind[[i]] <- 
            unique(unlist(lapply(match_columns[[i]], 
                                function(x) grep(x, colnames(df)))))
        
        match_col_ind[[i]] <- match_col_ind[[i]][order(match_col_ind[[i]])]
        if (length(match_col_ind[[i]])!=length(match_columns[[i]])){
            warning(paste0("Columns specified for detecting '", 
                            paste(match_names[[i]], collapse=", "), 
                            "' do not match the df"))
        }
    }
    
    
    for (i in seq_along(otherwise_columns)){
        otherwise_col_ind[[i]] <- 
            unique(unlist(lapply(otherwise_columns[[i]], 
                                function(x) grep(x, colnames(df)))))
        
        otherwise_col_ind[[i]] <- 
            otherwise_col_ind[[i]][order(otherwise_col_ind[[i]])]
        
        if (length(otherwise_col_ind[[i]])!=length(otherwise_columns[[i]])){
            warning(paste0("Columns specified for detecting '", 
                            paste(otherwise_names[[i]], collapse=", "), 
                            "' do not match the df"))
        }
    }
    
    #---------------------------------------------------------------
    
    
    #---------------------------------------------------------------
    #Initialise a list to store vectors with row indices
    necessary_tot <- rep(list(rep(FALSE, nrow(df))), length(necessary_names))
    match_tot <- rep(list(rep(FALSE, nrow(df))), length(match_names))
    otherwise_tot <- rep(list(rep(FALSE, nrow(df))), length(otherwise_names))
    #---------------------------------------------------------------
    
    
    #---------------------------------------------------------------
    #Search for matches
    #---------------------------------------------------------------
    #i = 1 #i'th key_word
    #c = 1 #c'th column
    #s = 1 #s'th synonym
    #temp <- grepl(necessary_names[[i]][s], df[,necessary_col_ind[[i]][c]])
    
    for (i in seq_along(necessary_names)){ #For every key_word
        for (s in seq_along(necessary_names[[i]])){ #For every name synonym
            for (c in seq_along(necessary_col_ind[[i]])){ #For every column
                temp <- grepl(necessary_names[[i]][s], 
                                df[,necessary_col_ind[[i]][c]], 
                                ignore.case = TRUE)
                necessary_tot[[i]]<- necessary_tot[[i]] | temp
            }
        }
    }
    
    #For every key_word
    for (i in seq_along(match_names)){ 
        #For every name synonym
        for (s in seq_along(match_names[[i]])){ 
            #For every column
            for (c in seq_along(match_col_ind[[i]])){ 
                temp <- grepl(match_names[[i]][s], 
                                df[,match_col_ind[[i]][c]], 
                                ignore.case = TRUE)
                match_tot[[i]]<- match_tot[[i]] | temp
            }
        }
    }
    
    
    #otherwise_tot will be used later
    
    #For every key_word
    for (i in seq_along(otherwise_names)){ 
        #For every name synonym
        for (s in seq_along(otherwise_names[[i]])){ 
            #For every column
            for (c in seq_along(otherwise_col_ind[[i]])){ 
                temp <- grepl(otherwise_names[[i]][s], 
                                df[,otherwise_col_ind[[i]][c]], 
                                ignore.case = TRUE)
                otherwise_tot[[i]]<- otherwise_tot[[i]] | temp
            }
        }
    }
    
    #---------------------------------------------------------------
    
    
    #---------------------------------------------------------------
    #Combine necessary and match vectors (necessary & match)
    #Label relevant rows with 'input'
    #---------------------------------------------------------------
    
    #Create necessary_combined vector (all necessary conditions fulfilled)
    #TRUE needed because will apply '&'
    necessary_combined <- rep(TRUE, nrow(df)) 
    for (i in seq_along(necessary_tot)){
        necessary_combined <- necessary_tot[[i]] & necessary_combined
    }
    
    #Create match_combined vector 
    # (at least one of the match conditions fulfilled)
    #FALSE needed because will apply '|'
    match_combined <- rep(FALSE, nrow(df)) 
    for (i in seq_along(match_tot)){
        match_combined <- match_tot[[i]] | match_combined
    }
    
    #Create otherwise_combined vector 
    # (at least one of the otherwise conditions fulfilled)
    #Will be used later
    #FALSE needed because will apply '|'
    otherwise_combined <- rep(FALSE, nrow(df)) 
    for (i in seq_along(otherwise_tot)){
        otherwise_combined <- otherwise_tot[[i]] | otherwise_combined
    }
    #---------------------------------------------------------------
    
    necessary_match_combined <- necessary_combined & match_combined
    
    df$control[necessary_match_combined] <- "control"
    #---------------------------------------------------------------
    
    
    
    
    #---------------------------------------------------------------
    # Find the indices of SRP members 
    # where no successful match occurred in the entire SRP
    # (these will be taken into account for 'otherwise' conditions)
    #---------------------------------------------------------------
    #Code from the previous version of the inputDetector function 
    # - see older versions for alternative approaches using only two columns 
    # or for sorting not from last (requires use of xtfrm)
    detected <- data.frame(df$run_accession, 
                            df$study_accession, 
                            necessary_match_combined, 
                            stringsAsFactors = FALSE)
    
    colnames(detected) <- c("run_accession", 
                            "study_accession", 
                            "necessary_match_combined")
    
    #Order by SRP and boolean value
    detected <- detected[order(detected$study_accession, 
                                detected$necessary_match_combined),] 
    
    #Remove duplicates
    detected <- detected[!duplicated(detected$study_accession, 
                                        fromLast = TRUE),] 
    
    #Merge to obtain one boolean value per SRP
    detected <- merge(df[,c("run_accession", 
                                "study_accession")], 
                    detected[,c("study_accession", 
                                "necessary_match_combined")]) 
    #---------------------------------------------------------------
    
    
    #---------------------------------------------------------------
    #Label entries with no match (undetected) 
    # and which satisfy 'otherwise' conditions
    #---------------------------------------------------------------
    otherwise_undetected <- (!(detected$necessary_match_combined) & 
                                    necessary_combined & 
                                    otherwise_combined)
    
    df$control[otherwise_undetected] <- "otherwise"
    #---------------------------------------------------------------
    
    
    #------------------------------------------------------------------------
    
    mm("detectControls completed", "fn")
    return(df)
}
#----------------------------------------------------------------------------




#----------------------------------------------------------------------------
#' @importFrom dplyr %>%
dplyr::`%>%`

#----------------------------------------------------------------------------


#----------------------------------------------------------------------------
#' @importFrom crayon %+%
crayon::`%+%`

#----------------------------------------------------------------------------




#----------------------------------------------------------------------------
#Developed in mergeDetector.R

#' Detect merges in a sample sheet
#' 
#' @param df Data frame (must have experiment_accession column; 
#' also, number of rows in df must correspond to number of SRRs)
#' @param do_nothing Logical indicating whether, 
#' after creating relevant columns, merge detection should be performed 
#' (if FALSE, fills these columns with NAs)
#' 
#' @return Original data frame with added columns: \itemize{
#'     \item n - count of SRRs within that SRx
#'     \item lane - index of runs within a lane (1:n for each SRX)
#'     \item mer - indication on how to merge runs \itemize{
#'         \item "" - no merging required
#'         \item SRX... - merge runs with corresponding SRXs
#'     }
#' }
#' 
#' @keywords internal
#' 
detectMerges <- function(df, do_nothing = FALSE){
    
    mm("Running detectMerges", "fn")
    
    dm_columns <- c("n", "lane", "mer")
    
    if (do_nothing == TRUE){
        
        df <- createEmptyColumns(df, dm_columns)
        
        #if (dim(df)[1]==0){
        #    df$n <- character(0)
        #    df$lane <- character(0)
        #    df$mer <- character(0) #===*=== added later
        #} else {
        #    df$n <- NA
        #    df$lane <- NA
        #    df$mer <- NA #===*=== added later
        #}
        mm("detectMerges completed", "fn")
        return(df)
    }
    
    if (sum(!is.na(df$experiment_accession))==0){
        
        df <- createEmptyColumns(df, dm_columns)
        #if (dim(df)[1]==0){
        #    df$n <- character(0)
        #    df$lane <- character(0)
        #    df$mer <- character(0) #===*=== added later
        #} else {
        #    df$n <- NA
        #    df$lane <- NA
        #    df$mer <- NA #===*=== added later
        #}
        
        warning("No not-NA experiment_accesion elements")
        mm("detectMerges completed", "fn")
        return(df)
    }
    
    n <- NULL
    experiment_accession <- NULL
    
    df <- df %>%
        dplyr::add_count(experiment_accession) %>% #Count SRRx within SRX
        dplyr::group_by(experiment_accession) %>%
        dplyr::mutate(lane = seq_along(n)) %>% #Indexes all SRRs within SRX
        dplyr::mutate(mer = dplyr::case_when(n >= 2 ~ experiment_accession, #1)
                                            n < 2 ~ "")) #2)
    #1) # Label with SRX when multiple SRRs exist in SRX
    #2) # Leave empty if only one SRR in SRX
    
    df <- as.data.frame(df)
    mm("detectMerges completed", "fn")
    return(df)
}
#----------------------------------------------------------------------------



#----------------------------------------------------------------------------
#Developed in missingRunVerifier.R
#NEW VERSION (using parQuery() function)

#' Verify missing Runs
#' 
#' @param srr_list_in A character vector with SRRs
#' @return Nothing but a printed notice on whether the SRXs 
#' to which the SRRs belong also have any other SRRs
#' 
#' @keywords internal
#' 
verifyMissingRuns <- function(srr_list_in){
    # ===*=== Double check if all the entries are identical...
    mm("Running verifyMissingRuns", "fn")
    
    database_name <- "sra_con"
    database_env <- ".GlobalEnv"
    
    mm("CHECKING FOR MISSING RUNS", "fn")
    
    srr_list_in <- unique(srr_list_in[order(srr_list_in)])
    
    miss_exp <- parQuery(get(database_name, envir = get(database_env)), 
                        paste0("SELECT experiment_accession, run_accession ",
                            "FROM sra WHERE run_accession = ?"), 
                        srr_list_in)
    
    srx_list <- unique(miss_exp$experiment_accession)
    
    miss_run <- parQuery(get(database_name, envir = get(database_env)), 
                        paste0("SELECT experiment_accession, run_accession ",
                            "FROM sra WHERE experiment_accession = ?"), 
                        srx_list)
    
    srr_list_out <- miss_run$run_accession
    srr_list_out <- unique(srr_list_out[order(srr_list_out)])
    
    if (!setequal(srr_list_in, srr_list_out)) {
        #if (length(intersect(srr_list_out, 
        # srr_list_in)) != length(srr_list_out)) {
        
        missing <- paste(setdiff(srr_list_out, srr_list_in), collapse = ", ")
        mm("The list does not include all the runs", "adverse")
        mm(paste0("Missing runs: ", missing), "adverse")
        
        #warning("The list does not include all the runs") 
        #Warning does not work for some reason
        
    } else { #===*=== Maybe another criterion...?
        mm(paste0("There are no missing runs within ",
                        "the selected experiment accessions"), 
            "adverse")
    }
    mm("verifyMissingRuns completed", "fn")
    
}

#================================================

#Needed for verifyMissingRuns()

#' Parametrised query for verifyMissingRuns()
#' 
#' @param db_con,query,par_list Character vectors
#' @return Data frame with results
#' 
#' @keywords internal
#' 
parQuery <- function(db_con, query, par_list){
    mm("Running parQuery", "fn")
    res <- DBI::dbSendQuery(db_con, query)
    DBI::dbBind(res, param = list(par_list))
    df <- DBI::dbFetch(res)
    DBI::dbClearResult(res)
    mm("parQuery completed", "fn")
    return(df)
}
#----------------------------------------------------------------------------




#----------------------------------------------------------------------------
#Developed in convertPairedEnds.R


#' Convert the entries to give information about paired ends
#' 
#' @param df Data frame to be processed
#' @return Original data frame with a new column with a logical 
#' indicating whether the run has paired end layout
#' 
#' @keywords internal
#' 
convertPairedEnds <- function(df){
    mm("Running convertPairedEnds", "fn")
    
    # Rename col
    rename_col <- FALSE
    if (sum(grepl("SRA_library_layout", colnames(df)))==1){
        rename_col <- TRUE
        colnames(df)[grepl("SRA_library_layout", colnames(df))] <- 
                                                            "library_layout"
    }
    
    
    
    verifyColumns(df, "library_layout")
    
    
    # Empty data frame
    if (dim(df)[1]==0){
        df <- createEmptyColumns(df, "pairedEnd")
        
        if (rename_col){
            colnames(df)[grepl("library_layout", colnames(df))] <- 
                                                        "SRA_library_layout"
        }
        return(df)
    }
    
    #Locate the library_layout column
    column_index <- grep("^library_layout$", colnames(df))
    #paired_indices <- rep(FALSE, nrow(df))
    
    #Get indices of paired layout (assumes anything non-paired is single)
    #paired_indices <- grepl("PAIRED", df[, column_index]) #This didn't work!
    paired_indices <- grepl("PAIRED", df$library_layout)
    #unpaired_indices <- grepl("SINGLE", df$library_layout)
    
    mm(paste0("Found ", sum(paired_indices), " runs with paired ends"), "res")
    #print(sum(paired_indices))
    #print(sum(unpaired_indices))
    
    df$pairedEnd <- NA
    df$pairedEnd[paired_indices] <- "true"
    df$pairedEnd[!paired_indices] <- "false"
    
    
    if (rename_col){
        colnames(df)[grepl("library_layout", colnames(df))] <- 
                                                        "SRA_library_layout"
    }
    
    mm("convertPairedEnds completed", "fn")
    return(df)
}
#----------------------------------------------------------------------------






#----------------------------------------------------------------------------
#Developed in superseriesVerifier.R

#' Verify presence of superseries
#' 
#' @param gse_list List of GSEs (as they appear in the series_id column, 
#' i.e. comma-separated if multiple)
#' 
#' @return A character vector with GSEs that existed 
#' in conjuction with other GSEs 
#' (i.e. suggesting some of them might be superseries); 
#' NULL if no potential superseries.
#' Also gives console information on the number of samples with mutliple GSEs 
#' and the first few GSEs which co-occur with other GSEs 
#' (i.e. displays beginning of the function output)
#' 
#' @keywords internal
#' 
verifySuperseries <- function(gse_list){
    mm("Running verifySuperseries", "fn")
    
    mm("CHECKING FOR PRESENCE OF SUPERSERIES", "fn")
    
    #Grepl GSE..., GSE... (...)
    ss_match <- grepl("^GSE\\d\\d\\d+,GSE\\d\\d\\d+.*$", gse_list)
    mm(paste0(sum(ss_match), 
                " out of ", 
                length(gse_list), 
                " entries belong to more than one GSE ",
                "(some of them might be superseries)"), 
        "res")
    
    if (sum(ss_match)>0){
        #Get a list of GSEs that co-occur with other GSEs 
        # (i.e. GSEs from samples which have more than one GSEs)
        ss_list <- unlist(strsplit(unique(gse_list[ss_match]), split=","))
        ss_list <- unique(ss_list[order(ss_list)])
        if (length(ss_list)!=0){
            mm(paste0("Consider carrying out superseries ",
                    "search on the following GSEs: ", 
                    paste(ss_list, collapse = ", ")), 
            "adverse")
        }
        return(ss_list)
        
    } else {
        return(NULL)
    }
    
    mm("verifySuperseries completed", "fn")
}
#----------------------------------------------------------------------------




#----------------------------------------------------------------------------
# manageLibraryStrategy
#----------------------------------------------------------------------------


#'
#' Manage Library Strategy Formats
#' 
#' \code{manageLibraryStrategy} by default converts between different formats 
#' of library strategy strings and offers a few other tasks 
#' related to library strategies
#' 
#' @param x Character vector to be converted
#' @param input String denoting the input format (see below)
#' @param output String denoting the output format (see below)
#' @param task String denoting the task to be performed (see below)
#' @param mismatch.ignore Logical denoting whether mismatches are allowed 
#' (if TRUE and no match, original character is returned)
#' 
#' @return Library strategy in a desired format
#' 
#' Format types:
#' \enumerate{
#'     \item can (canonical) - as exists within the database
#'     \item short (shorthand) - shortened version of the canonical form
#'     \item syn (synonyms) - potential synonym
#' }
#' 
#' 
#' Available tasks:
#' \enumerate{
#'     \item conv - convert between formats
#'     \item ex - produce a list of library strategies
#'     \item check_can - check if x is in canonical form
#' }
#' 
#' 
#' Currently, the function supports the following conversions:
#' \enumerate{
#'     \item can -> short, i.e. input = "can", ouput = "short"
#'     \item syn -> can, i.e. input = "syn", output = "can"
#' }
#' 
#' 
#' 
#' @examples
#' # Convert into short form
#' # manageLibraryStrategy("RNA-Seq", input = "can", output = "short") 
#' 
#' # Convert into canonical form
#' # manageLibraryStrategy("RNA", input = "syn", output = "can") 
#' 
#' # List supported formats for library strategy
#' # manageLibraryStrategy(task = "ex") 
#' 
#' # Check whether library strategy is in canonical form
#' # manageLibraryStrategy("RNA-Seq", task = "check_can") 
#' 
#' 
#' 
#' 
#' @keywords internal
#' 
#' 
manageLibraryStrategy <- function(
                                    x, 
                                    input, 
                                    output, 
                                    task="conv", 
                                    mismatch.ignore = FALSE){
    
    mm("Running manageLibraryStrategy", "fn")
    
    
    
    # Canonical names (as exist within the SRA database)
    can <- c("WGS", #1
                "AMPLICON", #2
                "RNA-Seq", #3
                "OTHER", #4
                "WXS", #5
                "ChIP-Seq", #6
                "CLONE", #7
                "POOLCLONE", #8
                "Bisulfite-Seq", #9
                "SELEX", #10
                "miRNA-Seq", #11
                "WGA", #12
                "RAD-Seq", #13
                "Targeted-Capture", #14
                "ATAC-seq") #15
    
    
    #Shorthand forms
    short <- c("WGS", #1
                "AMPLI", #2
                "RNA", #3
                "OTHER", #4
                "WXS", #5
                "ChIP", #6
                "CLONE", #7
                "POOLCL", #8
                "Bisulf", #9
                "SELEX", #10
                "miRNA", #11
                "WGA", #12
                "RAD", #13
                "Tar-Cap", #14
                "ATAC") #15
    
    
    #Synonym forms
    syn <- list(c("WGS"), #1
                c("AMPLICON"), #2
                c("RNA-Seq"), #3
                c("OTHER"), #4
                c("WXS"), #5
                c("ChIP-Seq"), #6
                c("CLONE"), #7
                c("POOLCLONE"), #8
                c("Bisulfite-Seq"), #9
                c("SELEX"), #10
                c("miRNA-Seq"), #11
                c("WGA"), #12
                c("RAD-Seq"), #13
                c("Targeted-Capture"), #14
                c("ATAC-seq")) #15
    
    
    #Make sure that the list of synonyms 
    #also contains short and canonical forms
    for (i in seq_along(syn)){
        syn[[i]] <- c(can[[i]], syn[[i]], short[[i]])
        syn[[i]] <- unique(syn[[i]])
    }
    
    
    #Sanity check that all formats have the same length
    if ( (length(can)!=length(short)) | 
                                        (length(can)!=length(syn)) | 
                                        (length(short)!=length(syn)) ){
        stop("The format lists have unequal lengths")
    }
    
    
    #Non-converting tasks
    if (task == "ex"){ #Special track for task == "ex"
        
        y <- list(Canonical_Forms = can,
                    Short_Forms = short, 
                    Currently_Accepted_Synonyms = syn)
        mm("manageLibraryStrategy completed", "fn")
        return(y)
        
    } else if (task == "check_can"){ #Special track for task == "check_can"
        y <- x %in% can
        mm("manageLibraryStrategy completed", "fn")
        return(y)
    }
    
    
    
    
    #Only two combinations are allowed (syn->can, can->short); 
    # (or output = "ex" which was done earlier)
    if ( !( (input == "can" & output == "short") | 
                                        (input == "syn" & output == "can") ) ){
        stop("Invalid input-output combination provided")
    }
    
    
    
    #CONVERSION: can->short
    if (input == "can" & output == "short"){
        
        mm("CONVERSION: can -> short", "query")
        
        ind <- grep(paste0("^", x, "$"), can)
        
        if (mismatch.ignore==TRUE){ #Mismatch allowed
            if (length(ind) !=1){ #If none/too many matches were found
                y <- x
            } else { #Only one match found
                y <- short[ind]
            }
            
        } else { #Mismatch not allowed
            if (length(ind) != 1) {
                stop("Unexpected number of matches")
            }
            y <- short[ind]
        }
        
        
    }
    
    
    
    
    #CONVERSION: syn->can
    if (input == "syn" & output == "can"){
        
        mm("CONVERSION: syn -> can", "query")
        
        ind <- rep(list(integer(0)),15)
        res_num <- 0
        ind_fin <- NULL
        
        for (j in seq_along(syn)){
            ind[[j]] <- grep(paste0("^", x, "$"), syn[[j]], ignore.case = TRUE)
            #print(ind)
            if (length(ind[[j]])>0){
                ind_fin <- c(ind_fin, j)
            }
            #print(ind_fin)
            res_num <- (length(ind[[j]])>0) + res_num
            #print(res_num)
        }
        
        if (mismatch.ignore == TRUE){ #Mismatch allowed
            if (res_num == 1){
                y <- can[ind_fin]
            } else {
                y <- x
            }
        } else { #Mismatch not allowed
            if (res_num >1){
                
                stop(paste0("Multiple matches were found. ",
                        "Please make your search term unique"))
                
            } else if (res_num == 0){
                stop("No results found")
            } else if (res_num == 1){
                y <- can[ind_fin]
            }
        }
        
    }
    
    
    
    
    mm("manageLibraryStrategy completed", "fn")
    
    return(y)
    
    
}

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------




#----------------------------------------------------------------------------
# renameGSMColumns
#----------------------------------------------------------------------------


#' Rename df columns derived from gsm table to 'GSM_'
#' 
#' @param df Data frame
#' @return Data frame with modified column names
#' 
#' @keywords internal
#' 
renameGSMColumns <- function(df){
    
    database_name <- "geo_con"
    database_env <- ".GlobalEnv"
    
    if (!is.data.frame(df)){
        stop("df is not a data frame")
    }
    
    gsm_columns <- DBI::dbListFields(get(database_name, 
                                            envir = get(database_env)), 
                                    "gsm")
    
    # Exclude gsm and series_id
    gsm_columns <- gsm_columns[!gsm_columns %in% c("gsm", "series_id")] 
    
    
    gsm_id <- (colnames(df) %in% gsm_columns)
    
    
    colnames(df)[gsm_id] <- paste0("GSM_", colnames(df)[gsm_id])
    
    return(df)
}
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------





#----------------------------------------------------------------------------
# renameGSEColumns
#----------------------------------------------------------------------------


#' 
#' Rename df columns derived from gse table to 'GSE_'
#' 
#' @param df Data frame
#' @return Data frame with modified column names
#' 
#' @keywords internal
#' 
renameGSEColumns <- function(df){
    
    database_name <- "geo_con"
    database_env <- ".GlobalEnv"
    
    if (!is.data.frame(df)){
        stop("df is not a data frame")
    }
    
    gse_columns <- DBI::dbListFields(
                                get(database_name, envir = get(database_env)), 
                                "gse")
    
    # Exclude gsm and series_id
    gse_columns <- gse_columns[!gse_columns %in% c("gsm", "series_id", "gse")] 
    
    gse_id <- (colnames(df) %in% gse_columns)
    
    
    colnames(df)[gse_id] <- paste0("GSE_", colnames(df)[gse_id])
    
    return(df)
}
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------




#----------------------------------------------------------------------------
# renameSRAColumns
#----------------------------------------------------------------------------
#'
#' Rename df columns derived from sra table to 'SRA_'
#' 
#' @param df Data frame
#' @return Data frame with modified column names
#' 
#' @keywords internal
#' 
renameSRAColumns <- function(df){
    
    database_name <- "sra_con"
    database_env <- ".GlobalEnv"
    
    if (!is.data.frame(df)){
        stop("df is not a data frame")
    }
    
    sra_columns <- DBI::dbListFields(
                                get(database_name, envir = get(database_env)), 
                                "sra")
    
    # Exclude accession names
    sra_columns <- sra_columns[!sra_columns %in% c("run_accession", 
                                                    "experiment_accession", 
                                                    "sample_accession", 
                                                    "study_accession", 
                                                    "submission_accession")] 
    
    
    sra_id <- (colnames(df) %in% sra_columns)
    
    
    colnames(df)[sra_id] <- paste0("SRA_", colnames(df)[sra_id])
    
    return(df)
    
}
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------


#----------------------------------------------------------------------------
# renameOTHColumns
#----------------------------------------------------------------------------
#'
#' Rename non-SRA/GEO columns to 'OTH_'
#' 
#' @param df Data frame
#' @return Data frame with modified column names
#' 
#' 
#' @keywords internal
#' 
renameOTHColumns <- function(df){
    
    if (!is.data.frame(df)){
        stop("df is not a data frame")
    }
    
    oth_columns <- c("input", 
                        "control", 
                        "sa_tissue", 
                        "sa_antibody", 
                        "sa_gene", 
                        "sa_treatment", 
                        "sa_remainder", 
                        "ch1_tissue", 
                        "ch1_antibody", 
                        "ch1_gene", 
                        "ch1_treatment", 
                        "ch1_remainder",
                        "lane", 
                        "mer", 
                        "pairedEnd", 
                        "n")
    
    oth_id <- (colnames(df) %in% oth_columns)
    
    
    colnames(df)[oth_id] <- paste0("OTH_", colnames(df)[oth_id])
    
    return(df)
    
}
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------




#----------------------------------------------------------------------------
# generateEmptyDF
#----------------------------------------------------------------------------
#' Generate empty df with columns corresponding to database columns
#' 
#' @param tables Character vector with tables from which the columns
#' @return Data frame with columns corresponding to database columns 
#' (with names prepended with appropriate prefix). 
#' Format corresponds to that of searchForAccessionAcrossDBsDF
#' 
#' 
#' @keywords internal
#' 
generateEmptyDF <- function(tables = c("sra", "gsm", "gse", "other")){
    df_columns <- character()
    
    if ("sra" %in% tables){
        df_columns <- 
            c(df_columns, as.character(unlist(listValidColumns()$SRA)))
    }
    
    if ("gsm" %in% tables){
        df_columns <- 
            c(df_columns, as.character(unlist(listValidColumns()$GSM)))
    }
    
    if ("gse" %in% tables){
        df_columns <- 
            c(df_columns, as.character(unlist(listValidColumns()$GSE)))
    }
    
    if ("other" %in% tables){
        df_columns <- 
            c(df_columns, as.character(unlist(listValidColumns()$Other)))
    }
    
    df <- stats::setNames(data.frame(matrix(ncol = length(df_columns), 
                                        nrow = 0)), 
                        df_columns)
    return(df)
}
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------



#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
createEmptyColumns <- function(df, x){
    if (dim(df)[1]==0){
        for (i in seq_along(x)){
            if (!(x[i] %in% colnames(df))) {
                df[, x[i]] <- character(0)
            }
        }
        
    } else {
        x <- x[!(x %in% colnames(df))]
        df[ , x] <- as.character(NA)
        
    }
    return(df)
}
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------

