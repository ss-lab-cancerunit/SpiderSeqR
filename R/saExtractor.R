

#' Extract sample attributes
#' 
#' @param df Data frame with (SRA_)sample_attribute column
#' @return Data frame with extracted information 
#'     from the sample attribute column and a few added new columns, 
#'     currently: sa_remainder, sa_tissue, sa_antibody, sa_gene, sa_treatment
#' 
#' @description This function is a wrapper around universalExtractor 
#' with key words specific for (SRA_)sample_attribute field
#' 
#' @keywords internal
#' 
saExtractor <- function(df){
    #PREVIOUSLY
    #sra_attr_keywords <- list(c("tissue: ", "cell.line: ", 
    #                               "source.name: ", "cell.type: "),
    #                          c("antibody: "),
    #                          c("hgn: "),
    #                          c("treatment: "))
    
    
    mm("Running saExtractor", "fn")
    
    # Rename SRA_sample_attribute
    rename_col <- FALSE
    if (sum(grepl("SRA_sample_attribute", colnames(df)))==1){
        rename_col <- TRUE
        colnames(df)[grepl("SRA_sample_attribute", colnames(df))] <- 
                                                            "sample_attribute"
    }
    
    verifyColumns(df, "sample_attribute")
    
    sa_columns <-c("sa_original", 
                    "sa_remainder", 
                    "sa_tissue", 
                    "sa_antibody", 
                    "sa_gene", 
                    "sa_treatment")
    
    #Return unchanged df if no not-NA elements in df
    if (sum(!is.na(df$sample_attribute)) ==0){ 
        df <- createEmptyColumns(df, sa_columns[-1])
        
        #if (dim(df)[1]==0){
        #    df$sa_remainder <- character(0)
        #    df$sa_tissue <- character(0)
        #    df$sa_antibody <- character(0)
        #    df$sa_gene <- character(0)
        #    df$sa_treatment <- character(0)
        #} else {
        #    df$sa_remainder <- NA
        #    df$sa_tissue <- NA
        #    df$sa_antibody <- NA
        #    df$sa_gene <- NA
        #    df$sa_treatment <- NA
        #}
        
        if (rename_col==TRUE){
            colnames(df)[grepl("sample_attribute", colnames(df))] <- 
                                                        "SRA_sample_attribute"
        }
        warning("No not-NA sample attributes available")
        mm("saExtractor completed", "fn")
        return(df)
    }
    
    
    #========================================================================
    # Setting up keywords
    #========================================================================
    sra_tissue <- c("strain", 
                    "tissue", 
                    "source.?name", 
                    "isolation.?source", 
                    "isolate", 
                    "body.?site", 
                    "sample.?type", 
                    "cell.?type", 
                    "cell.?line", 
                    "ArrayExpress-CellType", 
                    "inferred.?cell.?type", 
                    "cell", 
                    "cre.?line", 
                    "cell.?description", 
                    "cell.?subtype", 
                    "cell.?or.?tisue.?type",
                    "ArrayExpress-StrainOrLine", 
                    "lineage", 
                    "line", 
                    "strain.?or.?line",
                    "body.?site", 
                    "site", 
                    "corrected.?sample.?site", 
                    "host.?body.?site",
                    "tissue.?type", 
                    "host.?tissue.?sampled", 
                    "tissue.?depot",
                    "source_material_id", 
                    "source",
                    "organism.?part") #TISSUE #ONLY USED HERE #RESEARCHED WELL
    
    sra_tissue <- paste0(sra_tissue, ": ")
    
    sra_antibody <- c("chip.antibody", 
                    "antibody", 
                    "ArrayExpress.Immunoprecipitate", 
                    "ip.antibody", 
                    "rip.antibody", 
                    "medip.antibody", 
                    "clip.antibody", 
                    "frip.antibody", 
                    "chip-seq.antibody") #ANTIBODY #RESEARCHED WELL
    
    sra_antibody <- paste0(sra_antibody, ": ")
    
    sra_gene <- c("genotype", 
                    "ArrayExpress.Genotype", 
                    "genotype/variation", 
                    "target.gene", 
                    "genetic.background", 
                    "host.genotype", 
                    "Plant.genotype", 
                    "genetic.modification", 
                    "transgene", 
                    "gene.id", 
                    "myd88.genotype", 
                    "gene.perturbation.type", 
                    "genetic.condition", 
                    "cytogenetics", 
                    "concise.genotype.name", 
                    "genspecies.abbr", 
                    "melanoma.genetic.conditions", 
                    "marker.gene", 
                    "gene", 
                    "strain/genotype", 
                    "genotype/variation", 
                    "knockout", 
                    "knockdown", 
                    "hgn") #GENE #RESEARCHED (based on sa_categories)
    
    sra_gene <- paste0(sra_gene, ": ")
    
    sra_treatment <- c("treatment", 
                        "ArrayExpress.Treatment", 
                        "treated.with", 
                        "treatment.description", 
                        "drug.treatment", 
                        "treatment.protocol", 
                        "Vaccine.Treatment", 
                        "experimental.treatment", 
                        "diet.treatment", 
                        "treatment.group") #TREATMENT #RESEARCHED
    
    sra_treatment <- paste0(sra_treatment, ": ")
    
    #========================================================================
    
    sra_attr_keywords <- list(sra_tissue, 
                                sra_antibody, 
                                sra_gene, 
                                sra_treatment)
    
    #========================================================================
    #========================================================================
    
    
    #========================================================================
    sra_sep_split <- " \\|\\| "
    sra_sep_collapse <- " || "
    #========================================================================
    
    
    df_sra_attr <- plyr::ldply(df$sample_attribute, 
                                function(x) universalExtractor(x, 
                                                            sra_attr_keywords, 
                                                            sra_sep_split, 
                                                            sra_sep_collapse))
    
    colnames(df_sra_attr) <- sa_columns

    
    #Combine extracted columns with df (except attr_original column)
    df <- cbind(df, df_sra_attr[,(-1)]) 
    
    
    
    
    if (rename_col==TRUE){
        colnames(df)[grepl("sample_attribute", colnames(df))] <- 
                                                        "SRA_sample_attribute"
    }
    
    #========================================================================
    
    mm("saExtractor completed", "fn")
    return(df)
    
}




