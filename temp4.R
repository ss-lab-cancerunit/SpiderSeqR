t1 <- data.frame(experiment_title = df_broadest[sample(1:553, 50),19])

t1$match <- findSRAIndicesBasedOnMatchesInLevels("stat", t1, "experiment")

View(t1)


#---------------


df <- read.csv("test_findSRAIndices_input.csv")

df$match_output <- findSRAIndicesBasedOnMatchesInLevels("stat3", df, "experiment")

df$match_output <- findSRAIndicesBasedOnMatchesInLevels("\\bstat3\\b", df, "experiment")

df$match_output <- findSRAIndicesBasedOnMatchesInLevels(c("match", "hpc"), df, "experiment")





not_allowed <- grepl("\\sNOT\\s", x) | grepl("^[[:alnum:]]", x)


if(not_allowed){
  warning("The query contains 'NOT' or non-alphanumeric characters. Non-filtered data frame will be returned")
  
  # ===*=== return original df/indices
}


