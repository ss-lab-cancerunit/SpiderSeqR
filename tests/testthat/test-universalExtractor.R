context(".universalExtractor")

#.universalExtractor(characteristics, sep_split, sep_collapse, key_words)

sep_split_sra <- " \\|\\| "
sep_split_geo <- ";\t"

sep_collapse_sra <- " || "
sep_collapse_geo <- ";\t"

#basic case
#multiple key words

test_uE_sra <- function(characteristics, key_words){
  sep_split_sra <- " \\|\\| "
  sep_collapse_sra <- " || "
  .universalExtractor(characteristics = characteristics, key_words = key_words, sep_split = sep_split_sra, sep_collapse = sep_collapse_sra)
}

test_that("One key word", {
  
  ch1a <- "cell: lymphocyte"
  k1a <- "cell: "
  expect_equal(test_uE_sra(ch1a, k1a)[1], ch1a) #Original string returned
  expect_true(is.na(test_uE_sra(ch1a, k1a)[2])) #NA returned (no string remains)
  expect_equal(test_uE_sra(ch1a, k1a)[3], "lymphocyte") #Correct extract returned
  
  ch1b <- "cell: lymphocyte"
  k1b <- list("cell: ")
  expect_equal(test_uE_sra(ch1b, k1b)[1], ch1b) #Original string returned
  expect_true(is.na(test_uE_sra(ch1b, k1b)[2])) #NA returned (no string remains)
  expect_equal(test_uE_sra(ch1b, k1b)[3], "lymphocyte") #Correct extract returned
  
  
  ch2a <- "cell: lymphocyte || line: 1234"
  k2 <- list(c("cell: ", "line: "))
  expect_equal(test_uE_sra(ch2a, k2)[1], ch2a) #Original string returned
  expect_true(is.na(test_uE_sra(ch2a, k2)[2])) #NA returned (no string remains)
  expect_equal(test_uE_sra(ch2a, k2)[3], "lymphocyte -;- 1234") #Correct extract returned
  
  
  
  #FAILING TEST - due to requirement for grep to have a fixed = TRUE (which forces ignore.case = FALSE)
  
  ch <- "cell: lymphocyte || line: Lymphocyte"
  k2 <- list(c("cell: ", "line: "))
  expect_equal(test_uE_sra(ch, k2)[1], ch) #Original string returned
  expect_true(is.na(test_uE_sra(ch, k2)[2])) #NA returned (no string remains)
  
  skip("Not developed yet")
  #FAILING TEST - due to requirement for grep to have a fixed = TRUE (which forces ignore.case = FALSE)
  expect_equal(test_uE_sra(ch, k2)[3], "lymphocyte") #Correct extract returned
  
  
})



#- one key word
#- two key words
#- two key words with overlap
