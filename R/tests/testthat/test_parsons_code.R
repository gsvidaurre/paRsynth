# G.A. Juarez
# 4 Dec 2024
# updated: RS, 2025-01-30

# Just for code development
# library(tidyverse)
# library(lubridate)
# library(testthat)
# library(dplyr)
rm(list = ls())
source("/Users/raneemsamman/Documents/GitHub/paRsynth/R/generate_strings.R")
source("/Users/raneemsamman/Documents/GitHub/paRsynth/R/parsons_code.R")

# Define parameters
n_groups <- 2
n_individuals <- 5
n_calls <- 10
globals <- 12
group_information <- 8
individual_information <- 2
random_variation <- 2
string_length <- group_information + individual_information + random_variation + globals

generated_strings <- generate_strings(
  n_groups = n_groups, n_individuals = n_individuals, n_calls = n_calls, string_length = string_length, 
  group_information = group_information, individual_information = individual_information, random_variation = random_variation
)

Conversion <- parsons_code(
  generated_strings,
  string_col = "Call", global_head_col = "Global_head", group_head_col = "Group_head",
  individual_middle_col = "Individual_middle", random_variation_col = "Random_variation", group_tail_col = "Group_tail",
  global_tail_col = "Global_tail", list("A" = "up", "B" = "down", "C" = "constant")
)

# Calculate the generated parsons code directions
count_words <- function(string) {
  length(strsplit(string, "-")[[1]])
}

# 1. Unit test to check that the parsons code is the correct length
test_that("This functions generates the correct length of parsons code", {
  # Calculate the length of the parsons code for each generated string then get the unique lengths
  generated_parsons_code <- unique(sapply(Conversion$Call_Parsons_Code, count_words)) # sapply is able to apply a function to each element of a list or vector

  # Check that the generated parsons code is the correct length
  expect_true(string_length == generated_parsons_code,
              info = "Not all generated calls have the expected number.")
})

# 2. Unit test to check that correct number of parson codes were generated
test_that("The functions generates the correct number of parson codes", {
  # Calculate how many parsons codes were generated
  n_generated_parsons_codes <- nrow(Conversion)

  # Find the number of expected parson codes
  n_expected_parsons_codes <- n_calls*n_groups*n_individuals

  # Check that the number of parsons codes were generated as expected
  expect_equal(n_generated_parsons_codes, n_expected_parsons_codes,
               info = "Not all parson codes were generated.")
})

# 3. Unit test to check that the correct parsons code were generated
test_that("The function generates correct parsons code", {
  # Generate generic strings (easy to track conversion)
  Global_head <- "AABA"
  Group_head <- "BBCC"
  Individual_middle <- "CCBA"
  Random_variation <- "BA"
  Group_tail <- "BBAC"
  Global_tail <- "BBAA"

  generated_strings <- data.frame(
    Call = paste(Global_head, Group_head, Individual_middle, Random_variation, Group_tail, Global_tail, sep = ""),
    Global_head = Global_head, Group_head = Group_head, Individual_middle = Individual_middle,
    Random_variation = Random_variation, Group_tail = Group_tail, Global_tail = Global_tail
  )

  # Convert using parsons_code
  Conversion <- parsons_code(
    generated_strings, string_col = "Call", global_head_col = "Global_head", group_head_col = "Group_head", 
    individual_middle_col = "Individual_middle", random_variation_col = "Random_variation", 
    group_tail_col = "Group_tail", global_tail_col = "Global_tail", list("A" = "up", "B" = "down", "C" = "constant")
  )
  
  # Check that the generated parsons code is the same as the expected parsons code ("A" = "up", "B" = "down", "C" = "constant")
  generated_parsons_code <- unname(Conversion$Call_Parsons_Code)[1]
  expected_parsons_code <- "up-up-down-up-down-down-constant-constant-constant-constant-down-up-down-up-down-down-up-constant-down-down-up-up"
  expect_equal(generated_parsons_code, expected_parsons_code)
})

# 4. Unit test to check that the df has the right number of dimensions (right number of rows and columns)
test_that("The function generates a data frame that has the right number of rows and columns",{
  # names(Conversion) returns the column names of the data frame Conversion
  conversation_cols <- names(Conversion)
  # Get the columns that contain the parsons code.
  parsons_df_cols <- conversation_cols[grep("_Parsons_", conversation_cols)]

  # Check that the data frame has the right number of rows and columns
  expect_equal(nrow(generated_strings),nrow(Conversion))
  expect_equal(ncol(generated_strings) + length(parsons_df_cols), ncol(Conversion))
  
})
