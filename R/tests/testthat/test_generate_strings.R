# R. Samman
# November 07 2024

rm(list = ls())

if (!require(testthat)) install.packages('testthat')
library(testthat)

source("/Users/raneemsamman/Documents/GitHub/paRsynth/R/generate_strings.R")

# Checking that the number of groups and individuals is correct

    # what do we want to do?
        # 1. check n_groups
        # 2. check n_individuals (per group?)
        # 3. n_calls (i am adding this here bc it will be a double test for n_individuals)
    # how are we doing that?
        # 1. set a hypothetical set of parameters
        # 2. call the function using my hypothetical parameters
        # 3. check if the number of rows created is correct:
            # n_groups * n_individuals * n_calls should give me the total number of rows
        # 4. check if n_groups created is correct:
            # use equal for the number of gruops (unique ones so that it doesnt double count) in the group column
        # 5. check if n_individuals created is correct
            # use equal for the number of individuals (unique ones) in the individuals column

test_that("The function generates the number of groups and individuals", {

# Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("data.table")
  
  # Just for code development
#   library(tidyverse)
#   library(lubridate)
#   library(testthat)
#   library(data.table)
#   library(stringer)

#   # Create a temporary directory for testing. Files will be written and read here
#   path <- "/Users/raneemsamman/Desktop"
#   data_dir <- "tmp_tests"
#   tmp_path <- file.path(path, data_dir)
#   if(!dir.exists(tmp_path)){ 
#     dir.create(tmp_path)
#   }

# 1. set a hypothetical set of parameters
    n_groups = 3
    n_individuals = 4
    n_calls = 5
    string_length = 16
    group_information = 8
    individual_information = 2

# 2. call the function using my hypothetical parameters
    test_result <- generate_strings(n_groups = n_groups, n_individuals = n_individuals, n_calls = n_calls, string_length = string_length, group_information = group_information, individual_information = individual_information)

# 3. check if the number of rows created is correct:
    # n_groups * n_individuals * n_calls should give me the total number of rows
    expected_rows <- n_groups * n_individuals * n_calls
    expect_equal(nrow(test_result), expected_rows)
    cat("expected rows:", expected_rows, "\n")
    cat("expected n_calls", n_calls, "\n")
# 4. check if n_groups created is correct:
    # use equal for the number of gruops (unique ones so that it doesnt double count) in the group column
    expect_equal(length(unique(test_result$Group)), n_groups)
    cat("expected n_groups:", n_groups, "\n")
    cat("unique n_groups in test", unique(test_result$Group), "\n")
    cat("number of n_groups in test", length(unique(test_result$Group)), "\n")

# 5. check if n_individuals created is correct
    # use equal for the number of individuals (unique ones) in the individuals column
    cat("expected n_individuals:", n_individuals, "\n")
    for (group in 1:n_groups){
        test_group_individuals <- unique(test_result[test_result$Group == group, "Individual"])
        expect_equal(length(test_group_individuals), n_individuals)
    }
    cat("unique n_individuals in test", (test_group_individuals), "\n")
    cat("number of n_individuals in test", length(test_group_individuals), "\n")


})
# library(testthat)
# test_file("/Users/raneemsamman/Documents/GitHub/paRsynth/R/tests/testthat/test_generate_strings.R")

# Checking that the number of characters in each string devoted to group information, individual information, and global head and tail are correct
rm(list = ls())

if (!require(testthat)) install.packages('testthat')
library(testthat)

source("/Users/raneemsamman/Documents/GitHub/paRsynth/R/generate_strings.R")
# Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("data.table")
  
  # Just for code development
  library(tidyverse)
  library(lubridate)
  library(testthat)
  library(data.table)
  library(stringer)

test_that("The function generates character-based vocal strings per catergory", {

# 1. set a hypothetical set of parameters
n_groups = 3
n_individuals = 4
n_calls = 5
string_length = 16
group_information = 8
individual_information = 2
# 2. call the function using my hypothetical parameters
test_result2 <- generate_strings(n_groups = n_groups, n_individuals = n_individuals, n_calls = n_calls, string_length = string_length, group_information = group_information, individual_information = individual_information)

# Get all the calls generated
  for (call in 1:nrow(test_result2)) {
    current_call <- test_result2$Call[call]
    cat("-------- Testing current call: --------", current_call, "\n")
    
    # Check if the total length of the string matches the expected string length
    expect_equal(nchar(current_call), string_length)
    
    # Calculate the length of the global head and tail
    expected_head_tail_length <- (string_length - group_information - individual_information) / 2
    
    # Get the global head and check its length
    global_head <- stringr::str_sub(current_call, 1, expected_head_tail_length)
    cat("Global head string is:", global_head, "\n")
    cat("Expected global head length is", expected_head_tail_length, "\n")
    expect_equal(nchar(global_head), expected_head_tail_length)
    
    # Get the global tail and check its length
    global_tail <- stringr::str_sub(current_call, -expected_head_tail_length)
    cat("Global tail string is:", global_tail, "\n")
    cat("Expected global tail length is", expected_head_tail_length, "\n")
    expect_equal(nchar(global_tail), expected_head_tail_length)
    
    # calculate the half-length of the group membership
    half_group_length <- (group_information - individual_information) / 2
    # get the group middle (split into two parts)
    group_middle_head <- stringr::str_sub(current_call, expected_head_tail_length + 1, expected_head_tail_length + half_group_length)
    group_middle_tail <- stringr::str_sub(current_call, expected_head_tail_length + half_group_length + individual_information + 1, 
                                           expected_head_tail_length + group_information)
    cat("Group middle head string is:", group_middle_head, "\n")
    cat("Expected group middle head length is", half_group_length, "\n")
    expect_equal(nchar(group_middle_head), half_group_length)
    
    cat("Group middle tail string is:", group_middle_tail, "\n")
    cat("Expected group middle tail length is", half_group_length, "\n")
    expect_equal(nchar(group_middle_tail), half_group_length)
    
    # get the individual middle section 
    individual_middle <- stringr::str_sub(current_call, expected_head_tail_length + half_group_length + 1, 
                                           expected_head_tail_length + half_group_length + individual_information)
    cat("Individual middle string is:", individual_middle, "\n")
    cat("Expected individual middle length is", individual_information, "\n")
    expect_equal(nchar(individual_middle), individual_information)
    
  }
})

# the generated strings encode the string in the following set up:
# Global head    HALF Group membership    Individual Membership   the other half of the group memebership info            Global Tail
#     AAA            CBCB                   DD                                   CBCB                                        AAA
# AAA CBCBCBCB DD AAA