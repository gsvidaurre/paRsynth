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

test_that("The function generates the number of groups and individuals"){
# Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("data.table")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  # library(data.table)

  # Create a temporary directory for testing. Files will be written and read here
  path <- "/Users/raneemsamman/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }

# 1. set a hypothetical set of parameters
    n_groups = 3
    n_individuals = 4
    n_calls = 5
    string_length = 16
    group_information = 8
    individual_information = 2

# 2. call the function using my hypothetical parameters
    test_result <- generate strings(n_groups = n_groups, n_individuals = n_individuals, n_calls = n_calls, string_length = string_length, group_information = group_information, individual_information = individual_information)

# 3. check if the number of rows created is correct:
    # n_groups * n_individuals * n_calls should give me the total number of rows
    expected_rows <- n_groups * n_individuals * n_calls
    expect_equal(nrow(test_result), expected_rows)
    print("expected_rows:" expected_rows)

# 4. check if n_groups created is correct:
    # use equal for the number of gruops (unique ones so that it doesnt double count) in the group column
    expect_equal(length(unique(test_result$Group)), n_groups)
    print("expected n_groups:" n_groups)
    print("unique n_groups in test" unique(test_result$Group))

# 5. check if n_individuals created is correct
    # use equal for the number of individuals (unique ones) in the individuals column
    for (group in 1:n_groups){
        test_group_individuals <- unique(test_result[test_result$Group == group, "Individual"])
        expect_equal(test_group_individuals, n_individuals)
    }

}

library(testthat)
test_file("/Users/raneemsamman/Documents/GitHub/paRsynth/R/tests/testthat/test_generate_strings.R")

# # Checking that the number of characters in each string devoted to group information, individual information, and global head and tail are correct

# test_that("The function generates character-based vocal strings per catergory"){

#   # Create a temporary directory for testing. Files will be written and read here
#   # Generate files of raw  data per unique date


# # Remove the temporary directory and all files within it
#   if(tmp_path == file.path(path, data_dir)){
#     unlink(tmp_path, recursive = TRUE)
#   }
# }