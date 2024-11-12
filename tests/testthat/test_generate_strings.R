# G.A. Juarez
# 11 Nov 2024

rm(list = ls())

if (!require(testthat)) install.packages('testthat')
library(testthat)

source("~/Desktop/GitHub_repos/paRsynth/R/generate_strings.R")

# Unit test to check string length
test_that("Generated strings have the correct length", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  
  # Define parameters
  string_length <- 16
  n_calls <- 10
  
  # Call the function with the test parameters
  generated_strings <- generate_strings(n_groups = 2, n_individuals = 5, n_calls = n_calls, string_length = string_length, group_information = 8, individual_information = 2)
  
  # glimpse(generated_strings)
  
  # Extract the length of each generated string
  # i <- CBAABBAABBBBCB # testing
  string_lengths <- sapply(generated_strings$Call, FUN = function(i){
    nchar(i)
  }, USE.NAMES = FALSE)
  
  # Check that each string has the correct length
  expect_true(all(string_lengths == string_length),
              info = "Not all generated strings have the expected length.")
})

# Unit test to check that correct number of string were generated
test_that("Generated strings are correct number", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  
  # Define parameters
  string_length <- 16
  n_calls <- 10
  n_groups <- 2
  n_individuals <- 5
  
  # Call the function with the test parameters
  generated_strings <- generate_strings(n_groups = n_groups, n_individuals = n_individuals, n_calls = n_calls, string_length = string_length, group_information = 8, individual_information = 2) 
  
  # glimpse(generated_strings)
  
  # Get the number of unique individuals
  unique_individuals <- n_groups*n_individuals
  n_expected_calls <- n_calls*unique_individuals
  n_expected_calls
  
  # Check that the correct number of strings were generated
expect_equal(n_expected_calls, nrow(generated_strings),
             info = "Not all number of strings were correctly generated.")
})
