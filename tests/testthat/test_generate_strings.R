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
  expect_equal(string_lengths, string_length,
              info = "Not all generated strings have the expected length.")
})

# Unit test to check that correct number of string were generated
test_that("Generated number of strings are correct number", {

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
  generated_calls <- nrow(generated_strings)
  generated_calls
  # Check that the correct number of strings were generated
  expect_equal(n_expected_calls, generated_calls,
  info = "Not all number of strings were correctly generated.")
})
# I am not sure why its not passing the test

# Unit test to check that the number of groups are correct
test_that("Generated groups are correct number", {

  # Avoid library calls and other changes to the virual environment
  # See https://r-pkgs.rg/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("dplyr")
  withr::local_package("lubridate")

  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library (testthat)

  # Define parameters
  string_length <- 16
  n_calls <- 10
  n_groups <- 2
  n_individuals <- 5

  # Call the function with the test parameters
  generated_strings <- generate_strings(n_groups = n_groups, n_individuals = n_individuals, n_calls = n_calls, string_length = string_length, group_information = 8, individual_information = 2)

  # glimpse(generated_strings)

  # Extract the number of unique groups
  # t <- 1 # testing
  unique_groups <- length(unique(generated_strings$Group))

  # Check that the number of groups are correct
  expect_true(all(unique_groups == n_groups),
              info = "Not all number of groups were correctly generated.")
})

# Unit test to check that the number of individuals are correct
test_that("Generated number of individuals are correct", {

  # Avoid library calls and other changes to the  virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
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
  unique_individuals <- length(unique(generated_strings$Individual))

  # Check that the correct the number of individuals are correct
  expect_equal(unique_individuals, n_individuals,
               info = "Not all numbers of individuals were generated correctly.")
})

# Check that the number of characters in each string devoted to group information, individual information, and global head and tail are correct
# Google doc
