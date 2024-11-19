# G.A. Juarez
# 11 Nov 2024

rm(list = ls())

if (!require(testthat)) install.packages('testthat')
library(testthat)

source("~/Desktop/BIRDS/GitHub_repos/paRsynth/R/generate_strings.R")

# 1. Test that string length is correct
# 2. Test that correct number of string were generated

# 1. Unit test to check string length
test_that("Generated strings have the correct length", {

  # Avoid library calls and other changes to the virtual environment
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

  # Call the function with the test parameters
  generated_strings <- generate_strings(n_groups = 2, n_individuals = 5, n_calls = n_calls, string_length = string_length, group_information = 8, individual_information = 2)

  # glimpse(generated_strings)

  # Extract the length of each generated string
  string_lengths <- nchar(generated_strings$Call[1])

  # Check that each string has the correct length
  expect_equal(string_length, string_lengths,
              info = "Not all generated strings have the expected length.")
})

# 2. Unit test to check that correct number of string were generated
test_that("Generated correct number of strings", {

  # Avoid library calls and other changes to the virtual environment
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

  # Get the number of generated calls
  generated_calls <- nrow(generated_strings)

  # Get the number of expected calls
  n_expected_calls <- n_calls*n_groups*n_individuals

  # Check that the correct number of calls were generated
  expect_true(generated_calls == n_expected_calls,
               info = "Not all generated calls have the expected number.")
})
