# G.A. Juarez
# 11 Nov 2024

rm(list = ls())

if (!require(testthat)) install.packages('testthat')
library(testthat)

source("~/Desktop/BIRDS/GitHub_repos/paRsynth/R/generate_strings.R")

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
test_that("Generated calls have the correct number", {

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

  # Get the number of unique individuals and expected calls
  unique_individuals <- n_groups*n_individuals
  n_expected_calls <- n_calls*unique_individuals

  # Get the number of generated calls
  generated_calls <- nrow(generated_strings)

  # Check that the correct number of calls were generated
  expect_true(generated_calls == n_expected_calls,
               info = "Not all generated calls have the expected number.")
})

# 3. Unit test to check that the number of groups are correct
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
  unique_groups <- length(unique(generated_strings$Group))

  # Check that the number of groups are correct
  expect_true(all(unique_groups == n_groups),
              info = "Not all number of groups were correctly generated.")
})

# 4. Unit test to check that the number of individuals are correct
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

# 5. Unit test to check that the number of characters in each string devoted to group information is correct
test_that("Generated number of characters in each string devoted to group information have the correct length", {

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
  group_information <- 8

  # Call the function with the test parameters
  generated_strings <- generate_strings(n_groups = n_groups, n_individuals = n_individuals, n_calls = n_calls, string_length = string_length, group_information = group_information, individual_information = 2)

  # glimpse(generated_strings)

  # List of all generated_calls
  generated_calls <- generated_strings$Call
  generated_calls

  # Extract the first four characters that pertain to group information
  group_information_1 <- substr(generated_calls, 4, 7)
  group_information_1

  # Distinguish first four characters of group 1
  group_information_1_1 <- sapply(group_information_1[1:50], function(x){
    print(char(x))
    }, USE.NAMES = FALSE)

  # Distinguish first four characters of group 2
  group_information_1_2 <- sapply(group_information_1[51:100], function(x){
    print(char(x))
  }, USE.NAMES = FALSE)

  # Extract the last four characters that pertain to group information
  group_information_2 <- substr(generated_calls, 10, 13)
  group_information_2

  # Distinguish last four characters of group 1
  group_information_2_1 <- sapply(group_information_2[1:50], function(x){
    print(char(x))
  }, USE.NAMES = FALSE)

  # Distinguish last four characters of group 2
  group_information_2_2 <- sapply(group_information_2[51:100], function(x){
    print(char(x))
  }, USE.NAMES = FALSE)

  # Total number of characters in each string devoted to group information for group 1
  total_group_information_1 <- unique(nchar(group_information_1_1) + nchar(group_information_2_1))
  total_group_information_1

  # Total number of characters in each string devoted to group information for group 2
  total_group_information_2 <- unique(nchar(group_information_1_2) + nchar(group_information_2_2))
  total_group_information_2

  # Check that the number of characters in each string devoted to group information is correct
  # Group 1
  expect_equal(total_group_information_1, group_information,
               info = "Not all generated strings devoted to group information have the expected length for group 1.")

  # Group 2
  expect_equal(total_group_information_2, group_information,
               info = "Not all generated strings devoted to group information have the expected length for group 2.")
})

# 6. Unit test to check that the number of characters in each string devoted to individual information is correct
test_that("Generated number of characters in each string devoted to individual information have the correct length", {

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
  group_information <- 8
  individual_information <- 2

  # Call the function with the test parameters
  generated_strings <- generate_strings(n_groups = n_groups, n_individuals = n_individuals, n_calls = n_calls, string_length = string_length, group_information = group_information, individual_information = individual_information)

  # glimpse(generated_strings)

  # List of all generated_calls
  generated_calls <- generated_strings$Call
  generated_calls

  # Expected number of unique individual information
  unique_individuals_information <- n_individuals*n_groups
  unique_individuals_information

  # Total calls with the unique individual information


  # Extract the two characters that pertain to individual information
  individuals_information <- substr(generated_calls, 8, 9)
  individuals_information

  # Distinguish individual information from individual 1-5 in group 1 and 2
  individuals_information_1 <- sapply(individuals_information[1:10], function(x){
    print(char(x))
  }, USE.NAMES = FALSE)
  individuals_information_1
  # AC

  individuals_information_2 <- sapply(individuals_information[11:20], function(x){
    print(char(x))
  }, USE.NAMES = FALSE)
  individuals_information_2
  # CC

  individuals_information_3 <- sapply(individuals_information[21:30], function(x){
    print(char(x))
  }, USE.NAMES = FALSE)
  individuals_information_3
  # CA

  individuals_information_4 <- sapply(individuals_information[31:40], function(x){
    print(char(x))
  }, USE.NAMES = FALSE)
  individuals_information_4
  # CB

  individuals_information_5 <- sapply(individuals_information[41:50], function(x){
    print(char(x))
  }, USE.NAMES = FALSE)
  individuals_information_5
  # AB


  # Check that the number of characters in each string devoted to individual information is correct
  expect_equal(total_group_information_1, group_information,
               info = "Not all generated strings devoted to group information have the expected length for group 1.")
})

# 7. Unit test to check that the number of characters in each string devoted to global head and is correct

# 8. Unit test to check that the number of characters in each string devoted to global tail is correct
