# G.A. Juarez
# 15 Nov 2024

rm(list = ls())

if (!require(testthat)) install.packages('testthat')
library(testthat)

source("~/Desktop/BIRDS/GitHub_repos/paRsynth/R/generate_strings.R")
source("~/Desktop/BIRDS/GitHub_repos/paRsynth/R/parsons_code.R")

# 1. Unit test to check that the parsons code is the correct length
test_that("Generated parsons code is the correct length", {

  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("dplyr")
  withr::local_package("lubridate")

  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  
  #Define parameters
  n_groups <- 2
  n_individuals <- 5
  n_calls <- 10
  string_length <- 16

  # Generate strings
  generated_strings <- generate_strings(n_groups = n_groups, n_individuals = n_individuals, n_calls = n_calls, string_length = string_length, group_information = 8, individual_information = 2)
  
  # Generate parsons code from generated strings
  generated_strings_parsons <- parsons_code(generated_strings, "Call", list("A" = "up", "B" = "down", "C" = "constant"))

  # glimpse(generated_strings_parsons)

  generated_parsons_code <- sapply(generated_strings_parsons$Parsons_Code, function(x){
    print(x)
  }, USE.NAMES = FALSE)
  
  # Calculate parsons code directions generated
  count_words <- function(string) {
    
    # Split the string by hyphen
    words <- unlist(strsplit(string, "-"))
    
    # Count the number of words (segments) between hyphens
    length(words)
  }
  
  # Calculate how length of parsons code
  expected_parsons_code <- (sapply(count_words(generated_parsons_code), function(y){
    print(y)
    }, USE.NAMES = FALSE)) 
  
  n_expected_parsons_code <- expected_parsons_code/ (n_groups*n_individuals*n_calls)
  
  # Check that the generated parsons code is the correct length
  expect_true(string_length == n_expected_parsons_code,
              info = "Not all generated calls have the expected number.")
})

# 2. Unit test to check that correct number of parson code strings were generated
test_that("Generated correct number of parson code strings", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  
  #Define parameters
  n_groups <- 2
  n_individuals <- 5
  n_calls <- 10
  string_length <- 16
  
  # Generate strings
  generated_strings <- generate_strings(n_groups = n_groups, n_individuals = n_individuals, n_calls = n_calls, string_length = string_length, group_information = 8, individual_information = 2)
  
  # Generate parsons code from generated strings
  generated_strings_parsons <- parsons_code(generated_strings, "Call", list("A" = "up", "B" = "down", "C" = "constant"))
  
  # glimpse(generated_strings_parsons)
  
  generated_parsons_code <- sapply(generated_strings_parsons$Parsons_Code, function(x){
    print(x)
  }, USE.NAMES = FALSE)
  
  # Calculate how many parsons codes that were generated
  n_generated_parsons_codes <- nrow(generated_strings_parsons)
  
  # Get the number of expected parson codes
  n_expected_parsons_codes <- n_calls*n_groups*n_individuals

  # Check that the generated parsons code is the correct length
  expect_true(n_generated_parsons_codes == n_expected_parsons_codes,
              info = "Not all generated calls have the expected number.")
})

# 3. Unit test to check that the parsons code is correctly generated
test_that("Generated parsons code is correct", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  
  # Generate strings
  generated_strings <- generate_strings(n_groups = 2, n_individuals = 5, n_calls = 10, string_length = 16, group_information = 8, individual_information = 2)

  # Generate parsons code from generated strings
  generated_stings_parsons <- parsons_code(generated_strings, "Call", list("A" = "up", "B" = "down", "C" = "constant"))
  
  # glimpse(generated_strings_parsons)
  
  generated_parsons_code <- sapply(generated_strings_parsons$Parsons_Code, function(x){
    print(x)
    }, USE.NAMES = FALSE)
  
  # Get the parsons code from the generated strings
  expect_parsons_code <- generated_strings
  expected_parsons_code <- sapply(expect_parsons_code, function(x){
    print(x)}, USE.NAMES = FALSE)
  
  # Check that each string has the correct length
  expect_identical(generated_parsons_code, expected_parsons_code,
               info = "Not all generated strings have the expected length.")
})