# G.A. Juarez
# 15 Nov 2024

# parsons_code():
#   1 Test for the generation of correct parsons code length
#   2 Test for the generation of correct parsons code amount
#   3 Test for the generation of correct parsons code
#   4	Test the function performance for multiple row entry

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

  # Convert generated strings to parsons code
  Conversion <- parsons_code(generated_strings, "Call", list("A" = "up", "B" = "down", "C" = "constant"))

  # glimpse(Conversion)

  generated_parsons_code <- sapply(Conversion$Parsons_Code, function(x){
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
  generated_parsons_code <- (sapply(count_words(generated_parsons_code), function(y){
    print(y)
    }, USE.NAMES = FALSE))

  generated_parsons_code_length <- generated_parsons_code / (n_groups*n_individuals*n_calls)

  # Check that the generated parsons code is the correct length
  expect_true(string_length == generated_parsons_code_length,
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

  # Convert generated strings to parsons code
  Conversion <- parsons_code(generated_strings, "Call", list("A" = "up", "B" = "down", "C" = "constant"))

  # glimpse(Conversion)

  generated_parsons_code <- sapply(Conversion$Parsons_Code, function(x){
    print(x)
  }, USE.NAMES = FALSE)

  # Calculate how many parsons codes that were generated
  n_generated_parsons_codes <- nrow(generated_parsons_code)

  # Get the number of expected parson codes
  n_generated_parsons_codes <- n_calls*n_groups*n_individuals

  # Check that the generated parsons code is the correct length
  expect_true(n_generated_parsons_codes == n_generated_parsons_codes,
              info = "Not all parson codes were generated.")
})

# 3. Unit test to check that the correct parsons code were generated
test_that("Generated parsons code are correct",{

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
  # library(stringr)

  # Define parameter
  n_groups <- 3
  n_individuals <- 4
  n_calls <- 5
  string_length <- 16
  group_information <- 8
  individual_information <- 2

  # Generate_strings
  generated_strings <- generate_strings(n_groups = n_groups, n_individuals = n_individuals, n_calls = n_calls, string_length = string_length, group_information = group_information, individual_information = individual_information)

  # Generate parsons code from generated strings
  Conversion <- parsons_code(generated_strings, "Call", list("A" = "up", "B" = "down", "C" = "constant"))

  # glimpse(Conversion)

  generated_parsons_code <- sapply(Conversion$Parsons_Code, function(x){
    print(x)
  }, USE.NAMES = FALSE)
})

# 4. Unit test to check performance for multiple row entry
test_that("Generated parsons code for multiple row entry are correct", {

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
  Conversion <- parsons_code(generated_strings, "Call", list("A" = "up", "B" = "down", "C" = "constant"))

  # glimpse(Conversion)

  generated_parsons_code <- sapply(Conversion$Parsons_Code, function(x){
    print(x)
  }, USE.NAMES = FALSE)
})
