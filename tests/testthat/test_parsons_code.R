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

  # Generate strings
  n_calls <- 10
  n_groups <- 2
  n_individuals <- 5

  generated_strings <- generate_strings(n_groups = n_groups, n_individuals = n_individuals, n_calls = n_calls, string_length = 16, group_information = 8, individual_information = 2)

  # glimpse(generated_strings)

  # Convert generated strings to parsons code
  Conversion <- parsons_code(generated_strings, "Call", list("A" = "up", "B" = "down", "C" = "constant"))

  # glimpse(Conversion)

  # Calculate how many parsons codes that were generated
  n_generated_parsons_codes <- nrow(Conversion)

  # Get the number of expected parson codes
  n_expected_parsons_codes <- n_calls*n_groups*n_individuals

  # Check that the generated parsons code is the correct length
  expect_equal(n_generated_parsons_codes, n_expected_parsons_codes,
              info = "Not all parson codes were generated.")
})

# 3. Unit test to check that the correct parsons code were generated
test_that("Generated parsons code are correct",{

  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("dplyr")
  withr::local_package("lubridate")

  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)

  # Make up generated strings
  generated_strings <- data.frame(
    Call = c("AAA", "BBB", "CCC")
  )

 # Use parsons_code to convert it
  Conversion <- parsons_code(generated_strings, "Call", list("A" = "up", "B" = "down", "C" = "constant"))

  generated_parsons_code <- unname(Conversion$Parsons_Code)

  # Check that the generated parsons code is the same as the expected parsons code ("A" = "up", "B" = "down", "C" = "constant")
  expect_equal(generated_parsons_code[1], "up-up-up")
  expect_equal(generated_parsons_code[2], "down-down-down")
  expect_equal(generated_parsons_code[3], "constant-constant-constant")
})
