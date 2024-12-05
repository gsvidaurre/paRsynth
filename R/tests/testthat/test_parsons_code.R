# G.A. Juarez
# 4 Dec 2024

rm(list = ls())

if (!require(testthat)) install.packages('testthat')
library(testthat)

# Change "~Desktop/.../GitHub_repos" based on where paRsynth is stored
source("~/Desktop/BIRDS/GitHub_repos/paRsynth/R/generate_strings.R")
source("~/Desktop/BIRDS/GitHub_repos/paRsynth/R/parsons_code.R")

# 1. Unit test to check that the parsons code is the correct length
test_that("This functions generates the correct length of parsons code", {

  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("dplyr")
  withr::local_package("lubridate")

  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  # library(dplyr)

  # Define parameters
  n_groups <- 2
  n_individuals <- 5
  n_calls <- 10
  string_length <- 16

  # Generate strings using the parameters
  generated_strings <- generate_strings(n_groups = n_groups, n_individuals = n_individuals, n_calls = n_calls, string_length = string_length, group_information = 8, individual_information = 2)

  # Convert the previously generated strings to parsons code
  Conversion <- parsons_code(generated_strings, "Call", list("A" = "up", "B" = "down", "C" = "constant"))
  # glimpse(Conversion)
  # View(Conversion)

  # Calculate the generated parsons code directions
  count_words <- function(string) {

    # Split the string by hyphen
    words <- unlist(strsplit(string, "-"))

    # Count the number of words (segments) between hyphens
    length(words)
  }

  # Calculate the length of the parsons code for each generated string
  generated_parsons_code <- count_words(Conversion$Parsons_Code) / (n_groups*n_individuals*n_calls)

  # Check that the generated parsons code is the correct length
  expect_true(string_length == generated_parsons_code,
              info = "Not all generated calls have the expected number.")
})

# 2. Unit test to check that correct number of parson codes were generated
test_that("The functions generates the correct number of parson codes", {

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
  n_calls <- 10
  n_groups <- 2
  n_individuals <- 5

  # Generate strings using the parameters
  generated_strings <- generate_strings(n_groups = n_groups, n_individuals = n_individuals, n_calls = n_calls, string_length = 16, group_information = 8, individual_information = 2)

  # Convert the previously generated strings to parsons code
  Conversion <- parsons_code(generated_strings, "Call", list("A" = "up", "B" = "down", "C" = "constant"))
  # glimpse(Conversion)
  # View(Conversion)

  # Calculate how many parsons codes were generated
  n_generated_parsons_codes <- nrow(Conversion)

  # Find the number of expected parson codes
  n_expected_parsons_codes <- n_calls*n_groups*n_individuals

  # Check that the number of parsons codes were generated as expected
  expect_equal(n_generated_parsons_codes, n_expected_parsons_codes,
               info = "Not all parson codes were generated.")
})

# 3. Unit test to check that the correct parsons code were generated
test_that("The function generates correct parsons code",{

  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("dplyr")
  withr::local_package("lubridate")

  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)

  # Generate generic strings (easy to track conversion)
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

# 4. Unit test to check that the df has the right number of dimensions (right number of rows and columns)
test_that("The function generates a data frame that has the right number of rows and columns",{

  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("dplyr")
  withr::local_package("lubridate")

  # Just for code development
  library(tidyverse)
  library(lubridate)
  library(testthat)

  # Generate strings
  generated_strings <- generate_strings(n_groups = 2, n_individuals = 5, n_calls = 10, string_length = 16, group_information = 8, individual_information = 2)


  # Use parsons_code to convert the generated strings
  generated_parsons <- parsons_code(generated_strings, "Call", list("A" = "up", "B" = "down", "C" = "constant"))
  # glimpse(generated_parsons)
  # View(generated_parsons)

  # Check that the data frame has the right number of rows
  expect_equal(nrow(generated_strings),nrow(generated_parsons))

  # Check that the data frame has the right number of columns
  expect_equal(ncol(generated_strings)+1, ncol(generated_parsons))
})
