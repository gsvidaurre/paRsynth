# R. Samman
# 2024-12-17

rm(list = ls())

if (!require(testthat)) install.packages('testthat')
library(testthat)

source("/Users/raneemsamman/Documents/GitHub/paRsynth/R/generate_strings.R")

test_that("Error handling for generate_strings", {
  
  # Test that string length is between 6 and 200
  expect_error(
    generate_strings(
      n_groups = 2,
      n_individuals = 5,
      n_calls = 10,
      string_length = 5,
      group_information = 8,
      individual_information = 2),
    "string_length must be between 6 and 200"
  )

  expect_error(
    generate_strings(
      n_groups = 2,
      n_individuals = 5,
      n_calls = 10,
      string_length = 201,
      group_information = 8,
      individual_information = 2), 
    "string_length must be between 6 and 200"
  )
  
  # Test that group information is specified correctly (numeric value required)
  expect_error(
    generate_strings(
      n_groups = 2,
      n_individuals = 5,
      n_calls = 10,
      string_length = 16,
      group_information = "8",  # String instead of numeric
      individual_information = 2),
    "group_information must be numeric"
  )

  # Test that individual information is specified correctly (numeric value required)
  expect_error(
    generate_strings(
      n_groups = 2,
      n_individuals = 5,
      n_calls = 10,
      string_length = 16,
      group_information = 8,
      individual_information = "two"),  # String instead of numeric
    "individual_information must be numeric"
  )
  
  # Test that group information is even
  expect_error(
    generate_strings(
      n_groups = 2,
      n_individuals = 5,
      n_calls = 10,
      string_length = 16,
      group_information = 7,  # Not even
      individual_information = 2),
    "group_information and individual_information must be even numbers"
  )

  # Test that individual information is even
  expect_error(
    generate_strings(
      n_groups = 2,
      n_individuals = 5,
      n_calls = 10,
      string_length = 16,
      group_information = 8,
      individual_information = 3),  # Not even
    "group_information and individual_information must be even numbers"
  )
  
  # Test that all arguments are integers (whole numbers)
  expect_error(
    generate_strings(
      n_groups = 2,
      n_individuals = 5.0,  # Decimal instead of integer
      n_calls = 10,
      string_length = 16,
      group_information = 8,
      individual_information = 2.5),  # Decimal instead of integer
    "All arguments must be integers"
  )

  # Test that all arguments are greater than 0
  expect_error(
    generate_strings(
      n_groups = 2,
      n_individuals = 5,
      n_calls = 0,  # 0 calls should raise an error
      string_length = 16,
      group_information = 8,
      individual_information = 2),
    "All arguments must be greater than 0"
  )
})
