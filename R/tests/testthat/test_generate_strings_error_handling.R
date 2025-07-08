# Author: Raneem Samman
# Date created: December 17, 2024

# Assign base parameters
base_parameters <- list(
  n_groups = 2,
  n_individuals = 5,
  n_calls = 10,
  string_length = 16,
  group_information = 8,
  individual_information = 2,
  random_variation = 2,
  alphabet = c("A", "B", "C"),
  string_structure = "GI-II-RV-GI"
)
test_that("Error handling for generate_strings", {

  # Test that string length is between 6 and 200
  invalid_string_lengths <- c(5, 201)
  for (invalid_length in invalid_string_lengths) {
    expect_error(
      generate_strings(
        n_groups = base_parameters$n_groups,
        n_individuals = base_parameters$n_individuals,
        n_calls = base_parameters$n_calls,
        string_length = invalid_length,
        group_information = base_parameters$group_information,
        individual_information = base_parameters$individual_information),
      "string_length must be between 6 and 200"
    )
  }

  # Test that group information is specified correctly (numeric value required)
  expect_error(
    generate_strings(
      n_groups = base_parameters$n_groups,
      n_individuals = base_parameters$n_individuals,
      n_calls = base_parameters$n_calls,
      string_length = base_parameters$string_length,
      group_information = "eight",  # String instead of numeric
      individual_information = base_parameters$individual_information
    ),
    "group_information must be numeric"
  )

  # Test that individual information is specified correctly (numeric value required)
  expect_error(
    generate_strings(
      n_groups = base_parameters$n_groups,
      n_individuals = base_parameters$n_individuals,
      n_calls = base_parameters$n_calls,
      string_length = base_parameters$string_length,
      group_information = base_parameters$group_information,
      individual_information = "two"  # String instead of numeric
    ),
    "individual_information must be numeric"
  )

  # Test that group information is even
  expect_error(
    generate_strings(
      n_groups = base_parameters$n_groups,
      n_individuals = base_parameters$n_individuals,
      n_calls = base_parameters$n_calls,
      string_length = base_parameters$string_length,
      group_information = 7,  # Not even
      individual_information = base_parameters$individual_information
    ),
    "group_information and individual_information must be even numbers"
  )

  # Test that individual information is even
  expect_error(
    generate_strings(
      n_groups = base_parameters$n_groups,
      n_individuals = base_parameters$n_individuals,
      n_calls = base_parameters$n_calls,
      string_length = base_parameters$string_length,
      group_information = base_parameters$group_information,
      individual_information = 3  # Not even
    ),
    "group_information and individual_information must be even numbers"
  )

  # Test that all arguments are integers (whole numbers)
  expect_error(
    generate_strings(
      n_groups = base_parameters$n_groups,
      n_individuals = 5.0,  # Decimal instead of integer
      n_calls = base_parameters$n_calls,
      string_length = base_parameters$string_length,
      group_information = base_parameters$group_information,
      individual_information = 2.5  # Decimal instead of integer
    ),
    "All arguments must be integers"
  )

  # Test that all arguments are greater than 0
  expect_error(
    generate_strings(
      n_groups = base_parameters$n_groups,
      n_individuals = base_parameters$n_individuals,
      n_calls = 0,  # 0 calls should raise an error
      string_length = base_parameters$string_length,
      group_information = base_parameters$group_information,
      individual_information = base_parameters$individual_information
    ),
    "All arguments must be greater than 0"
    
    # Test that strings are generated with valid structure argument
    expect_error(
      generate_strings(
        n_groups = base_parameters$n_groups,
        n_individuals = base_parameters$n_individuals,
        n_calls = base_parameters$n_calls,
        string_length = base_parameters$string_length,
        group_information = base_parameters$group_information,
        individual_information = base_parameters$individual_information,
        random_variation = base_parameters$random_variation,
        alphabet = base_parameters$alphabet, 
        string_structure = "GI-GI-GI" # Invalid string structure
      ),
      "String structure argument must use one of the 16 valid string structures"
  )
})
