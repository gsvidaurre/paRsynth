# Author: G.A. Juarez and Raneem Samman
# Date created: May 22, 2024

rm(list = ls())

# make a list of packages to install
pkgs <- c("testthat", "soundgen", "dplyr", "stringr", "rlang", "tidyverse", "lubridate", "pbapply", "data.table")

# check if the packages are installed, if not, install them
for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}
# load the packages (for software development purposes)
# lapply(pkgs, library, character.only = TRUE)

# Change the path for testing to reflect where the package is installed on your local machine
# testing_path <- "/Users/raneemsamman/Documents/GitHub/paRsynth/R" #raneem's
testing_path <- "~/Desktop/BIRDS/GitHub_repos/paRsynth/R" #alexandra's

# Change the desktop path to reflect where temporary directories for testing will be created to save files generated during testing
desktop_path <- "~/Desktop"

# Load the paRsynth functions that will be tested below
source(file.path(testing_path, "generate_strings.R"))

# Define parameters
base_parameters <- list(
  group_information <- 8,
  individual_information <- 2,
  random_variation <- 4,
  global_head <- 4,
  string_length <- group_information + individual_information + random_variation + (global_head * 2),
  n_calls <- 1,
  n_groups <- 2,
  n_individuals <- 2,
  alphabet <- c("A", "B", "C"),
  string_structure <- "GI-II-RV-GI"
)

# Call the function using parameters
generated_strings <- generate_strings(
  n_groups = n_groups,
  n_individuals = n_individuals,
  n_calls = n_calls,
  string_length = string_length,
  group_information = group_information,
  individual_information = individual_information,
  random_variation = random_variation,
  alphabet = alphabet,
  string_structure = string_structure
)

# Create a vector of all valid structures
valid_structures <- c(
  "GI-II-RV-GI", "GI-RV-II-GI",
  "II-GI-RV-II", "II-RV-GI-II",
  "GI-II-RV", "GI-RV-II",
  "II-GI-RV", "II-RV-GI",
  "RV-II-GI", "RV-GI-II",
  "GI-RV-GI", "II-RV-II",
  "GI-RV", "RV-GI",
  "RV-II", "II-RV")

# Loop the function to create calls of each valid structure
generated_structures <- lapply(valid_structures, function(x){
  generate_strings(
    n_groups = n_groups,
    n_individuals = n_individuals,
    n_calls = n_calls,
    string_length = string_length,
    group_information = group_information,
    individual_information = individual_information,
    random_variation = random_variation,
    alphabet = alphabet,
    string_structure = x
  )
})

# 1. Unit test to check string length
test_that("The function generates strings that have the correct length", {
  
  # Check that the number of characters in each Call equal the number of string_length
  expect_true(unique(nchar(generated_strings$Call) == string_length),
              info = "Not all generated strings have the expected length.")
  cat("expected string_length:", string_length, "\n")
  cat("unique string_length in test", unique(nchar(generated_strings$Call)), "\n")
})

# 2. Unit test to check that correct number of string were generated
test_that("The function generates the correct number of strings", {  
  
  # Get the number of generated calls
  n_generated_calls <- nrow(generated_strings)

  # Get the number of expected calls
  n_expected_calls <- n_calls*n_groups*n_individuals

  # Check that the number of generated calls equal the number of expected calls
  expect_true(n_generated_calls == n_expected_calls,
              info = "Not all generated calls have the expected number.")
  cat("expected number of calls", n_expected_calls, "\n")
  cat("number of generated calls in test:", n_generated_calls, "\n")
  
})

# 3. Unit test to check that the number of groups and individuals is correct
test_that("The function generates the number of groups and individuals", {
  # Check if the number of rows created is correct
  expected_rows <- n_groups*n_individuals*n_calls
  expect_equal(nrow(generated_strings), expected_rows)
  cat("expected rows:", expected_rows, "\n")

  # Check if n_groups created is correct:
  expect_equal(length(unique(generated_strings$Group)), n_groups)

  cat("expected n_groups:", n_groups, "\n")
  cat("unique n_groups in test", unique(generated_strings$Group), "\n")
  cat("number of n_groups in test", length(unique(generated_strings$Group)), "\n")

  # Check if number of individuals created is correct
  expected_individuals <- n_groups*n_individuals
  expect_equal(length(unique(generated_strings$Individual))*n_groups, expected_individuals)
  cat("expected n_individuals:", expected_individuals, "\n")
})

# 4. Unit test to check that the number of characters in each string devoted to group information, individual information, global head and global tail are correct
test_that("The function generates character-based vocal strings per catergory", {
  # Get all the calls generated
  for (i in 1:nrow(generated_strings)) {
    current_call <- generated_strings$Call[i]
    cat("-------- Testing current call: --------", current_call, "\n")
    
    # Check if the total length of the string matches the expected string length
    expect_equal(nchar(current_call), string_length)
    
    # Calculate the length of the global head and tail
    expected_head_tail_length <- (string_length - group_information - individual_information - random_variation) / 2
    
    # Get the global head and check its length
    global_head <- stringr::str_sub(current_call, 1, expected_head_tail_length)
    cat("Global head string is:", global_head, "\n")
    cat("Expected global head length is", expected_head_tail_length, "\n")
    expect_equal(nchar(global_head), expected_head_tail_length)
    expect_equal(global_head, generated_strings$Global_head[i])
    
    # Get the global tail and check its length
    global_tail <- stringr::str_sub(current_call, -expected_head_tail_length)
    cat("Global tail string is:", global_tail, "\n")
    cat("Expected global tail length is", expected_head_tail_length, "\n")
    expect_equal(nchar(global_tail), expected_head_tail_length)
    expect_equal(global_tail, generated_strings$Global_tail[i])
    
    # Calculate the half-length of the group membership
    half_group_length <- group_information / 2
    
    # get the group middle (split into two parts)
    group_middle_head <- stringr::str_sub(current_call,
                                          expected_head_tail_length + 1,
                                          expected_head_tail_length + half_group_length) # nolint
    
    group_middle_tail <- stringr::str_sub(current_call,
                                          expected_head_tail_length + half_group_length + individual_information + random_variation + 1, # nolint
                                          expected_head_tail_length + half_group_length + individual_information + random_variation + half_group_length) # nolint
    
    cat("Group middle head string is:", group_middle_head, "\n")
    cat("Expected group middle head length is", half_group_length, "\n")
    expect_equal(nchar(group_middle_head), half_group_length)
    expect_equal(group_middle_head, generated_strings$Group_head[i])
    
    cat("Group middle tail string is:", group_middle_tail, "\n")
    cat("Expected group middle tail length is", half_group_length, "\n")
    expect_equal(nchar(group_middle_tail), half_group_length)
    expect_equal(group_middle_tail, generated_strings$Group_tail[i])
    
    # Get the individual middle section
    individual_middle <- stringr::str_sub(current_call, # nolint
                                          expected_head_tail_length + half_group_length + 1, # nolint
                                          expected_head_tail_length + half_group_length + individual_information) # nolint
    cat("Individual middle string is:", individual_middle, "\n")
    cat("Expected individual middle length is", individual_information, "\n")
    expect_equal(nchar(individual_middle), individual_information)
    expect_equal(individual_middle, generated_strings$Individual_complete[i])
  }
})

# 5. Unit test to check that each individual is assigned to correct number of calls per group
test_that("The function generates # of calls per individuals per social group correctly", {
  # Ensure each individual appears in the correct number of calls per group
  call_count_per_group <- generated_strings %>%
    dplyr::group_by(Group, Individual) %>%
    dplyr::summarize(call_count = n()) %>%
    dplyr::arrange(Group, Individual)

  # Check that each individual has exactly `n_calls` in each group
  expected_call_count <- n_calls
  expect_true(all(call_count_per_group$call_count == expected_call_count), 
              info = paste("Each individual should have", expected_call_count, "calls per group."))
  # Optionally, check that each individual appears only once per call per group
  # Check for duplicates in (Group, Individual) pairs within each call
  duplicate_checks <- generated_strings %>%
    dplyr::group_by(Group, Individual, Call_ID) %>%
    dplyr::tally() %>%
    dplyr::filter(n > 1)

  # Expect no duplicates for any (Group, Individual) in a given Call_ID
  expect_true(nrow(duplicate_checks) == 0,
              info = "Some individuals are assigned to the same group more than once per call.")
  
  cat("number of extra unwanted group/individual information:", nrow(duplicate_checks), "\n")
  
})

# 6. Unit test to check correct string structure
test_that("The function generates strings that have the correct given string structure", {
  
  # combine datasets with different different string structure into one dataframe
  all_generated_structures <- data.table::rbindlist(generated_structures)
  # glimpse(generated_structures)
  
  observed_structures <- unique(all_generated_structures$String_structure)
  
  expect_identical(valid_structures, observed_structures)
  
  cat("Expected Valid Structures:", valid_structures, "\n")
  cat("String structures generated:", observed_structures, "\n")
  
 })

# 7. Unit test to check that `generate_structures` throws an error for invalid string structures input
test_that("The function outputs an error when given invalid string structure", {
  
  # Create a vector of all valid structures
  invalid_structures <- c(
    "GI-GI-GI", "ABC", "123", "***")
  
  # Loop the function to create calls of each valid structure
  expect_error(lapply(invalid_structures, function(y){
    generate_strings(
      n_groups = n_groups,
      n_individuals = n_individuals,
      n_calls = n_calls,
      string_length = string_length,
      group_information = group_information,
      individual_information = individual_information,
      random_variation = random_variation,
      alphabet = alphabet,
      string_structure = y
    )
  }))
  
  cat("Error thrown for these invalid structures:", invalid_structures, "\n")
  
})
