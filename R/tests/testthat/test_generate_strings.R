# G.A. Juarez
# 4 Dec 2024

# 1. Unit test to check string length
test_that("The function generates strings that have the correct length", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("lubridate")
  
  # Just for code development
  library(tidyverse)
  library(lubridate)
  library(testthat)
  
  # Define parameters
  group_information <- 8
  individual_information <- 2
  random_variation <- 4
  global_head <- 4
  string_length <- group_information + individual_information + random_variation + (global_head * 2)
  n_calls <- 10
  
  # Call the function using parameters
  generated_strings <- generate_strings(n_groups = 2, n_individuals = 5, n_calls = n_calls, string_length = string_length, group_information = group_information, individual_information = individual_information, random_variation = random_variation)
  # glimpse(generated_strings)
  # View(generated_strings)
  
  # Extract the length of each generated string
  string_lengths <- unique(nchar(generated_strings$Call))
  # glimpse(string_lengths)
  # View(string_lengths)
  
  # Check that each string has the correct length by comparing previously defined parameter
  expect_equal(string_length, string_lengths,
               info = "Not all generated strings have the expected length.")
})

# 2. Unit test to check that correct number of string were generated
test_that("The function generates the correct number of strings", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("lubridate")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  
  # Define parameters
  n_groups <- 2
  n_individuals <- 5
  group_information <- 8
  individual_information <- 2
  random_variation <- 4
  global_head <- 4
  string_length <- group_information + individual_information + random_variation + (global_head * 2)
  n_calls <- 10
  
  # Call the function using parameters
  generated_strings <- generate_strings(n_groups = n_groups, n_individuals = n_individuals, n_calls = n_calls, string_length = string_length, group_information = group_information, individual_information = individual_information, random_variation = random_variation)
  # glimpse(generated_strings)
  
  # Get the number of generated calls
  generated_calls <- nrow(generated_strings)
  
  # Get the number of expected calls
  n_expected_calls <- n_calls*n_groups*n_individuals
  
  # Check that the number of generated calls equal the number of expected calls
  expect_true(generated_calls == n_expected_calls,
              info = "Not all generated calls have the expected number.")
})

# 3. Unit test to check that the number of groups and individuals is correct
test_that("The function generates the number of groups and individuals", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("lubridate")
  withr::local_package("data.table")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  # library(data.table)
  # library(stringr)
  
  # Define parameters
  n_groups <- 2
  n_individuals <- 5
  group_information <- 8
  individual_information <- 2
  random_variation <- 4
  global_head <- 4
  string_length <- group_information + individual_information + random_variation + (global_head * 2)
  n_calls <- 10
  
  # Call the function using parameters
  generated_strings <- generate_strings(n_groups = n_groups, n_individuals = n_individuals, n_calls = n_calls, string_length = string_length, group_information = group_information, individual_information = individual_information, random_variation = random_variation)
  
  # Find the expected number of rows
  expected_rows <- n_groups*n_individuals*n_calls
  
  # Check if the number of rows created is correct
  expect_equal(nrow(generated_strings), expected_rows)
  cat("expected rows:", expected_rows, "\n")
  cat("expected n_calls", n_calls, "\n")
  
  # Check if n_groups created is correct:
  expect_equal(length(unique(generated_strings$Group)), n_groups)
  cat("expected n_groups:", n_groups, "\n")
  cat("unique n_groups in test", unique(generated_strings$Group), "\n")
  cat("number of n_groups in test", length(unique(generated_strings$Group)), "\n")
  
  # Check if number of individuals created is correct
  # Use equal for the number of individuals (unique ones) in the individuals column
  cat("expected n_individuals:", n_individuals, "\n")
  for (group in 1:n_groups) {
    test_group_individuals <- unique(generated_strings[generated_strings$Group == group, "Individual"])
    expect_equal(length(test_group_individuals), n_individuals)
  }
  cat("unique n_individuals in test", (test_group_individuals), "\n")
  cat("number of n_individuals in test", length(test_group_individuals), "\n")
})

# 4. Unit test to check that the number of characters in each string devoted to group information, individual information, global head and global tail are correct
test_that("The function generates character-based vocal strings per catergory", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("lubridate")
  withr::local_package("data.table")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  # library(data.table)
  # library(stringr)
  
  # Define parameters
  n_groups <- 2
  n_individuals <- 5
  group_information <- 8
  individual_information <- 2
  random_variation <- 4
  global_head <- 4
  string_length <- group_information + individual_information + random_variation + (global_head * 2)
  n_calls <- 10
  
  # Call the function using parameters
  generated_strings <- generate_strings(n_groups = n_groups, n_individuals = n_individuals, n_calls = n_calls, string_length = string_length, group_information = group_information, individual_information = individual_information, random_variation = random_variation)
  
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
    expect_equal(individual_middle, generated_strings$Individual_middle[i])
  }
  
})

# 5. Unit test to check that each individual is assigned to only one social group
test_that("The function generates strings of individuals assigned to one social group", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("lubridate")
  withr::local_package("data.table")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  # library(data.table)
  # library(stringr)
  
  # Define parameters
  group_information <- 8
  individual_information <- 2
  random_variation <- 4
  global_head <- 4
  string_length <- group_information + individual_information + random_variation + (global_head * 2)
  n_calls <- 10
  
  # Call the function using parameters
  generated_strings <- generate_strings(n_groups = 2, n_individuals = 5, n_calls = n_calls, string_length = string_length, group_information = group_information, individual_information = individual_information, random_variation = random_variation)
  
  # Find each unique individuals and groups
  indivs <- unique(generated_strings$Individual)
  groups <- unique(generated_strings$Group)
  
  # For each individual, how many unique groups are they assigned to
  group_assign <- unlist(sapply(1:length(groups), function(i){
    
    # Filter by group
    tmp <- generated_strings %>%
      dplyr::filter(Group == groups[i])
    
    # Then filter by individual
    res <- sapply(1:length(groups), function(j){
      
      return(
        tmp %>%
          dplyr::filter(Individual == indivs[j]) %>%
          dplyr::pull(Group) %>%
          unique() %>% 
          length()
      )
      
    })
    
    return(res)
    
  }, simplify = FALSE))
  
  # Check that each individual is assigned to only one social group
  expect_true(all(group_assign == 1))
})
