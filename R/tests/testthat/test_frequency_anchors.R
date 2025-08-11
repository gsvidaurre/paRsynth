# Author: Raneem Samman
# Date created: November 15, 2024

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
source(file.path(testing_path, "parsons_code.R"))
source(file.path(testing_path, "frequency_anchors.R"))

# Helper function to generate test data
generate_test_data <- function() {
  Global_head <- "A"
  Group_head <- "BB"
  Individual_head <- "C"
  Individual_tail <- "A"
  Individual_complete <- "CA"
  Group_complete <- "BBCC"
  Random_variation <- "BA"
  Group_tail <- "CC"
  Global_tail <- "B"
  String_structure <- "GI-II-RV-GI"
  mapping <- list("A" = "up", "B" = "down", "C" = "constant")
  
  data.frame(
    Group_ID = 1,
    Individual_ID = 1,
    Call_ID = 1,
    Call = paste(Global_head, Group_head, Individual_complete, Random_variation, Group_tail, Global_tail, sep = ""),
    Global_head = Global_head,
    Group_head = Group_head,
    Individual_head = Individual_head,
    Individual_tail = Individual_tail,
    Individual_complete = Individual_complete,
    Group_complete = Group_complete,
    Random_variation = Random_variation,
    Group_tail = Group_tail,
    Global_tail = Global_tail,
    String_structure = String_structure
  )
}
# Helper function to perform Parsons Code conversion
apply_parsons_code <- function(generated_strings) {
  parsons_code(
    generated_strings,
    string_col = "Call",
    global_head_col = "Global_head",
    group_head_col = "Group_head",
    individual_head_col = "Individual_head",
    individual_tail_col = "Individual_tail",
    individual_complete_col = "Individual_complete",
    group_complete_col = "Group_complete",
    random_variation_col = "Random_variation",
    group_tail_col = "Group_tail",
    global_tail_col = "Global_tail",
    string_structure_col = "String_structure",
    mapping = list("A" = "up", "B" = "down", "C" = "constant")
  )
}

# 1. Unit test to check that the generation of a data frame with multiple rows
test_that("The function generates a data frame with multiple rows", {
  generated_strings <- generate_test_data()
  # convert strings to parsons code
  Conversion <- apply_parsons_code(generated_strings)
  
  # Call the function with test df
  result <- frequency_anchors(
    df = Conversion, 
    parsons_col = "Call_Parsons_Code", 
    group_id_col = "Group_ID", 
    individual_id_col = "Individual_ID", 
    call_id_col = "Call_ID", 
    call_string_col = "Call",
    string_structure_col = "String_structure",
    starting_frequency = 4000, 
    frequency_shift = 1000, 
    section_transition = "continuous_trajectory"
  )
  # glimpse(result)

  # Test that the returned df (result) has the expected columns
  expect_true(all(c("Group", "Individual", "Call_ID", "Call", "Parsons_Code") %in% colnames(result)))
  expect_true(any(grepl("Frequency", colnames(result)))) # At least one column with the word "Frequency" in the name

  # Test the number of frequency columns, which should be equal to the character string length plus 1 for the starting frequency
  expect_equal(length(grep("Frequency", names(result))), (nchar(generated_strings$Call) + 1))

})

# 2. Unit test to check that frequency directions shift
test_that("This function shifts up, constant, and down directions frequency correctly", {
  # generate test data
  generated_strings <- generate_test_data()
  # convert strings to parsons code
  Conversion <- apply_parsons_code(generated_strings)
  
  # Call the function with test df
  result <- frequency_anchors(
    df = Conversion,
    parsons_col = "Call_Parsons_Code",
    group_id_col = "Group_ID",
    individual_id_col = "Individual_ID",
    call_id_col = "Call_ID",
    call_string_col = "Call",
    string_structure_col = "String_structure",
    starting_frequency = 4000,
    frequency_shift = 1000,
    section_transition = "continuous_trajectory"
  )
  # Check the first row's frequencies (the starting frequency is 4 kHz)
  expected_anchors <- c(4, 5, 4, 3, 3, 4, 3, 4, 4, 4, 3) * 1000
  expect_equal(as.vector(t(result[, grep("Frequency", names(result))])), expected_anchors)
})

# 3. Unit test to check that there are no negative or zero frequencies created when generating many calls
test_that("The function corrects negative or zero frequencies", {
  # generate strings
  generated_strings <- suppressWarnings(generate_strings(
    n_groups = 2,
    n_individuals = 5,
    n_calls = 10,
    string_length = 40,
    group_information = 8,
    individual_information = 2,
    random_variation = 4,
    alphabet = c("A", "B", "C"),
    string_structure = "GI-II-RV-GI"
  ))
  # convert strings to parsons code
  parsons_results <- parsons_code(
    generated_strings,
    string_col = "Call",
    global_head_col = "Global_head",
    group_head_col = "Group_head",
    individual_head_col = "Individual_head",
    individual_tail_col = "Individual_tail",
    individual_complete_col = "Individual_complete",
    group_complete_col = "Group_complete",
    random_variation_col = "Random_variation",
    group_tail_col = "Group_tail",
    global_tail_col = "Global_tail",
    string_structure_col = "String_structure",
    mapping = list("A" = "up", "B" = "down", "C" = "constant")
  )
  # generate frequency anchors with the test df
  anchors <- frequency_anchors(
    df = parsons_results,
    parsons_col = "Call_Parsons_Code",
    group_id_col = "Group",
    individual_id_col = "Individual",
    call_id_col = "Call_ID",
    call_string_col = "Call",
    string_structure_col = "String_structure",
    starting_frequency = 4000,
    frequency_shift = 1000,
    section_transition = "continuous_trajectory"
  )

  freq_cols <- anchors[, grep("Frequency", names(anchors))]

  # There should be no zero or negative values in the frequency anchors
  expect_false(any(freq_cols[freq_cols <= 0]))

})
