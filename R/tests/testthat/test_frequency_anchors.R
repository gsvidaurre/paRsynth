# Author: Raneem Samman
# Date created: November 15, 2024

source("R/frequency_anchors.R")
source("R/parsons_code.R")
source("R/generate_strings.R")
# Load necessary libraries
library(testthat)
library(tidyverse)

# # Helper function to perform Parsons Code conversion
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
    string_structure_col = "String_Structure",
    mapping = list("A" = "up", "B" = "down", "C" = "constant")
  )
}

# 1. Unit test to check that the generation of a data frame with multiple rows
test_that("The function generates a data frame with multiple rows", {

  generated_strings <- generate_strings(
    n_groups = 2,
    n_individuals = 5,
    n_calls = 10,
    string_length = 40,
    group_information = 8,
    individual_information = 2,
    random_variation = 4,
    alphabet = c("A", "B", "C"),
    string_structure = "GI-II-RV-GI"
  )
  # convert strings to parsons code
  Conversion <- apply_parsons_code(generated_strings)

  # Call the function with test df
  anchors <- frequency_anchors(
    df = Conversion,
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
  # glimpse(anchors)

  # Test that the returned df (anchors) has the expected columns
  expect_true(all(c("Group", "Individual", "Call_ID", "Call", "Parsons_Code", "String_structure") %in% colnames(anchors)))
  expect_true(any(grepl("Frequency", colnames(anchors)))) # At least one column with the word "Frequency" in the name

  # Test the number of frequency columns, which should be equal to the character string length plus 1 for the starting frequency
  expect_equal(length(grep("Frequency", names(anchors))), nchar(generated_strings$Call[1]) + 1)

})

# 2. Unit test to check that frequency directions shifts correctly in continuous_trajectory mode
test_that("This function computes frequency anchors correctly in continuous trajectory", {

  starting_freq <- 4000
  freq_shift <- 1000

  # Generate synthetic test data
  generated_strings <- generate_strings(
    n_groups = 2,
    n_individuals = 5,
    n_calls = 10,
    string_length = 40,
    group_information = 8,
    individual_information = 2,
    random_variation = 4,
    alphabet = c("A", "B", "C"),
    string_structure = "GI-II-RV-GI"
  )
  # convert strings to parsons code
  Conversion <- apply_parsons_code(generated_strings)

  # Call the function with test df
  anchors <- frequency_anchors(
    df = Conversion,
    parsons_col = "Call_Parsons_Code",
    group_id_col = "Group",
    individual_id_col = "Individual",
    call_id_col = "Call_ID",
    call_string_col = "Call",
    string_structure_col = "String_structure",
    starting_frequency = starting_freq,
    frequency_shift = freq_shift,
    section_transition = "continuous_trajectory"
  )

  for (row in seq_len(nrow(anchors))) {
    # Extract the frequencies
    frequencies <- as.numeric(anchors[row, grep("^Frequency", names(anchors))])
    parsons_code <- as.character(anchors$Parsons_Code[row])
    directions <- strsplit(parsons_code, "-")[[1]]

    # Check that expected lengths are correct
    expect_equal(
      length(frequencies), length(directions) + 1,
      info = paste("Row", row, "frequency length mismatch with Parsons code")
    )

    # Check that start frequency is correct
    expect_equal(
      frequencies[1], starting_freq,
      info = paste("Row", row, "should start at starting_freq")
    )

    # get expected frequencies
    expected_freqs <- numeric(length(frequencies))
    expected_freqs[1] <- starting_freq

    for (i in seq_along(directions)) {
      shift <- switch(directions[i],
                      "up" = freq_shift,
                      "down" = -freq_shift,
                      "constant" = 0,
                      stop(paste("Invalid direction in row", row, ":", directions[i])))

      candidate_freqs <- expected_freqs[i] + shift
      if (candidate_freqs <= 0) {
        candidate_freqs <- freq_shift
      }
      expected_freqs[i + 1] <- candidate_freqs
    }

    # check if the generated frequencies match the expected frequencies
    expect_equal(
      frequencies, expected_freqs,
      info = paste("Row", row, "frequencies mismatch expected trajectory")
    )
  }
})

# 3. Unit test to check that there are no negative or zero frequencies created when generating many calls
test_that("The function corrects negative or zero frequencies", {
  # generate strings
  generated_strings <- generate_strings(
    n_groups = 2,
    n_individuals = 5,
    n_calls = 10,
    string_length = 40,
    group_information = 8,
    individual_information = 2,
    random_variation = 4,
    alphabet = c("A", "B", "C"),
    string_structure = "GI-II-RV-GI"
  )
  # convert strings to parsons code
  Conversion <- apply_parsons_code(generated_strings)

  # generate frequency anchors with the test df
  anchors <- frequency_anchors(
    df = Conversion,
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

# 4. Unit test to check that the function works in starting_frequency mode
test_that("In starting_frequency mode, each section resets to start freq and applies directions shifts correctly", {
  starting_freq <- 4000
  freq_shift <- 1000

  all_string_structures <- c(
    "GI-II-RV", "GI-RV-II", "II-GI-RV", "II-RV-GI", "RV-II-GI", "RV-GI-II",
    "GI-II-RV-GI", "GI-RV-II-GI", "II-GI-RV-II", "II-RV-GI-II",
    "GI-RV-GI", "II-RV-II", "GI-RV", "RV-GI", "II-RV", "RV-II"
  )
  for (test_structure in all_string_structures) {
    # generate strings
    generated_strings <- generate_strings(
      n_groups = 2,
      n_individuals = 3,
      n_calls = 5,
      string_length = 16,
      group_information = 8,
      individual_information = 2,
      random_variation = 4,
      alphabet = c("A", "B", "C"),
      string_structure = test_structure
    )
    parsons_results <- apply_parsons_code(generated_strings)

    # Run the frequency_anchors function
    anchors <- frequency_anchors(
      df = parsons_results,
      parsons_col = "Call_Parsons_Code",
      group_id_col = "Group",
      individual_id_col = "Individual",
      call_id_col = "Call_ID",
      call_string_col = "Call",
      string_structure_col = "String_structure",
      starting_frequency = starting_freq,
      frequency_shift = freq_shift,
      section_transition = "starting_frequency"
    )

    # Get frequencies from output
    freq_cols <- grep("^Frequency", names(anchors), value = TRUE)
    freq_cols

    # Check that the first frequency is the starting frequency
    for (i in 1:nrow(parsons_results)) {
      returned_freqs <- as.numeric(anchors[i, freq_cols])
      returned_freqs

      # fetch the string structure provided
      string_str <- parsons_results$String_structure[i]
      string_str
      # use the fetched string structure to know how the string is ordered (how the sections are ordered)
      section_order <- switch(string_str,
          "GI-II-RV" = c("group_complete", "individual_complete", "random_variation"),
          "GI-RV-II" = c("group_complete", "random_variation", "individual_complete"),
          "II-GI-RV" = c("individual_complete", "group_complete", "random_variation"),
          "II-RV-GI" = c("individual_complete", "random_variation", "group_complete"),
          "RV-II-GI" = c("random_variation", "individual_complete", "group_complete"),
          "RV-GI-II" = c("random_variation", "group_complete", "individual_complete"),
          "GI-II-RV-GI" = c("group_head", "individual_complete", "random_variation", "group_tail"),
          "GI-RV-II-GI" = c("group_head", "random_variation", "individual_complete", "group_tail"),
          "II-GI-RV-II" = c("individual_head", "group_complete", "random_variation", "individual_tail"),
          "II-RV-GI-II" = c("individual_head", "random_variation", "group_complete", "individual_tail"),
          "GI-RV-GI" = c("group_head", "random_variation", "group_tail"),
          "II-RV-II" = c("individual_head", "random_variation", "individual_tail"),
          "GI-RV" = c("group_complete", "random_variation"),
          "RV-GI" = c("random_variation", "group_complete"),
          "II-RV" = c("individual_complete", "random_variation"),
          "RV-II" = c("random_variation", "individual_complete"),
          stop(paste("Invalid string structure:", string_str))
        )
      section_order

      sections <- c("global_head", section_order, "global_tail")
      sections

      # Get this row's frequencies and start index
      freq_idx <- 2  # Skip first Frequency1 (which is the base starting freq)

      cat(sprintf("Row %d: String structure = %s\n", i, string_str))
      cat(sprintf("Section order: %s\n", paste(sections, collapse = " -> ")))

        # Check that the first frequency in the each row is the starting frequency
      expect_equal(returned_freqs[1], starting_freq,
                   info = paste("Row", i, "First frequency should be:", starting_freq))
      cat(sprintf("\n Row: %s | Expected: %s | Got: %s \n",
                  i, starting_freq, returned_freqs[1]))

      for (section_num in seq_along(sections)) {
        section <- sections[section_num]
        section_col <- paste0(section, "_parsons_code")
        section_col
        # Skip if the section isn't present
        if (!section_col %in% colnames(parsons_results)) next

        section_code <- parsons_results[[section_col]][[i]]
        directions <- strsplit(section_code, "-")[[1]]
        if (length(directions) == 0) next  # skip empty sections

        # Calculate expected frequency from direction
        first_direction <- directions[1]
        expected_first_freq <- switch(first_direction,
                                      "up" = starting_freq + freq_shift,
                                      "down" = starting_freq - freq_shift,
                                      "constant" = starting_freq,
                                      stop("Invalid direction in Parsons code"))

        observed_first_freq <- returned_freqs[freq_idx]

        expect_equal(observed_first_freq, expected_first_freq,
                     info = paste("Row", i, "| Section:", section,
                                  "| First direction:", first_direction,
                                  "| Expected:", expected_first_freq,
                                  "| Got:", observed_first_freq))

        cat(sprintf("Row %s | Section: %s | First direction: %s | Expected: %s | Got: %s\n",
                    i, section, first_direction, expected_first_freq, observed_first_freq))

        freq_idx <- freq_idx + length(directions)
      }
      cat("\n")
    }
  }
  cat("\nall string structures were tested successuflly\n")
})


# 5. Unit test to check that frequency_anchors handles different string structures correctly
test_that("frequency_anchors handles different string_structures correctly", {
  string_structures <- c(
    "GI-II-RV",
    "II-RV-GI",
    "GI-II-RV-GI",
    "II-RV-GI-II",
    "GI-RV",
    "RV-GI"
  )

  for (structure in string_structures) {
    cat(sprintf("\n=== Testing structure: %s ===\n", structure))

    # Generate strings using the correct function name
    generated_strings <- generate_strings(
      n_groups = 2,
      n_individuals = 2,
      n_calls = 2,
      string_length = 18,
      group_information = 4,
      individual_information = 2,
      random_variation = 4,
      alphabet = c("A", "B", "C"),
      string_structure = structure
    )

    # Use apply_parsons_code instead of parsons_code
    parsons_df <- apply_parsons_code(generated_strings)

    # Apply frequency anchors
    result <- frequency_anchors(
      df = parsons_df,
      parsons_col = "Call_Parsons_Code",
      group_id_col = "Group",
      individual_id_col = "Individual",
      call_id_col = "Call_ID",
      call_string_col = "Call",
      string_structure_col = "String_structure",
      starting_frequency = 4000,
      frequency_shift = 1000,
      section_transition = "starting_frequency"
    )

    # Test 1: Ensure the string structure was correctly included in output
    expect_true("String_structure" %in% names(result),
                info = paste("String_structure column missing for", structure))
    expect_equal(result$String_structure[1], structure,
                 info = paste("String structure mismatch for", structure))

    # Test 2: Check at least one frequency column exists
    freq_cols <- grep("^Frequency", names(result), value = TRUE)
    expect_true(length(freq_cols) > 0,
                info = paste("No frequency columns found for", structure))

    # Test 3: Check that frequency column count matches Parsons code length
    # Use the correct column name for the full parsons code
    parsons_code <- as.character(result$Parsons_Code[1])
    cat(sprintf("Full Parsons code: %s\n", parsons_code))

    # Split and count directions, removing empty strings
    directions <- strsplit(parsons_code, "-")[[1]]
    directions <- directions[directions != ""]  # Remove empty strings
    cat(sprintf("Directions found: %s (count: %d)\n",
                paste(directions, collapse = ", "), length(directions)))
    cat(sprintf("Frequency columns: %s (count: %d)\n",
                paste(freq_cols, collapse = ", "), length(freq_cols)))

    # The frequency columns should equal directions + 1 (starting frequency)
    expect_equal(length(freq_cols), length(directions) + 1,
                 info = paste("Frequency count mismatch for structure", structure,
                              "- Expected:", length(directions) + 1,
                              "Got:", length(freq_cols),
                              "Parsons code:", parsons_code))

    cat(sprintf("✓ Structure %s passed all tests\n", structure))
  }

  cat("\nAll string structures tested successfully!\n")
})

