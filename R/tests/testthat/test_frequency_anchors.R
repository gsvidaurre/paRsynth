# Author: Raneem Samman
# Date created: November 15, 2024

# Helper function to generate test data
generate_test_data <- function() {
  Global_head <- "AABA"
  Group_head <- "BBCC"
  Individual_middle <- "CCBA"
  Random_variation <- "BA"
  Group_tail <- "BBAC"
  Global_tail <- "BBAA"
  
  data.frame(
    Group_ID = 1,
    Individual_ID = 1,
    Call_ID = 1,
    Call = paste(Global_head, Group_head, Individual_middle, Random_variation, Group_tail, Global_tail, sep = ""),
    Global_head = Global_head,
    Group_head = Group_head,
    Individual_middle = Individual_middle,
    Random_variation = Random_variation,
    Group_tail = Group_tail,
    Global_tail = Global_tail
  )
}
# Helper function to perform Parsons Code conversion
apply_parsons_code <- function(generated_strings) {
  parsons_code(
    generated_strings,
    string_col = "Call",
    global_head_col = "Global_head",
    group_head_col = "Group_head",
    individual_middle_col = "Individual_middle",
    random_variation_col = "Random_variation",
    group_tail_col = "Group_tail",
    global_tail_col = "Global_tail",
    mapping = list("A" = "up", "B" = "down", "C" = "constant"),
    alphabet = c("A", "B", "C")
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
test_that("This function shifts up, constant, and down directions frequency correctly in contineous trajectory", {
  # generate test data
  generated_strings <- generate_strings(
    n_groups = 2,
    n_individuals = 5,
    n_calls = 10,
    string_length = 40,
    group_information = 8,
    individual_information = 2,
    random_variation = 4
  )
  # convert strings to parsons code
  Conversion <- apply_parsons_code(generated_strings)
  
  # Call the function with test df
  result <- frequency_anchors(
    df = Conversion,
    parsons_col = "Call_Parsons_Code",
    group_id_col = "Group",
    individual_id_col = "Individual",
    call_id_col = "Call_ID",
    call_string_col = "Call",
    starting_frequency = 4000,
    frequency_shift = 1000,
    section_transition = "continuous_trajectory"
  )

  # make parsons code columns to character strings
  section_parsons_cols <- c(
    global_head = "Global_Head_Parsons_Code",
    group_head = "Group_Head_Parsons_Code",
    individual_middle = "Individual_Middle_Parsons_Code",
    random_variation = "Random_Variation_Parsons_Code",
    group_tail = "Group_Tail_Parsons_Code",
    global_tail = "Global_Tail_Parsons_Code"
  )

  frequencies <- as.numeric(result[1, grep("Frequency", names(result))])
  # Confirm it starts at 4000
  expect_equal(frequencies[1], 4000)

  # All diffs must be in {+1000, 0, -1000}
  diffs <- diff(frequencies)
  expect_true(all(diffs %in% c(-1000, 0, 1000)))

  for (section in names(section_parsons_cols)) {
        code_col <- section_parsons_cols[[section]]
        directions <- strsplit(parsons_results[[code_col]][1], "-")[[1]]

        if (length(directions) == 0 || is.na(directions[1])) next

        # Section starts at 4000
        first_direction <- directions[1]
        # Compute expected first frequency based on direction of the first character of the section
        expected_first_freq <- switch(first_direction,
                                      up = starting_freq + freq_shift,
                                      down = max(starting_freq - freq_shift, freq_shift),
                                      constant = starting_freq,
                                      stop("Invalid direction in Parsons code"))

        observed_first_freq <- returned_freqs[freq_idx]

        # Check that the section's first frequency is correctly computed
        expect_equal(observed_first_freq, expected_first_freq,
                    info = paste("Section:", section,
                                  "| First direction:", first_direction,
                                  "| Expected:", expected_first_freq,
                                  "| Got:", observed_first_freq))
        cat(sprintf("Section: %s | First direction: %s | Expected: %s | Got: %s\n",
        section, first_direction, expected_first_freq, observed_first_freq))

        # Advance freq_idx to skip over rest of section (length of directions)
        freq_idx <- freq_idx + length(directions)
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
    random_variation = 4
  )
  # convert strings to parsons code
  parsons_results <- parsons_code(
    generated_strings,
    string_col = "Call",
    global_head_col = "Global_head",
    group_head_col = "Group_head",
    individual_middle_col = "Individual_middle",
    random_variation_col = "Random_variation",
    group_tail_col = "Group_tail",
    global_tail_col = "Global_tail",
    list("A" = "up", "B" = "down", "C" = "constant")
  )
  # generate frequency anchors with the test df
  anchors <- frequency_anchors(
    df = parsons_results,
    parsons_col = "Call_Parsons_Code",
    group_id_col = "Group",
    individual_id_col = "Individual",
    call_id_col = "Call_ID",
    call_string_col = "Call",
    starting_frequency = 4000,
    frequency_shift = 1000,
    section_transition = "continuous_trajectory"
  )

  freq_cols <- anchors[, grep("Frequency", names(anchors))]

  # There should be no zero or negative values in the frequency anchors
  expect_false(any(freq_cols[freq_cols <= 0]))

})

test_that("In starting_frequency mode, each section resets to start freq and applies directional shifts", {
  starting_freq <- 4000
  freq_shift <- 1000

  # generate strings
  generated_strings <- generate_strings(
    n_groups = 2,
    n_individuals = 5,
    n_calls = 10,
    string_length = 16,
    group_information = 8,
    individual_information = 2,
    random_variation = 4
  )
  parsons_results <- parsons_code(
    generated_strings,
    string_col = "Call",
    global_head_col = "Global_head",
    group_head_col = "Group_head",
    individual_middle_col = "Individual_middle",
    random_variation_col = "Random_variation",
    group_tail_col = "Group_tail",
    global_tail_col = "Global_tail",
    mapping = list("A" = "up", "B" = "down", "C" = "constant")
  )
  # make parsons code columns to character strings
  section_parsons_cols <- c(
    global_head = "Global_Head_Parsons_Code",
    group_head = "Group_Head_Parsons_Code",
    individual_middle = "Individual_Middle_Parsons_Code",
    random_variation = "Random_Variation_Parsons_Code",
    group_tail = "Group_Tail_Parsons_Code",
    global_tail = "Global_Tail_Parsons_Code"
  )
  for (code_col in section_parsons_cols) {
    parsons_results[[code_col]] <- as.character(parsons_results[[code_col]])
  }
  # Run the frequency_anchors function
  anchors <- frequency_anchors(
    df = parsons_results,
    parsons_col = "Call_Parsons_Code",
    group_id_col = "Group",
    individual_id_col = "Individual",
    call_id_col = "Call_ID",
    call_string_col = "Call",
    starting_frequency = starting_freq,
    frequency_shift = freq_shift,
    section_transition = "starting_frequency"
  )
  # Get frequencies from output
  freq_cols <- grep("^Frequency", names(anchors), value = TRUE)

  # Check that the first frequency is the starting frequency
  for (i in 1:nrow(parsons_results)) {
    returned_freqs <- as.numeric(anchors[1, freq_cols])

    # Check that the first frequency is the starting frequency
    expect_equal(returned_freqs[1], starting_freq,
               info = paste("Row", i, "First frequency should be:", starting_freq))
      cat(sprintf("\n Row: %s | Expected: %s | Got: %s \n",
                    i, starting_freq, returned_freqs[1]))

    # Walk through each section, compute what first frequency after reset should be
    freq_idx <- 2  # Skip Frequency1 (first is always 4000)
    for (section in names(section_parsons_cols)) {
      code_col <- section_parsons_cols[[section]]
      directions <- strsplit(parsons_results[[code_col]][1], "-")[[1]]

      if (length(directions) == 0 || is.na(directions[1])) next

      # Section starts at 4000
      first_direction <- directions[1]
      # Compute expected first frequency based on direction of the first character of the section
      expected_first_freq <- switch(first_direction,
                                    up = starting_freq + freq_shift,
                                    down = max(starting_freq - freq_shift, freq_shift),
                                    constant = starting_freq,
                                    stop("Invalid direction in Parsons code"))

      observed_first_freq <- returned_freqs[freq_idx]

      # Check that the section's first frequency is correctly computed
      expect_equal(observed_first_freq, expected_first_freq,
                  info = paste("Section:", section,
                                "| First direction:", first_direction,
                                "| Expected:", expected_first_freq,
                                "| Got:", observed_first_freq))
      cat(sprintf("Section: %s | First direction: %s | Expected: %s | Got: %s\n",
      section, first_direction, expected_first_freq, observed_first_freq))

      # Advance freq_idx to skip over rest of section (length of directions)
      freq_idx <- freq_idx + length(directions)
    }
  }
})
