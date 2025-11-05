# Author: G.A. Juarez and Raneem Samman
# Date created: December 4, 2024

base_parameters <- list(
  n_groups <- 2,
  n_individuals <- 5,
  n_calls <- 10,
  string_length <- 16,
  group_information <- 8,
  individual_information <- 2,
  random_variation <- 2,
  alphabet <- c("A", "B", "C"),
  string_structure <- "GI-II-RV-GI"
)

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

Conversion <- parsons_code(
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

# Calculate the generated parsons code directions
count_words <- function(string) {
  length(strsplit(string, "-")[[1]])
}

# 1. Unit test to check that the parsons code is the correct length
test_that("This functions generates the correct length of parsons code", {
  # Calculate the length of the parsons code for each generated string then get the unique lengths
  generated_parsons_code <- unique(sapply(Conversion$Call_Parsons_Code, count_words)) # sapply is able to apply a function to each element of a list or vector

  # Check that the generated parsons code is the correct length
  expect_true(string_length == generated_parsons_code,
              info = "Not all generated calls have the expected number.")
})

# 2. Unit test to check that correct number of parson codes were generated
test_that("The functions generates the correct number of parson codes", {
  # Calculate how many parsons codes were generated
  n_generated_parsons_codes <- nrow(Conversion)

  # Find the number of expected parson codes
  n_expected_parsons_codes <- n_calls*n_groups*n_individuals

  # Check that the number of parsons codes were generated as expected
  expect_equal(n_generated_parsons_codes, n_expected_parsons_codes,
               info = "Not all parson codes were generated.")
})

# 3. Unit test to check that the correct parsons code were generated
test_that("The function generates correct parsons code", {
  # Generate generic strings (easy to track conversion)
  Global_head <- "AABA"
  Group_head <- "BBCC"
  Individual_complete <- "CCBA"
  Random_variation <- "BA"
  Group_tail <- "BBAC"
  Global_tail <- "BBAA"
  Individual_head <- gsub('CC', '', Individual_complete)
  Individual_tail <- gsub('BA', '', Individual_complete)
  Group_complete <- c(Group_head, Group_tail)

  generated_strings <- data.frame(
    Call = paste(Global_head, Group_head, Individual_complete, Random_variation, Group_tail, Global_tail, sep = ""),
    Global_head = Global_head, Group_head = Group_head, Individual_complete = Individual_complete,
    Global_head = Global_head, Group_head = Group_head, Individual_head = Individual_head, Individual_tail = Individual_tail, Individual_complete = Individual_complete, Group_complete = Group_complete,
    Random_variation = Random_variation, Group_tail = Group_tail, Global_tail = Global_tail
  )

  # Convert using parsons_code
  Conversion <- parsons_code(
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
  
  # Check that the generated parsons code is the same as the expected parsons code ("A" = "up", "B" = "down", "C" = "constant")
  generated_parsons_code <- unname(Conversion$Call_Parsons_Code)[1]
  expected_parsons_code <- "up-up-down-up-down-down-constant-constant-constant-constant-down-up-down-up-down-down-up-constant-down-down-up-up"
  expect_equal(generated_parsons_code, expected_parsons_code)
})

# 4. Unit test to check that the df has the right number of dimensions (right number of rows and columns)
test_that("The function generates a data frame that has the right number of rows and columns",{
  # names(Conversion) returns the column names of the data frame Conversion
  conversation_cols <- names(Conversion)
  # Get the columns that contain the parsons code.
  parsons_df_cols <- conversation_cols[grep("_Parsons_", conversation_cols)]

  # Check that the data frame has the right number of rows and columns
  expect_equal(nrow(generated_strings),nrow(Conversion))
  expect_equal(ncol(generated_strings) + length(parsons_df_cols), ncol(Conversion))
  
})

# 5. Unit test to check that the function correctly takes in alphabet and mapping beyond the standard 3 base encoding (A, B, C)
test_that("The function generates correct parsons code when base encoding goes beyond A = up, B = down, C = constant", {
  
  # Generate strings with 3+ base encoding
  generated_strings_alphabet <- suppressWarnings(generate_strings(
    n_groups = n_groups,
    n_individuals = n_individuals,
    n_calls = n_calls,
    string_length = string_length,
    group_information = group_information,
    individual_information = individual_information,
    random_variation = random_variation,
    alphabet = alphabet <- c("A", "B", "C", "D", "E"),
    string_structure = string_structure
  ))
  
  # Convert using parsons_code
  Conversion_alphabet <- parsons_code(
    generated_strings_alphabet, 
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
    mapping = list("A" = "up", "B" = "down", "C" = "constant", "D" = "up_0.5", "E" = "down_0.5")
  )
  
  for (i in 1:nrow(generated_strings_alphabet)) {
    
    # Call out a specific generated string
    generated_call <- Conversion_alphabet$Call[i]
    
    # Split string into individual letters
    generated_call_vec <- strsplit(generated_call, "")[[1]]
    
    # Call out a the strings' parsons code conversion
    generated_parsons_code <-
      unname(Conversion_alphabet$Call_Parsons_Code)[i]
    
    cat("----- Testing string conversion with 3+ bases: -----", generated_call, "\n")
    
    # Switch the bases to expected mapping
    expected_mapping <- recode(generated_call_vec, "A" = "up", "B" = "down", "C" = "constant", "D" = "up_0.5", "E" = "down_0.5", .default = NA_character_)
    
    # collapse into one string
    expected_parsons_code <- paste(expected_mapping, collapse = "-")
    
  expect_equal(generated_parsons_code, expected_parsons_code)
  
  cat("Expected Parsons Code:", generated_parsons_code, "\n")
  cat("Generated Parsons Code:", expected_parsons_code, "\n")
  
  }
})
