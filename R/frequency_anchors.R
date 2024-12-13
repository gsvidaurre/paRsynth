#' Generate frequency anchors
#'
#' @author Ari Cross, Grace Smith-Vidaurre
#'
#' @description This function converts Parsons code strings to frequency anchors for later guiding frequency modulation patterns in `soundgen` audio files.
#'
#' @param df Data frame. A data frame generated by `parsons_code()` that contains the Parsons code strings.
#' @param parsons_col Character string. The name of the column in the data frame containing Parsons code strings.
#' @param group_id_col Character string. The name of the column in the data frame with numeric group identifiers.
#' @param individual_id_col Character string. The name of the column in the data frame containing numeric individual identifiers.
#' @param call_id_col Character string. The name of the column in the data frame containing unique numeric identifiers for each vocalization per individual.
#' @param call_string_col Character string. The name of the column in the data frame that contains the character string per vocalization.
#' @param starting_frequency Numeric value. A numeric value in Hz that specifies the frequency value that will be used as a baseline for creating frequency anchors with Parsons code. For instance, if this value is 4000 Hz and the first Parsons code value is "constant", then the first frequency anchor will be 4000 Hz. The default value is 4000 Hz.
#' @param frequency_shift Numeric value. A numeric value in Hz that specifies the frequency value that will be used to shift direction (or not). This is from the previous frequency anchor based on the Parsons code. For instance, if `frequency_shift` is 1000 Hz and the first Parsons code value is "up", then the first frequency anchor will be 5000 Hz. The default value is 1000 Hz. We have found that for total string lengths over 60 characters, it is better to use a smaller value (100 Hz). This avoids generating negative or zero values.
#'
#' @details `frequency_anchors()` returns the same data frame that was used as input with additional columns that hold frequency values in Hz. These columns will be used as anchors to guide frequency modulation patterns when creating synthetic audio files with the `soundgen` package. The starting frequency value is also used to end the frequency anchors. The number of frequency anchor columns in the data frame returned by the function depends on the length of each string. Currently, this function internally corrects frequency anchors that are negative or zero. It sets those values to the same value as the frequency shift (default of 1 kHz). While testing this function, we found that setting negative or zero values in the resulting data frame to 1000 Hz worked well. However, this change has not been thoroughly tested.
#'
#' @return This function returns a data frame containing the string of the vocalization, the original Parsons code string, and one column per frequency anchor value. This data structure facilitates calculating the minimum and maximum frequency anchors across vocalizations. This can facilitate writing synthetic audio files with `soundgen` and making spectrogram image files.
#'
#' @examples
#' seed <- 8
#' set.seed(seed) # For reproducibility
#' library(tidyverse)
#'
#' example_calls <- generate_strings(n_groups = 2, n_individuals = 5, n_calls = 10, string_length = 16, group_information = 8, individual_information = 2)
#'
#' example_calls_parsons <- parsons_code(example_calls, "Call", list("A" = "up", "B" = "down", "C" = "constant"))
#'
#' anchors <- frequency_anchors(example_calls_parsons, "Parsons_Code", "Group", "Individual", "Call_ID", "Call", starting_frequency = 4000, frequency_shift = 1000)
#'
#' glimpse(anchors)
#'
#' @export

frequency_anchors <- function(df, parsons_col, group_id_col, individual_id_col, call_id_col, call_string_col, starting_frequency = 4000, frequency_shift = 1000, section_transition = "starting_frequency") {

  if (starting_frequency <= 0) {
    stop("starting_frequency must be a positive value")
  }
  if (frequency_shift <= 0) {
    stop("frequency_shift must be a positive value")
  }
  if (nrow(df) == 0) {
    stop("Input data frame is empty")
  }
  if (!all(c(parsons_col, group_id_col, individual_id_col, call_id_col, call_string_col) %in% colnames(df))) {
    stop("One or more columns were not found in the data frame")
  }
  if (section_transition != "starting_frequency" && section_transition != "continuous_trajectory") {
    stop("section_transition must be 'starting_frequency' or 'continuous_trajectory'")
  }


  # Ensure column names are case-insensitive
  colnames(df) <- tolower(colnames(df))
  parsons_col <- tolower(parsons_col)
  group_id_col <- tolower(group_id_col)
  individual_id_col <- tolower(individual_id_col)
  call_id_col <- tolower(call_id_col)
  call_string_col <- tolower(call_string_col) # Fix: Added [i] to access specific row

  # Initialize an empty list to store the results
  results <- list()

  # Iterate over each row in the data frame
  for (i in seq_len(nrow(df))) {
    parsons_code <- df[[parsons_col]][i]
    group_id <- df[[group_id_col]][i]
    individual_id <- df[[individual_id_col]][i]
    call_id <- df[[call_id_col]][i]
    call_string <- df[[call_string_col]][i] # Fix: Added [i] to access specific row

    sections <- c("global_head", "group_head", "individual_middle", "group_tail", "global_tail")
    
    # separate out the different sections of the string based on known column names
    sections_data <- lapply(sections, function(section) { 
      strsplit(df[[sections]][i], "")[[1]] #spliting each row in the column (for each section) into a vector of individual characters
    })

    # Initialize an empty vector for frequency anchors
    frequencies <- c()

    # Initialize the frequencies vector with the starting frequency for each section
    previous_frequency <- starting_frequency
  
  # Loop over the sections to generate frequencies
  for (sec in 1:length(sections)) {
    section_code <- generate_frequencies_from_section(sections_data[[sec]], previous_frequency)
    if (section_transition == "starting_frequency") {
      section_frequencies <- generate_frequencies_from_section(section_code, previous_frequency)
    } else if (section_transition == "continuous_trajectory") {
      section_frequencies <- generate_frequencies_from_section(section_code, previous_frequency)
      previous_frequency <- tail(section_frequencies, 1) # Update previous_frequency for the next section based on the last frequency value in the current section
    }
    # Concatenate all frequencies into one final vector by appending the frequencies from the current section to the existing list of frequencies
    frequencies <- c(frequencies, section_frequencies)
  }

  # Store the results in a list then data frame for output
  results[[i]] <- c(
    group_id = group_id,
    individual_id = individual_id,
    call_id = call_id,
    call_string = call_string,
    frequencies = list(frequencies)
  )
}

  # Convert the list of results into a data frame with metadata and frequency values for the calls
  final_df <- do.call(rbind, lapply(results, as.data.frame))

  return(final_df)
}

# Helper function to generate frequencies from a section of Parsons code
generate_frequencies_from_section <- function(section_code, previous_value, frequency_shift=frequency_shift) {
  section_frequencies <- numeric(length(section_code))
  for (j in 1:length(section_code)) {
    direction <- section_code[j]
    if (direction == "up") {
      frequency <- previous_value + frequency_shift
    } else if (direction == "down") {
      frequency <- previous_value - frequency_shift
    } else if (direction == "constant") {
      frequency <- previous_value
    } else {
      stop("Invalid direction: ", direction)
    
    # Internally correct any frequency values that are zero or negative
    # Set these values to the frequency shift v
    if (frequency <= 0) {
      frequency <- frequency_shift
    }
    # Update the previous frequency value to have accurate frequency value assignment when the directionality is "constant" (in which the current frequency value should be the same as the previous value, but not the starting value)
    previous_value <- frequency
    section_frequencies[j] <- frequency
  }
  return(section_frequencies)
}}