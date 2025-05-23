#' Generate frequency anchors
#'
#' @author Ari Cross, Grace Smith-Vidaurre, Raneem Samman
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
#' @param section_transition Character string. The transition between sections in the Parsons code. The default value is "starting_frequency". The other option is "continuous_trajectory". In "starting_frequency" mode, the frequency value is reset to the `starting_frequency` value after each section. In "continuous_trajectory" mode, the frequency value is retained from the previous section. This can be useful for creating continuous frequency trajectories across vocalizations.
#'
#' @details `frequency_anchors()` returns the same data frame that was used as input with additional columns that hold frequency values in Hz. These columns will be used as anchors to guide frequency modulation patterns when creating synthetic audio files with the `soundgen` package. The starting frequency value is also used to end the frequency anchors. The number of frequency anchor columns in the data frame returned by the function depends on the length of each string. Currently, this function internally corrects frequency anchors that are negative or zero. It sets those values to the same value as the frequency shift (default of 1 kHz). While testing this function, we found that setting negative or zero values in the resulting data frame to 1000 Hz worked well. However, this change has not been thoroughly tested.
#'
#' @return This function returns a data frame containing the string of the vocalization, the original Parsons code string, and one column per frequency anchor value. This data structure facilitates calculating the minimum and maximum frequency anchors across vocalizations. This can facilitate writing synthetic audio files with `soundgen` and making spectrogram image files.
#'
#' @examples
#' seed <- 8
#' set.seed(seed) # For reproducibility
#' library(tidyverse)
#' example_calls <- generate_strings(n_groups = 2,
#'                                    n_individuals = 5,
#'                                    n_calls = 10,
#'                                    string_length = 16,
#'                                    group_information = 8,
#'                                    individual_information = 2,
#'                                    random_variation = 2)
#'
#' example_calls_parsons <- parsons_code(example_calls,
#'                                       "Call",
#'                                       "Global_head",
#'                                       "Group_head",
#'                                       "Individual_middle",
#'                                       "Random_variation",
#'                                       "Group_tail",
#'                                       "Global_tail",
#'                                       list("A" = "up",
#'                                            "B" = "down",
#'                                            "C" = "constant")
#'                                 )
#'
#' anchors <- frequency_anchors(example_calls_parsons,
#'                              "Call_Parsons_Code",
#'                              "Group",
#'                              "Individual",
#'                              "Call_ID",
#'                              "Call",
#'                              starting_frequency = 4000,
#'                              frequency_shift = 1000,
#'                              section_transition = "starting_frequency")
#'
#' glimpse(anchors)
#'
#' @export frequency_anchors

frequency_anchors <- function(df, parsons_col, group_id_col, individual_id_col,
                              call_id_col, call_string_col,
                              starting_frequency = 4000, frequency_shift = 1000,
                              section_transition = "starting_frequency") {

  if (starting_frequency <= 0) {
    stop("starting_frequency must be a positive value")
  }
  if (frequency_shift <= 0) {
    stop("frequency_shift must be a positive value")
  }
  if (nrow(df) == 0) {
    stop("Input data frame is empty")
  }
  if (!all(c(parsons_col,
             group_id_col,
             individual_id_col,
             call_id_col,
             call_string_col) %in% colnames(df))) {
    stop("One or more columns were not found in the data frame")
  }
  if (section_transition != "starting_frequency" &&
        section_transition != "continuous_trajectory") {
    stop("section_transition must be 'starting_frequency' or ",
         "'continuous_trajectory'")
  }

  # Ensure column names are case-insensitive
  colnames(df) <- tolower(colnames(df))
  parsons_col <- tolower(parsons_col)
  group_id_col <- tolower(group_id_col)
  individual_id_col <- tolower(individual_id_col)
  call_id_col <- tolower(call_id_col)
  call_string_col <- tolower(call_string_col)

  # Initialize an empty list to store the results
  results <- list()

  # Check all of these sections for NA values, which would indicate that they were not converted to Parsons code and should not be included in the frequency anchors
  sections <- c("global_head", "group_head", "individual_middle",
                "random_variation", "group_tail", "global_tail")

  # Get the names of section columns with NA values
  rem_nms <- names(which(sapply(sections, function(z) {
    any(is.na(df[[z]]))
  })))

  # Remove any columns with NAs from the sections to include in frequency anchors below
  if(length(rem_nms) > 0) {
    tmp_sections <- sections[-grep(paste(paste("^", rem_nms, "$", sep = ""), collapse = "|"), sections)]
  } else if(length(rem_nms) == 0) {
    tmp_sections <- sections
  }

  # Iterate over each row in the data frame
  for (i in seq_len(nrow(df))) {
    group_id <- df[[group_id_col]][i]
    individual_id <- df[[individual_id_col]][i]
    call_id <- df[[call_id_col]][i]
    call_string <- df[[call_string_col]][i]
    parsons_code <- df[[parsons_col]][i] # Extract the full Parsons code for each call 

    frequencies <- numeric(0) #initiating an empty vector to store the frequencies later
    previous_value <- starting_frequency

    # Iterate over each section in the Parsons_code columns, split the code, and calculate the frequencies
    for (section in tmp_sections) {
      section_code <- df[[paste0(section, "_parsons_code")]][i]
      split_parsons_code <- strsplit(section_code, "-")[[1]]
      call_length <- length(split_parsons_code)
      # Calculate frequencies for the current section
      section_frequencies <- numeric(call_length + 1)
      section_frequencies[1] <- previous_value  # Start with the previous section's last frequency

      for (j in seq_len(call_length)) {
        direction <- split_parsons_code[j]

        if (direction == "up") {
          frequency <- previous_value + frequency_shift
        } else if (direction == "down") {
          frequency <- previous_value - frequency_shift
        } else if (direction == "constant") {
          frequency <- previous_value
        } else {
          stop("Invalid direction: ", direction)
        }
        # Correct for negative or zero frequencies immediately
        if (frequency <= 0) {
          frequency <- frequency_shift
        }
        # Update previous_value to the current section frequency
        section_frequencies[j + 1] <- frequency
        previous_value <- frequency
      }
      frequencies <- c(frequencies, section_frequencies[-1])  # Exclude the first value to avoid repetition

      # After each section, reset the frequency or continue the trajectory
      if (section_transition == "starting_frequency") {
        previous_value <- starting_frequency
      } else {
        # In continuous trajectory mode, retain the last frequency value for the next section
        previous_value <- frequencies[length(frequencies)]
      }
    }

    # Do not add the starting frequency as the last frequency anchor
    frequencies <- c(starting_frequency, frequencies)

    # Create a data frame with the metadata and frequency values for the current call
    freq_df <- data.frame(
      Group = group_id,
      Individual = individual_id,
      Call_ID = call_id,
      Call = call_string,
      Parsons_Code = parsons_code,
      stringsAsFactors = FALSE
    )

    # Add frequency columns to the data frame
    for (k in 1:length(frequencies)) {
      freq_df[[paste0("Frequency", k)]] <- frequencies[k]
    }

    # Append the data frame to the results list
    results[[i]] <- freq_df
  }

  # Combine all results into a single data frame
  final_df <- do.call(rbind, results)
  return(final_df)
}
