#' Generate Frequency Data Frame from Parsons Code Column
#'
#'@author Ari Cross, Grace Smith-Vidaurre
#'
#' This function takes a data frame and a column name containing Parsons code strings,
#' and generates a data frame that includes social and call metadata, as well as frequency values
#' based on the Parsons code conversion.
#'
#' @param df A data frame containing the Parsons code strings.
#' @param parsons_col The name of the column in the data frame containing Parsons code strings.
#' @param group_id_col The name of the column in the data frame containing numeric group IDs.
#' @param individual_id_col The name of the column in the data frame containing numeric individual IDs.
#' @param call_id_col The name of the column in the data frame containing unique numeric identifiers for each call per individual.
#' @param callNo_id_col The name of the column in the data frame specifying which number call an individual is on.
#' @return A data frame containing the group ID, individual ID, call ID,
#' the original Parsons code string, and one column per frequency value.
#' @examples
#' df <- data.frame(
#'   Parsons = c("up-down-constant-up", "down-up-constant-down"),
#'   GroupID = c(1, 1),
#'   IndividualID = c(101, 102),
#'   CallID = c(1, 2),
#'   stringsAsFactors = FALSE
#' )
#' generate_frequency(df, "Parsons", "GroupID", "IndividualID", "CallID")
#' @export

generate_frequency <- function(df, parsons_col, group_id_col, individual_id_col, call_id_col, callNo_id_col) {
  # Ensure column names are case-insensitive
  colnames(df) <- tolower(colnames(df))
  parsons_col <- tolower(parsons_col)
  group_id_col <- tolower(group_id_col)
  individual_id_col <- tolower(individual_id_col)
  call_id_col <- tolower(call_id_col)
  callNo_id_col <- tolower(callNo_id_col)

  # Initialize an empty list to store the results
  results <- list()

  # Iterate over each row in the data frame
  for (i in 1:nrow(df)) {
    parsons_code <- df[[parsons_col]][i]
    group_id <- df[[group_id_col]][i]
    individual_id <- df[[individual_id_col]][i]
    call_id <- df[[call_id_col]][i]
    callNo_id <- df[[callNo_id_col]][i]  # Fix: Added [i] to access specific row

    # Split the parsons_code string by dashes
    split_parsons_code <- strsplit(parsons_code, "-")[[1]]

    # Calculate the length of the split vector
    call_length <- length(split_parsons_code)

    # Initialize the frequencies vector with starting frequency 4000
    frequencies <- numeric(length(split_parsons_code) + 2)
    frequencies[1] <- 4000
    previous_value <- 4000

    # Set the increment value based on call_length
    increment <- if (call_length > 60) 100 else 1000

    # Iterate over the split parsons code to generate frequencies
    for (j in 1:call_length) {
      direction <- split_parsons_code[j]
      if (direction == "up") {
        frequency <- previous_value + increment
      } else if (direction == "down") {
        frequency <- previous_value - increment
      } else if (direction == "constant") {
        frequency <- previous_value
      } else {
        stop("Invalid direction: ", direction)
      }
      previous_value <- frequency
      frequencies[j + 1] <- frequency
    }

    # Add the final frequency value
    frequencies[length(frequencies)] <- 4000

    # Create a data frame with the metadata and frequency values for the current call
    freq_df <- data.frame(
      Group = group_id,
      Individual = individual_id,
      Call.No = callNo_id,
      Call = call_id,
      Parsons_Code = parsons_code,
      stringsAsFactors = FALSE
    )

    # Add frequency columns to the data frame
    for (k in 1:length(frequencies)) {  # Fix: Iterate over length of frequencies correctly
      freq_df[[paste0("Frequency", k)]] <- frequencies[k]
    }

    # Append the data frame to the results list
    results[[i]] <- freq_df
  }

  # Combine all results into a single data frame
  final_df <- do.call(rbind, results)

  return(final_df)
}
