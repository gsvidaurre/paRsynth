#' Generate strings with group membership and individual identity information
#'
#'@author Ari Cross, Grace Smith-Vidaurre, Raneem Samman
#'
#'@description `generate_strings()` creates character strings representing vocalizations with varying amounts of group membership and individual identity information. It allows users to specify the length of string components allocated to encoding individual identity (characters emphasizing differentiation among individuals) versus group membership (characters shared among group members but not across groups). Currently, the function generates strings using 3 unique characters: "A", "B", and "C".
#'
#' @param n_groups Integer. The number of groups.
#' @param n_individuals Integer. The number of individuals per group.
#' @param n_calls Integer. The number of calls per individual.
#' @param string_length Integer. The number of characters in the string that encode individual-level and group-level information through frequency modulation patterns. The current minimum is 6 characters and the current maximum is 200 characters.
#' @param group_information Integer. The number of characters that vary in the middle of the string across groups. The default is 8 characters.
#' @param individual_information Integer. The number of characters that vary in the middle of the string within groups. The default is 2 characters.
#' @param string_length Integer. The number of characters in the string that encode individual- and group-level information through frequency modulation patterns. The current minimum is 6 characters and the maximum is 200 characters. The length of the character string can influence visualizations of synthetic vocalizations in ways that depend on the duration of the vocalization (see details below).
#' @param group_information Integer. The number of characters that vary in the middle of the string across groups. The default is 8 characters. The user must provide an even value; negative values will result in unexpected behavior.
#' @param individual_information Integer. The number of characters that vary in the middle of the string within groups. The default is 2 characters. The user must provide an even value; negative values will result in unexpected behavior.
#' @param random_variation Integer. The number of characters that will vary randomly and will be appended after the individual information. The default is 2 characters. The user must provide an even value; negative values will result in unexpected behavior.
#'
#' @details The individual-specific and group-specific string components are combined to form the middle of a longer string. The individual-specific component of the string may not be unique to a single individual within a group, as individual distinctiveness  depends on the total number of individuals in the group, the length of the individually-specific string component, and the number of unique characters or symbols available for creating strings (which may vary depending on how users modify the function). For example, if the length of the individual-specific string component is 2 characters long and 3 unique characters are used, there will be 3^2 (or 9) possible unique individual signatures.
#'
#' The final string is composed of a global head (a short string of characters shared across all individuals), the group membership information, individual identity information, random variation, and a global tail (a short string of characters shared across all individuals). The global heads and tails are used to guide the start and end of frequency modulation patterns created after converting the character strings to Parsons code in later functions. The relative amount of group versus individual information across calls can be controlled by setting the length of `group_information` and `individual_information`, respectively. For example, when `group_information` is longer than `individual_information`, there will be more group membership information encoded in strings, and vice versa. The current version of the function does not facilitate varying string length within or across individuals. The random variation added to each string simulates the stochasticity in vocal production to facilitate variation within an individual.
#'
#' The length of the character string can influence visualizations of the synthetic vocalizations, depending on the duration of the synthetic vocalization (which is set in `write_audio()`). Long character strings that are converted to short synthetic vocalizations may have frequency contours that appear thick and blurry in spectrograms. A general rule of thumb to yield clearer frequency contours can be to increase the duration of the synthetic vocalization (although this will increase computational time for large datasets). We have found that vocalizations of 200 ms duration have clearer frequency contours when character string length in `generate_strings()` is set to 10 characters or fewer. For strings that are 12 to about 22 characters long, a 400 ms duration vocalization will have clearer contours.
#'
#' @return This function returns a data frame containing the call strings and metadata columns that contain unique numeric identifiers for the group, individual, and call for the given individual.
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
#'                                    random_variation = 2
#'                                  )
#' glimpse(example_calls)
#'
#' @export generate_strings

generate_strings <- function(n_groups = 2, n_individuals = 5, n_calls = 10,
                             string_length = 16, group_information = 8,
                             individual_information = 2, random_variation = 2, alphabet = c("A", "B", "C"), string_structure = "GI-II-RV-GI") {

  if (string_length < 6 || string_length > 200) {
    stop("string_length must be between 6 and 200")
  }
  if (missing(group_information)) {
    stop("group_information must be specified")
  }
  if (missing(individual_information)) {
    stop("individual_information must be specified")
  }
  if (!is.numeric(group_information) || length(group_information) != 1) { #ensures that the input is not a vector and is numeric
    stop("group_information must be numeric")
  }
  if (!is.numeric(individual_information) || length(individual_information) != 1) {
    stop("individual_information must be numeric")
  }
  if (floor(n_groups) != n_groups || floor(n_individuals) != n_individuals ||
        floor(n_calls) != n_calls || floor(string_length) != string_length ||
        floor(group_information) != group_information ||
        floor(individual_information) != individual_information) {
    stop("All arguments must be integers")
  }
  if (n_calls < 1 || n_groups < 1 || n_individuals < 1) {
    stop("All arguments must be greater than 0")
  }
  if (group_information %% 2 != 0 || individual_information %% 2 != 0) {
    stop("group_information and individual_information must be even numbers")
  }
  if (random_variation %% 2 != 0) {
    stop("random_variation must be an even number")
  }

  valid_structures <- c(
    "GI-II-RV", "GI-RV-II",
    "II-GI-RV", "II-RV-GI",
    "RV-II-GI", "RV-GI-II",
    "GI-II-RV-GI", "GI-RV-II-GI",
    "II-GI-RV-II", "II-RV-GI-II",
    "GI-RV-GI", "II-RV-II",
    "GI-RV", "RV-GI",
    "RV-II", "II-RV"
  )
  if (!(string_structure %in% valid_structures)) {
  stop("Invalid string_structure. Must be one of: ", paste(valid_structures, collapse = ", "))
  }

  # Create global header and tail strings. The length of these strings will vary depending on the length of the group-specific information (group_information) and the individual-specific information (individual_information)
  head_tail_length <- floor((string_length - group_information - individual_information - random_variation) / 2)

  # Generate a single head and tail for all groups
  global_head <- generate_random_string(head_tail_length, alphabet)
  global_tail <- generate_random_string(head_tail_length, alphabet)
  
  if (group_information > 0) {
    # Generate distinct group middle sections
    group_middles <- character(n_groups)
    for (g in 1:n_groups) {
      group_middles[g] <- generate_random_string(group_information, alphabet)
    }
  }
  if (individual_information > 0) {
    individual_middle <- character(n_individuals)
    for (i in 1:n_individuals) {
      individual_middle[i] <- generate_random_string(individual_information, alphabet)
    }
  }

  calls <- character(n_groups * n_individuals * n_calls)
  groups <- rep(1:n_groups, each = n_individuals * n_calls)
  individuals <- numeric(n_groups * n_individuals * n_calls)
  call_numbers <- numeric(n_groups * n_individuals * n_calls)
  global_head_calls  <- character(n_groups * n_individuals * n_calls)
  global_tail_calls <- character(n_groups * n_individuals * n_calls)
  individual_head_calls <- character(n_groups * n_individuals * n_calls)
  individual_tail_calls <- character(n_groups * n_individuals * n_calls)
  random_string_calls <- character(n_groups * n_individuals * n_calls)
  group_head_calls <-  character(n_groups * n_individuals * n_calls)
  group_tail_calls <-  character(n_groups * n_individuals * n_calls)

  for (group in 1:n_groups) {
    for (ind in 1:n_individuals) {
      for (call in 1:n_calls) {

        # Generate random variation per call that will be appended after the individual information (to create variation within individuals)
        random_string <- generate_random_string(random_variation, alphabet)

        # Assemble the string with both group and individual information if both group and individual information are greater than 0
        if(group_information > 0 & individual_information > 0) {

          group_info <- group_middles[group]
          group_head <- substr(group_info, 1, group_information / 2)
          group_tail <- substr(group_info, group_information / 2 + 1, group_information)

          individual_info <- individual_middle[ind]
          ind_head <- substr(individual_info, 1, individual_information / 2)
          ind_tail <- substr(individual_info, individual_information / 2 + 1, individual_information)
          
          # Combine all components to create a string that represents a vocalization. 
          string_assembly <- build_string_structure(string_structure, group_head, group_tail, ind_head, ind_tail, random_string)
          
          individual_call <- paste0(
            global_head,
            string_assembly,
            global_tail
          )

          # Save general values for the current loop iteration
          idx <- ((group - 1) * n_individuals * n_calls) + ((ind - 1) * n_calls) + call
          individuals[idx] <- ind
          call_numbers[idx] <- call
          global_head_calls[idx] <- global_head
          global_tail_calls[idx] <- global_tail
          random_string_calls[idx] <- random_string
          individual_head_calls[idx] <- ind_head
          individual_tail_calls[idx] <- ind_tail
          group_head_calls[idx] <- group_head
          group_tail_calls[idx] <- group_tail

          # Save the full assembled vocalization for the current loop iteration
          calls[idx] <- individual_call

          # Assemble the string with individual information only if group information is 0
        } else if (group_information == 0 & individual_information > 0){
          
          individual_info <- individual_middle[ind]
          ind_head <- substr(individual_info, 1, individual_information / 2)
          ind_tail <- substr(individual_info, individual_information / 2 + 1, individual_information)
          
          # Combine all components to create a string that represents a vocalization. 
          string_assembly <- build_string_structure(string_structure, "", "", ind_head, ind_tail, random_string)
          
          individual_call <- paste0(
            global_head,
            string_assembly,
            global_tail
          )

          # Save general values for the current loop iteration
          idx <- ((group - 1) * n_individuals * n_calls) + ((ind - 1) * n_calls) + call
          individuals[idx] <- ind
          call_numbers[idx] <- call
          global_head_calls[idx] <- global_head
          global_tail_calls[idx] <- global_tail
          random_string_calls[idx] <- random_string
          individual_head_calls[idx] <- ind_head
          individual_tail_calls[idx] <- ind_tail
          group_head_calls[idx] <- NA
          group_tail_calls[idx] <- NA

          # Save the full assembled vocalization for the current loop iteration
          calls[idx] <- individual_call

          # Assemble the string with group information only if individual information is 0
        } else if (group_information > 0 & individual_information == 0) {

          group_info <- group_middles[group]
          group_head <- substr(group_info, 1, group_information / 2)
          group_tail <- substr(group_info, group_information / 2 + 1, group_information)
          
          # Combine all components to create a string that represents a vocalization. 
          string_assembly <- build_string_structure(string_structure, group_head, group_tail, "", "", random_string)
          
          individual_call <- paste0(
            global_head,
            string_assembly,
            global_tail
          )

          # Save general values for the current loop iteration
          idx <- ((group - 1) * n_individuals * n_calls) + ((ind - 1) * n_calls) + call
          individuals[idx] <- ind
          call_numbers[idx] <- call
          global_head_calls[idx] <- global_head
          global_tail_calls[idx] <- global_tail
          random_string_calls[idx] <- random_string
          individual_head_calls[idx] <- NA
          individual_tail_calls[idx] <- NA
          group_head_calls[idx] <- group_head
          group_tail_calls[idx] <- group_tail

          # Save the full assembled vocalization for the current loop iteration
          calls[idx] <- individual_call

        }
      }
    }
  }
  
  # Return the assembled vocalizations and metadata across iterations, including the separate sections of character strings used to assemble vocalizations
  data.frame(
    Group = groups,
    Individual = individuals,
    Call_ID = call_numbers,
    Call = calls,
    Global_head = global_head_calls,
    Group_head = group_head_calls,
    Individual_head = individual_head_calls,
    Individual_tail = individual_tail_calls,
    Random_variation = random_string_calls,
    Group_tail = group_tail_calls,
    Global_tail = global_tail_calls
  )
}


# Helper function to generate a random string from 3 unique characters (for 1-base Parsons code)
generate_random_string <- function(length, alphabet) {
  paste(sample(alphabet, length, replace = TRUE), collapse = "")
}

build_string_structure <- function(structure, GI_head, GI_tail, II_head, II_tail, RV) {
  res <- switch(structure,
    "GI-II-RV" = paste0(GI_head, GI_tail, II_head, II_tail, RV),
    "GI-RV-II" = paste0(GI_head, GI_tail, RV, II_head, II_tail),

    "II-GI-RV" = paste0(II_head, II_tail, GI_head, GI_tail, RV),
    "II-RV-GI" = paste0(II_head, II_tail, RV, GI_head, GI_tail),

    "RV-II-GI" = paste0(RV, II_head, II_tail, GI_head, GI_tail),
    "RV-GI-II" = paste0(RV, GI_head, GI_tail, II_head, II_tail),

    "GI-II-RV-GI" = paste0(GI_head, II_head, II_tail, RV, GI_tail),
    "GI-RV-II-GI" = paste0(GI_head, RV, II_head, II_tail, GI_tail),

    "II-GI-RV-II" = paste0(II_head, GI_head, GI_tail, RV, II_tail),
    "II-RV-GI-II" = paste0(II_head, RV, GI_head, GI_tail, II_tail),

    "GI-RV-GI" = paste0(GI_head, RV, GI_tail),
    "II-RV-II" = paste0(II_head, RV, II_tail),

    "GI-RV" = paste0(GI_head, GI_tail, RV),
    "RV-GI" = paste0(RV, GI_head, GI_tail),

    "II-RV" = paste0(II_head, II_tail, RV),
    "RV-II" = paste0(RV, II_head, II_tail),
  )
  return(res)
}