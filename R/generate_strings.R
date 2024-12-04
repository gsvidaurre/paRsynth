#' Generate strings with group membership and individual identity information
#'
#'@author Ari Cross, Grace Smith-Vidaurre
#'
#'@description `generate_strings()` creates character strings that represent vocalizations with varying amounts of group membership and individual identity information. The function facilitates specifying the length of string components that should be allocated to encoding individual identity (characters that are selected to emphasize differentiation among individuals) versus group membership (characters shared among group members but not between groups). Currently, this function builds strings using 3 unique characters only ("A", "B", and "C").
#'
#' @param n_groups Integer. The number of groups.
#' @param n_individuals Integer. The number of individuals per group.
#' @param n_calls Integer. The number of calls per individual.
#' @param string_length Integer. The number of characters present in the string that encodes individual-level and group-level information through frequency modulation patterns. The current minimum is 6 characters and the current maximum is 200 characters.
#' @param group_information Integer. The number of characters that will vary in the middle of the string across groups. The default is 8 characters.
#' @param individual_information Integer. The number of characters that will vary in the middle of the string within groups. The default is 2 characters.
#'
#' @details The individual-specific and group-specific string components are combined to form the middle of a longer string. The individually-specific component of the string may not be unique to any one individual in a group, as individual distinctiveness will depend on the total number of individuals in a group, the length of the individually-specific string component, and depending on how users modify this function, the number of unique characters or symbols that can be used to create strings. For instance, when the length of the individually distinctive string component is 2 characters long, and you're using 3 unique characters to create strings, there will be 3^2 or 9 possible unique individual signatures.
#'
#' The final string is composed of a global head (a short string of characters shared across all individuals), the group membership information, individual identity information, and a global tail (a short string of characters shared across all individuals). The global heads and tails are used to guide the start and end of frequency modulation patterns created after converting character strings to Parsons code in later functions. The relative amount of group versus individual information across calls can be controlled by setting the length of `group_information` and `individual_information`, respectively. For example, when `group_information` is longer than `individual_information`, there will be more group membership information encoded in strings, and vice versa. The current version of the function does not facilitate varying string length within or across individuals.
#'
#' @return A data frame containing the call strings as well as metadata columns that contain unique numeric identifiers for the group, individual, and call for the given individual.
#'
#' @examples
#' seed <- 8
#' set.seed(seed) # For reproducibility
#' library(tidyverse)
#' example_calls <- generate_strings(n_groups = 2, n_individuals = 5, n_calls = 10, string_length = 16, group_information = 8, individual_information = 2)
#' glimpse(example_calls)
#'
#' @export generate_strings

generate_strings <- function(n_groups = 2, n_individuals = 5, n_calls = 10, string_length = 16, group_information = 8, individual_information = 2) {

  if (string_length < 6 || string_length > 200) {
    stop("string_length must be between 6 and 200")
  }

  if(missing(group_information)){
    stop("group_information must be specified")
  }

  if(missing(individual_information)){
    stop("individual_information must be specified")
  }

  # Create global header and tail strings. The length of these strings will vary depending on the length of the group-specific information (group_information) and the individual-specific information (individual_information)
  head_tail_length <- floor((string_length - group_information - individual_information) / 2)

  # Generate a single head and tail for all groups
  global_head <- generate_random_string(head_tail_length)
  global_tail <- generate_random_string(head_tail_length)

  if (group_information > 0) {
    # Generate distinct group middle sections
    group_middles <- character(n_groups)
    for (g in 1:n_groups) {
      group_middles[g] <- generate_random_string(group_information)
    }
  }

  calls <- character(n_groups * n_individuals * n_calls)
  groups <- rep(1:n_groups, each = n_individuals * n_calls)
  individuals <- numeric(n_groups * n_individuals * n_calls)
  call_numbers <- numeric(n_groups * n_individuals * n_calls)
  # Raneem's additions
  global_head_calls  <- character(n_groups * n_individuals * n_calls)
  global_tail_calls <- character(n_groups * n_individuals * n_calls)
  individual_middle_calls <- character(n_groups * n_individuals * n_calls)
  group_head_calls <-  character(n_groups * n_individuals * n_calls)
  group_tail_calls <-  character(n_groups * n_individuals * n_calls)

  for (group in 1:n_groups) {
    for (ind in 1:n_individuals) {
      # Generate a unique middle part for each individual
      individual_middle <- generate_random_string(individual_information)
      if (group_information > 0) {
        # group_middle_head_tail_length <- (group_information - individual_information) / 2
        group_info <- group_middles[group]
        group_head <- substr(group_info, 1, group_information / 2)
        group_tail <- substr(group_info, group_information / 2 + 1, group_information)

        # Combine all components to create a string that represents a vocalization
        individual_call <- paste0(
          global_head,
          # substr(group_middles[group], 1, group_middle_head_tail_length),
          group_head,
          individual_middle,
          # substr(group_middles[group], group_middle_head_tail_length + individual_information + 1, group_information),
          group_tail,
          global_tail
        )
      } else {
        individual_call <- paste0(
          global_head,
          individual_middle,
          global_tail
        )
      }

      for (call in 1:n_calls) {
        idx <- ((group - 1) * n_individuals * n_calls) + ((ind - 1) * n_calls) + call
        calls[idx] <- individual_call
        individuals[idx] <- ind
        call_numbers[idx] <- call
        # Raneem's additions
        global_head_calls[idx] <- global_head
        global_tail_calls[idx] <- global_tail
        individual_middle_calls[idx] <- individual_middle
        group_head_calls[idx] <- group_head
        group_tail_calls[idx] <- group_tail
      }
    }
  }

  data.frame(
    Group = groups,
    Individual = individuals,
    Call_ID = call_numbers,
    Call = calls,
    # Raneem's additions
    Global_head = global_head_calls,
    Group_head = group_head_calls,
    Individual_middle = individual_middle_calls,
    Group_tail = group_tail_calls,
    Global_tail = global_tail_calls,


    stringsAsFactors = FALSE
  )
}


# Helper function to generate a random string from 3 unique characters (for 1-base Parsons code)
generate_random_string <- function(length) {
  paste(sample(c("A", "B", "C"), length, replace = TRUE), collapse = "")
}
