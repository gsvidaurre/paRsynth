#' Generate strings with varying amounts of group membership and individual identity information
#'
#'@author Ari Cross, Grace Smith-Vidaurre
#'
#'@description `generate_strings` creates character strings that represent vocalizations with varying amounts of group membership and individual identity information. The function facilitates specifying the length of string components that should be allocated to encoding individual identity (characters that are selected to emphasize differentiation among individuals) versus group membership (characters shared among group members but not between groups). Currently, this function builds strings using 3 unique characters only ("A", "B", and "C").
#'
#' @param n_groups Integer. Number of groups.
#' @param n_individuals Integer. Number of individuals per group.
#' @param n_calls Integer. Number of calls per individual.
#' @param string_length Integer. Number of characters present in the string that encodes individual-level and group-level information through frequency modulation. The current minimum is 6 and the current maximum is 200.
#' @param gl_middle Integer. Number of characters that will vary in the middle of the string across groups. Default is 8.
#' @param il_middle Integer. Number of characters that will vary in the middle of the string within groups. Default is 2.
#' @details The individual-specific and group-specific string components are combined to form the middle of a longer string. The individually-specific component of the string may not be unique to any one individual in a group, as individual distinctiveness will depend on the total number of individuals in a group, the length of the individually-specific string component, and depending on how users modify this function, the unique characters that can be used to create strings. The final string is composed of a global head (a short string of characters shared across all individuals), the group membership information, individual identity information, and a global tail (a short string of characters shared across all individuals). The global heads and tails are used to guide the start and end of frequency modulation patterns created after converting character strings to Parsons code in later functions. The relative amount of group versus individual information across calls can be controlled by setting the length of `gl_middle` and `il_middle`, respectively. For example, when `gl_middle` is longer than `il_middle`, there will be more group membership information encoded in strings, and vice versa.
#' @return A data frame containing the call strings as well as group, individual, and call metadata.
#' @examples
#' set.seed(3)  # For reproducibility
#' example_calls <- gen_calls(n_groups = 2, n_individuals = 5, n_calls = 10, string_length = 16, gl_middle = 8, il_middle = 2)
#' View(example_calls)
#' @export

gen_calls <- function(n_groups = 2, n_individuals = 5, n_calls = 10, string_length = 16, gl_middle = 8, il_middle = 2) {

  if (string_length < 6 || string_length > 200) {
    stop("string_length must be between 6 and 200")
  }

  if(missing(gl_middle)){
    stop("gl_middle must be specified")
  }

  if(missing(il_middle)){
    stop("il_middle must be specified")
  }

  # Create global header and tail strings. The length of these strings will vary depending on the length of the group-specific information (gl_middle) and the individual-specific information (il_middle)
  head_tail_length <- floor((string_length - gl_middle - il_middle) / 2)

  # Generate a single head and tail for all groups
  global_head <- generate_random_string(head_tail_length)
  global_tail <- generate_random_string(head_tail_length)

  if (gl_middle > 0) {
    # Generate distinct group middle sections
    group_middles <- character(n_groups)
    for (g in 1:n_groups) {
      group_middles[g] <- generate_random_string(gl_middle)
    }
  }

  calls <- character(n_groups * n_individuals * n_calls)
  groups <- rep(1:n_groups, each = n_individuals * n_calls)
  individuals <- numeric(n_groups * n_individuals * n_calls)
  call_numbers <- numeric(n_groups * n_individuals * n_calls)

  for (group in 1:n_groups) {
    for (ind in 1:n_individuals) {
      # Generate a unique middle part for each individual
      individual_middle <- generate_random_string(il_middle)
      if (gl_middle > 0) {
        group_middle_head_tail_length <- (gl_middle - il_middle) / 2
        # Combine all components to create a string that represents a vocalization
        individual_call <- paste0(
          global_head,
          substr(group_middles[group], 1, group_middle_head_tail_length),
          individual_middle,
          substr(group_middles[group], group_middle_head_tail_length + il_middle + 1, gl_middle),
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
      }
    }
  }

  data.frame(
    Group = groups,
    Individual = individuals,
    Call_ID = call_numbers,
    Call = calls,
    stringsAsFactors = FALSE
  )
}
