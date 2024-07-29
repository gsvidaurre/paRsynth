
# Helper function to generate a random string
generate_random_string <- function(length) {
  paste(sample(c("A", "B", "C"), length, replace = TRUE), collapse = "")
}
