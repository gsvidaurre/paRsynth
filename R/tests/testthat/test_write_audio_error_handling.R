# Author: Raneem Samman
# Date created: December 17, 2024

create_test_data <- function(desktop_path) {
  tmp_dir <- file.path(desktop_path, "R_test_temp")

  if (!dir.exists(tmp_dir)) {
    dir.create(tmp_dir)
  }
  # Example data frame for testing
  df_test <- data.frame(
    Group = c(1, 2),
    Individual = c(1, 2),
    Call_ID = c(1, 2),
    Frequency_1 = c(4000, 4500),
    Frequency_2 = c(5000, 5500)
  )
  # Call the function with test df
  result_df <- write_audio(df_test,
                          sylLen = 200,
                          sampling_rate = 150000,
                          pitch_sampling_rate = 100000,
                          smoothing = list(interpol = "loess", loessSpan = 1, discontThres = 0, jumpThres = 0),
                          rolloffExact = c(0.25, 0.25, 0.25, 0.25, 0.25),
                          formants = NA,
                          vocalTract = NA,
                          temperature = 0,
                          subratio = 2,
                          shortestEpoch = 300,
                          vibratoFreq = 1,
                          prefix = "TestPrefix",
                          save_path = tmp_dir,
                          invalidArgAction = "ignore"
  )
  return(list(df_test = df_test, tmp_dir = tmp_dir, result_df = result_df))
}

test_that("Error handling for write_audio", {
  # Create a temporary directory on the Desktop for storing files (for test purposes)
  test_data <- create_test_data(desktop_path)
  df_test <- test_data$df_test
  tmp_dir <- test_data$tmp_dir
  result_df <- test_data$result_df
  # include everything from df_test except Call_ID
  df_test2 <- df_test[, !names(df_test) %in% "Call_ID"]

  # test that the df input is not a data frame
  expect_error(
    write_audio(
      1, # not a data frame
      sampling_rate = 150000,
      sylLen = 200,
      prefix = "TestPrefix",
      save_path = tmp_dir
    ),
    "The 'df' argument must be a data frame."
  )
  # test that the df input is not empty
  expect_error(
    write_audio(
      data.frame(), # empty data frame
      sampling_rate = 150000,
      sylLen = 200,
      prefix = "TestPrefix",
      save_path = tmp_dir
    ),
    "Input data frame is empty"
  )
  # test that the df input does not contain the required columns
  expect_error(
    write_audio(
      df_test2, # missing Call_ID
      sampling_rate = 150000,
      sylLen = 200,
      prefix = "TestPrefix",
      save_path = tmp_dir
    ),
    "One or more columns were not found in the data frame"
  )
  # test that the save_path is not a character string
  expect_error(
    write_audio(
      df_test,
      sampling_rate = 150000,
      sylLen = 200,
      prefix = "TestPrefix",
      save_path = # Null save_path
    ),
    "The 'save_path' argument must be provided and cannot be empty."
  )
  # test that the sampling_rate is not a positive value
  expect_error(
    write_audio(
      df_test,
      sampling_rate = -150000, # not a positive value
      sylLen = 200,
      prefix = "TestPrefix",
      save_path = tmp_dir
    ),
    "The 'sampling_rate' must be a positive numeric value."
  )
  expect_error(
    write_audio(
      df_test,
      sampling_rate = 0, # not a positive value
      sylLen = 200,
      prefix = "TestPrefix",
      save_path = tmp_dir
    ),
    "The 'sampling_rate' must be a positive numeric value."
  )
  # test that the sylLen is not a positive value
  expect_error(
    write_audio(
      df_test,
      sampling_rate = 150000,
      sylLen = -200, # not a positive value
      prefix = "TestPrefix",
      save_path = tmp_dir
    ),
    "The 'sylLen' argument must be a positive numeric value."
  )
  expect_error(
    write_audio(
      df_test,
      sampling_rate = 150000,
      sylLen = 0, # not a positive value
      prefix = "TestPrefix",
      save_path = tmp_dir
    ),
    "The 'sylLen' argument must be a positive numeric value."
  )
  # test that the sampling_rate is not a numeric value
  expect_error(
    write_audio(
      df_test,
      sampling_rate = "150000", # not a numeric value
      sylLen = 200,
      prefix = "TestPrefix",
      save_path = tmp_dir
    ),
    "The 'sampling_rate' must be a positive numeric value."
  )
  # test that the sylLen is not a numeric value
  expect_error(
    write_audio(
      df_test,
      sampling_rate = 150000,
      sylLen = "200", # not a numeric value
      prefix = "TestPrefix",
      save_path = tmp_dir
    ),
    "The 'sylLen' argument must be a positive numeric value."
  )
  # test that the prefix is not a character string
  expect_error(
    write_audio(
      df_test,
      sampling_rate = 150000,
      sylLen = 200,
      prefix = 1, # not a character string
      save_path = tmp_dir
    ),
    "The 'prefix' argument must be a character string."
  )
  # test that the pitch_sampling_rate is not a positive numeric value
  expect_error(
    write_audio(
      df_test,
      sampling_rate = 150000,
      sylLen = 200,
      pitch_sampling_rate = -2, # not a positive value
      save_path = tmp_dir
    ),
      "The 'pitch_sampling_rate' must be a positive numeric value."
  )
  # test that temperature is not a numeric value
  expect_error(
    write_audio(
      df_test,
      sampling_rate = 150000,
      sylLen = 200,
      temperature = "0", # not a numeric value
      prefix = "TestPrefix",
      save_path = tmp_dir
    ),
    "The 'temperature' argument must be a numeric value."
  )
  # test that the subratio is not a numeric value
  expect_error(
    write_audio(
      df_test,
      sampling_rate = 150000,
      sylLen = 200,
      subratio = "2", # not a numeric value
      prefix = "TestPrefix",
      save_path = tmp_dir
    ),
    "The 'subratio' argument must be a positive numeric value."
  )
  # test that the shortestEpoch is not a numeric value
  expect_error(
    write_audio(
      df_test,
      sampling_rate = 150000,
      sylLen = 200,
      shortestEpoch = "300", # not a numeric value
      prefix = "TestPrefix",
      save_path = tmp_dir
    ),
    "The 'shortestEpoch' argument must be a positive numeric value."
  )
  # test that the vibratoFreq is not a numeric value
  expect_error(
    write_audio(
      df_test,
      sampling_rate = 150000,
      sylLen = 200,
      vibratoFreq = "1", # not a numeric value
      prefix = "TestPrefix",
      save_path = tmp_dir
    ),
    "The 'vibratoFreq' argument must be a positive numeric value."
  )
  # test that the rolloffExact is not a numeric vector
expect_error(
  write_audio(
    df_test,
    sampling_rate = 150000,
    sylLen = 200,
    prefix = "TestPrefix",
    save_path = tmp_dir,
    rolloffExact = c(0.5, 1.5, 0.25) # Contains value > 1
  ),
  "When 'rolloffExact' is a vector, all values must be between 0 and 1."
)

# Test matrix input with invalid values for rolloffExact
expect_error(
  write_audio(
    df_test,
    sampling_rate = 150000,
    sylLen = 200,
    prefix = "TestPrefix",
    save_path = tmp_dir,
    rolloffExact = matrix(c(0.5, 1.5, 0.25, 0.1), ncol=2) # Contains value > 1
  ),
  "When 'rolloffExact' is a matrix, all values must be between 0 and 1."
)

# Test non-numeric input for rolloffExact
expect_error(
  write_audio(
    df_test,
    sampling_rate = 150000,
    sylLen = 200,
    prefix = "TestPrefix",
    save_path = tmp_dir,
    rolloffExact = c("0.5", "0.25") # Character vector
  ),
  "The 'rolloffExact' argument must be a numeric vector or matrix."
)
# Test when formants is a list but missing required elements
expect_error(
  write_audio(
    df_test,
    sampling_rate = 150000,
    sylLen = 200,
    prefix = "TestPrefix",
    save_path = tmp_dir,
    formants = NULL, # missing
    vocalTract = NA
  ),
  "The formants can be NA, a numeric vector, or a list that contains the following elements: times, freqs, amps, bwds."
)

  # test error handling for vocalTract parameter
  expect_error(
    write_audio(
      df_test,
      sampling_rate = 150000,
      sylLen = 200,
      prefix = "TestPrefix",
      save_path = tmp_dir,
      formants = NA,
      vocalTract = "3" # not a numeric value
    ),
    "The 'vocalTract' can only be specified when formants is also specified."
  )
  # test the error handlng for vocalTract parameter
  expect_error(
    write_audio(
      df_test,
      sampling_rate = 150000,
      sylLen = 200,
      prefix = "TestPrefix",
      save_path = tmp_dir,
      formants =1,
      vocalTract = "3" # not a numeric value
    ),
    "When 'vocalTract' is provided, it must be a numeric value."
  )

  # test that prefix is not a character string
  expect_error(
    write_audio(
      df_test,
      sampling_rate = 150000,
      sylLen = 200,
      prefix = 1, # not a character string
      save_path = tmp_dir
    ),
    "The 'prefix' argument must be a character string."
  )
  # test that the invalidArgAction is not a character string or one of the three valid options
  expect_error(
    write_audio(
     df_test,
     sampling_rate = 150000,
     sylLen = 200,
     prefix = "TestPrefix",
     save_path = tmp_dir,
     invalidArgAction = "wrong_value" # This should trigger an error
  ),
  "The 'invalidArgAction' argument must be a character string and be one of 'ignore', 'adjust', or 'abort'."
)
  # test that the smoothing is not a list
  expect_error(
    write_audio(
      df_test,
      sampling_rate = 150000,
      sylLen = 200,
      prefix = "TestPrefix",
      save_path = tmp_dir,
      smoothing = "loess" # not a list
    ),
    "The 'smoothing' argument must be a list."
  )
  # test that the smoothing list does not contain the required elements
  expect_error(
    write_audio(
      df_test,
      sampling_rate = 150000,
      sylLen = 200,
      prefix = "TestPrefix",
      save_path = tmp_dir,
      smoothing = list(interpol = "loess", loessSpan = 1) # missing required element
    ),
    "The 'smoothing' argument must contain the following elements: interpol, loessSpan, discontThres, jumpThres."
  )

  unlink(tmp_dir, recursive = TRUE)
})