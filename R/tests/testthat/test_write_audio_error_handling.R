#R. Samman
#2024-12-17
# updated: RS 2025-02-03

rm(list = ls())

if (!require(testthat)) {
  install.packages("testthat")
}
library(testthat)
library(soundgen)
library(pbapply)

source("/Users/raneemsamman/Documents/GitHub/paRsynth/R/write_audio.R")

desktop_path <- "~/Desktop"

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
    "sampling_rate must be a positive value"
  )
  expect_error(
    write_audio(
      df_test,
      sampling_rate = 0, # not a positive value
      sylLen = 200,
      prefix = "TestPrefix",
      save_path = tmp_dir
    ),
    "sampling_rate must be a positive value"
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
    "sylLen must be a positive value"
  )
  expect_error(
    write_audio(
      df_test,
      sampling_rate = 150000,
      sylLen = 0, # not a positive value
      prefix = "TestPrefix",
      save_path = tmp_dir
    ),
    "sylLen must be a positive value"
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
    "The 'sampling_rate' argument must be a numeric value."
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
    "The 'sylLen' argument must be a numeric value."
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

  unlink(tmp_dir, recursive = TRUE)
})