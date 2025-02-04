# R. Samman
# Nov, 18, 2024
# G.A. Juarez
# 5Dec24 - Grammar check
# updated: RS 2025-01-31

source("/Users/raneemsamman/Documents/GitHub/paRsynth/R/write_audio.R")

# Just for code development
library(tidyverse)
library(lubridate)
library(soundgen)
library(testthat)
library(dplyr)
library(pbapply)
desktop_path <- "~/Desktop"

  # helper function to create the data and directory if it doesn't already exist 
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

# 1. Unit test to check for correct audio file creation
test_that("The function creates and removes audio files correctly", {

  # Create a temporary directory on the Desktop for storing files (for test purposes)
  test_data <- create_test_data(desktop_path)
  df_test <- test_data$df_test
  tmp_dir <- test_data$tmp_dir
  result_df <- test_data$result_df

  # Ensure files are created
  audio_files <- result_df$audio_file_name
  expect_true(all(file.exists(file.path(tmp_dir, audio_files))))

  # Remove the test directory (and since recursive = TRUE, all files within it too)
  if (dir.exists(tmp_dir) && tmp_dir == file.path(desktop_path, "R_test_temp")) {
    unlink(tmp_dir, recursive = TRUE)
  }
})

# 2. Unit test to check for correct df creation (must add an additional column -audio_file_name- for file names with a correct naming format)
test_that("The function creates data frame with correct audio file name format", {

  # Create a temporary directory on the Desktop for storing files (for test purposes)
  test_data <- create_test_data(desktop_path)
  df_test <- test_data$df_test
  tmp_dir <- test_data$tmp_dir
  result_df <- test_data$result_df

  # Test if the audio_file_name column exists and file names are correctly formatted
  expect_true("audio_file_name" %in% colnames(result_df))

  # Check the naming format for the first row
  for (i in 1:nrow(df_test)){
    expected_filename <- paste0("TestPrefix", "_Group", df_test$Group[i], "_Indiv", df_test$Individual[i], "_Call", df_test$Call_ID[i], ".wav")
    expect_equal(result_df$audio_file_name[i], expected_filename, info = paste("The audio file name is incorrect for row", i))
  }
  # Remove the created directory and files
  if (dir.exists(tmp_dir) && tmp_dir == file.path(desktop_path, "R_test_temp")) {
    unlink(tmp_dir, recursive = TRUE)
  }
})

# 3. Unit test to check whether the resulting files contain the correct file extension
test_that("The function creates files that have the correct .wav extension", {

  # Create a temporary directory on the Desktop for storing files (for test purposes)
  test_data <- create_test_data(desktop_path)
  df_test <- test_data$df_test
  tmp_dir <- test_data$tmp_dir
  result_df <- test_data$result_df

  # Check that all generated files have a .wav extension
  audio_files <- result_df$audio_file_name
  expect_true(all(grepl("\\.wav$", audio_files)))

  # Remove the created directory and files
  if (dir.exists(tmp_dir) && tmp_dir == file.path(desktop_path, "R_test_temp")) {
    unlink(tmp_dir, recursive = TRUE)
    }
})