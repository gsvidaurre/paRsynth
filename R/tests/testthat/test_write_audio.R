# R. Samman
# Nov, 18, 2024
# G.A. Juarez
# 5Dec24 - Grammar check

# 1. Unit test to check for correct audio file creation
test_that("The function creates and removes audio files correctly", {

  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("soundgen")

  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  # library(dplyr)
  # library(soundgen)

  # Create a temporary directory on the Desktop for storing files (for test purposes)
  tmp_dir <- file.path(desktop_path, "R_test_temp")

  # Create the directory if it doesn't already exist
  if (!dir.exists(tmp_dir)) {
    dir.create(tmp_dir)
  }
  print(tmp_dir)

  # Example data frame for testing
  df_test <- data.frame(
    Group = c(1, 2),
    Individual = c(1, 2),
    Call_ID = c(1, 2),
    Frequency_1 = c(4000, 4500),
    Frequency_2 = c(5000, 5500)
  )

  # Call the function with test df
  result_df <- write_audio(df_test, save_path = tmp_dir, sampling_rate = 150000, sylLen = 200, prefix = "TestPrefix")

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

  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("soundgen")

  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  # library(dplyr)
  # library(soundgen)

  # Create a temporary directory on the Desktop for storing files (for test purposes)
  tmp_dir <- file.path(desktop_path, "R_test_temp")

  # Create the directory if it doesn't already exist
  if (!dir.exists(tmp_dir)) {
    dir.create(tmp_dir)
  }
  print(tmp_dir)

  # Example data frame for testing
  df_test <- data.frame(
    Group = c(1, 2),
    Individual = c(1, 2),
    Call_ID = c(1, 2),
    Frequency_1 = c(4000, 4500),
    Frequency_2 = c(5000, 5500)
  )

  # Call the function with test df
  result_df <- write_audio(df_test, save_path = tmp_dir, sampling_rate = 150000, sylLen = 200, prefix = "TestPrefix")

  # Test if the audio_file_name column exists and file names are correctly formatted
  expect_true("audio_file_name" %in% colnames(result_df))

  # Check the naming format for the first row
  expected_filename <- paste0("TestPrefix", "_Group", df_test$Group[1], "_Ind", df_test$Individual[1],"_Call", df_test$Call_ID[1], ".wav")
  expect_equal(result_df$audio_file_name[1], expected_filename)

  # Remove the created directory and files
  if (dir.exists(tmp_dir) && tmp_dir == file.path(desktop_path, "R_test_temp")) {
    unlink(tmp_dir, recursive = TRUE)
    }
})

# 3. Unit test to check whether the resulting files contain the correct file extension
test_that("The function creates files that have the correct .wav extension", {

  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("soundgen")

  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  # library(dplyr)
  # library(soundgen)

  # Create a temporary directory on the Desktop for storing files (for test purposes)
  tmp_dir <- file.path(desktop_path, "R_test_temp")

  # Create the directory if it doesn't already exist
  if (!dir.exists(tmp_dir)) {
    dir.create(tmp_dir)
  }
  print(tmp_dir)

  # Example data frame for testing
  df_test <- data.frame(
    Group = c(1, 2),
    Individual = c(1, 2),
    Call_ID = c(1, 2),
    Frequency_1 = c(4000, 4500),
    Frequency_2 = c(5000, 5500)
  )

  # Call the function with test df
  result_df <- write_audio(df_test, save_path = tmp_dir, sampling_rate = 150000, sylLen = 200, prefix = "TestPrefix")

  # Check that all generated files have a .wav extension
  audio_files <- result_df$audio_file_name
  expect_true(all(grepl("\\.wav$", audio_files)))

  # Remove the created directory and files
  if (dir.exists(tmp_dir) && tmp_dir == file.path(desktop_path, "R_test_temp")) {
    unlink(tmp_dir, recursive = TRUE)
    }
})
