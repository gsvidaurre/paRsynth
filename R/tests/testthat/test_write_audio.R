# R. Samman
# Nov, 18, 2024

# RS NOTE: soundgen doesnt work for me so I have no idea if those unit tests are correct or not. PLease let me know if they are not or if they pass!

source("/Users/raneemsamman/Documents/GitHub/paRsynth/R/write_audio.R")
library(testthat)
desktop_path <- "/Users/raneemsamman/Desktop"

# Unit Test 1: Test for correct audio file creation
test_that("Audio files are correctly created and removed", {
  
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
    Call = c(1, 2),
    Frequency_1 = c(4000, 4500),
    Frequency_2 = c(5000, 5500)
  )

  result_df <- write_audio(df_test, save_path = tmp_dir, sampling_rate = 150000, sylLen = 200, prefix = "TestPrefix")

  # ensure files are created
  audio_files <- result_df$audio_file_name
  expect_true(all(file.exists(file.path(tmp_dir, audio_files))))

  # Remove the files created during the test
  file.remove(file.path(tmp_dir, audio_files))

  # Remove the test directory
  unlink(tmp_dir, recursive = TRUE)
})


# Unit Test 3: Test for correct df creation (must add an additional column -audio_file_name- for file names with a correct naming format) 
test_that("Data frame contains correct audio file name format", {  
  # Create a temporary directory on the Desktop for storing files (for test purposes)
  tmp_dir <- file.path(desktop_path, "R_test_temp")
  
  # Create the directory if it doesn't already exist
  if (!dir.exists(tmp_dir)) {
    dir.create(tmp_dir)
  }
  print(tmp_dir)
  
  df_test <- data.frame(
    Group = c(1, 2),
    Individual = c(1, 2),
    Call = c(1, 2),
    Frequency_1 = c(4000, 4500),
    Frequency_2 = c(5000, 5500)
  )
  result_df <- write_audio(df_test, save_path = tmp_dir, sampling_rate = 150000, sylLen = 200, prefix = "TestPrefix")

  # testing if the audio_file_name column exists and filenames are correctly formatted
  expect_true("audio_file_name" %in% colnames(result_df))
  
  # Check the naming format for the first row
  expected_filename <- paste0("TestPrefix_Group", df_test$Group[1], "_Ind", df_test$Individual[1], "_Call", df_test$Call[1], ".wav")
  expect_equal(result_df$audio_file_name[1], expected_filename)

  # Remove the created files
  unlink(tmp_dir, recursive = TRUE)
})

# Unit Test 4: Test whether the resulting files contain the correct file extentsion
test_that("Generated files have the correct .wav extension", {
  
  # Create a temporary directory on the Desktop for storing files (for test purposes)
  tmp_dir <- file.path(desktop_path, "R_test_temp")
  
  # Create the directory if it doesn't already exist
  if (!dir.exists(tmp_dir)) {
    dir.create(tmp_dir)
  }
  print(tmp_dir)

  df_test <- data.frame(
    Group = c(1, 2),
    Individual = c(1, 2),
    Call = c(1, 2),
    Frequency_1 = c(4000, 4500),
    Frequency_2 = c(5000, 5500)
  )

  result_df <- write_audio(df_test, save_path = tmp_dir, sampling_rate = 150000, sylLen = 200, prefix = "TestPrefix")

  # check that all generated files have a .wav extension
  audio_files <- result_df$audio_file_name
  expect_true(all(grepl("\\.wav$", audio_files)))

  # Remove the created files
  unlink(tmp_dir, recursive = TRUE)
})

# Unit Test 5: read one or two back in with readWave() from the tuneR package to check whether an object of class "Wave" is created
test_that("Generated files can be read as Wave objects", {

  # Create a temporary directory on the Desktop for storing files (for test purposes)
  tmp_dir <- file.path(desktop_path, "R_test_temp")
  
  # Create the directory if it doesn't already exist
  if (!dir.exists(tmp_dir)) {
    dir.create(tmp_dir)
  }
  print(tmp_dir)

  df_test <- data.frame(
    Group = c(1, 2),
    Individual = c(1, 2),
    Call = c(1, 2),
    Frequency_1 = c(4000, 4500),
    Frequency_2 = c(5000, 5500)
  )

  result_df <- write_audio(df_test, save_path = tmp_dir, sampling_rate = 150000, sylLen = 200, prefix = "TestPrefix")

  # check if the files can be read with readWave
  audio_files <- result_df$audio_file_name
  audio_paths <- file.path(tmp_dir, audio_files)
  
  # trying to read the first audio file using readWave
  library(tuneR)
  wave_obj <- readWave(audio_paths[1])
  
  # ensure that a Wave object is returned
  expect_s3_class(wave_obj, "Wave")

  # Remove the created files
  unlink(tmp_dir, recursive = TRUE)
})