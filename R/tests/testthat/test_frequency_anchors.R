# R. Samman
# November 15 2024
# G.A. Juarez
# 5Dec24 - Grammar check

rm(list = ls())

if (!require(testthat)) install.packages('testthat')
library(testthat)

# Change "~Desktop/.../GitHub_repos" based on where paRsynth is stored
source("~/Desktop/BIRDS/GitHub_repos/paRsynth/R/frequency_anchors.R")

# 1. Unit test to check that the generation of df with multiple rows input
test_that("The function generates df with multiple rows data frame", {

  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("dplyr")
  withr::local_package("lubridate")

  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  # library(dplyr)

  # Example data frame for testing
  test_df <- data.frame(
    Group = c(1, 2),
    Individual = c(1, 2),
    Call_ID = c(1, 2),
    Call = c(1, 2),
    Parsons_Code = c("up-down-constant", "constant-up-up"),
    stringsAsFactors = FALSE
  )

  # Call the function with test df
  result <- frequency_anchors(df = test_df, parsons_col = "Parsons_Code", group_id_col = "Group", individual_id_col = "Individual", call_id_col = "Call_ID", call_string_col = "Call", starting_frequency = 4000, frequency_shift = 1000)

  # Test that the returned df (result) has the expected columns
  expect_true("Group" %in% colnames(result))
  expect_true("Individual" %in% colnames(result))
  expect_true("Call_ID" %in% colnames(result))
  expect_true("Call" %in% colnames(result))
  expect_true("Parsons_Code" %in% colnames(result))
  expect_true(any(grepl("Frequency", colnames(result))))

  # Test the number of frequency columns (should be one per "up", "down", or "constant" in the Parsons code, plus two for the starting and ending frequencies)
  expect_equal(ncol(result), 10)  # 5 metadata columns + 5 frequency columns

  # Check the first row's frequencies
  expect_equal(result$Frequency1[1], 4000) # starting frequency
  expect_equal(result$Frequency2[1], 5000) # "up" shifts by 1000 Hz
  expect_equal(result$Frequency3[1], 4000) # "down" shifts back by 1000 Hz
  expect_equal(result$Frequency4[1], 4000) # "constant"
  expect_equal(result$Frequency5[1], 4000) # back to starting frequency

  # Check the second row's frequencies
  expect_equal(result$Frequency1[2], 4000) # starting frequency
  expect_equal(result$Frequency2[2], 4000) # "constant"
  expect_equal(result$Frequency3[2], 5000) # "up" shifts by 1000 Hz
  expect_equal(result$Frequency4[2], 6000) # "up" shifts again by 1000 Hz
  expect_equal(result$Frequency5[2], 4000) # back to starting frequency

  print(str(result)) # checking the structure of the result df
  print(head(result)) # the first few rows of the result df
  print(colnames(result)) # checking the column names of the result df
})

# 2. Unit test to check that frequency directions shift
test_that("This function shifts up, constant, and down directions frequency correctly", {

  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("dplyr")
  withr::local_package("lubridate")

  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  # library(dplyr)

  # Example data frame for testing
  df <- data.frame(
    Group = c(1),
    Individual = c(2, 1),
    Call_ID = c(1, 2),
    Call = c("A", "B"),
    Parsons_Code = c("up-down-up-down", "down-constant-up-down"),
    stringsAsFactors = FALSE
  )
  starting_frequency <- 4000
  frequency_shift <- 1000

  # Call the function with test df
  result <- frequency_anchors(df = df, parsons_col = "Parsons_Code", group_id_col = "Group", individual_id_col = "Individual", call_id_col = "Call_ID", call_string_col = "Call", starting_frequency, frequency_shift)

  # Check frequencies for each step
  expect_equal(result$Frequency1[1], 4000) # starting frequency
  expect_equal(result$Frequency2[1], 5000) # up: 4000 + 1000
  expect_equal(result$Frequency3[1], 4000) # down: 5000 - 1000
  expect_equal(result$Frequency4[1], 5000) # up: 4000 + 1000
  expect_equal(result$Frequency5[1], 4000) # down: 5000 - 1000
  expect_equal(result$Frequency6[1], 4000) # back to starting frequency

  expect_equal(result$Frequency1[2], 4000) # starting frequency
  expect_equal(result$Frequency2[2], 3000) # Down: 4000 - 1000
  expect_equal(result$Frequency3[2], 3000) # constant: 3000
  expect_equal(result$Frequency4[2], 4000) # up: 3000 + 1000
  expect_equal(result$Frequency5[2], 3000) # down: 5000 - 1000
  expect_equal(result$Frequency6[2], 4000) # back to starting frequency

})

# 3. Unit test to check that it corrects negative or zero frequencies
test_that("The function corrects negative or zero frequencies", {

  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("dplyr")
  withr::local_package("lubridate")

  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  # library(dplyr)

  # Example data frame for testing
  df <- data.frame(
    Group = c(1),
    Individual = c(2, 1),
    Call_ID = c(1, 2),
    Call = c("A", "B"),
    Parsons_Code = c("down-down-down-down", "down-down-down-up"),
    stringsAsFactors = FALSE
  )
  starting_frequency <- 3000
  frequency_shift <- 1000

  # Call the function with test df
  result <- frequency_anchors(df = df, parsons_col = "Parsons_Code", group_id_col = "Group", individual_id_col = "Individual", call_id_col = "Call_ID", call_string_col = "Call", starting_frequency, frequency_shift)

  # Check frequencies for each step
  expect_equal(result$Frequency1[1], 3000) # starting frequency
  expect_equal(result$Frequency2[1], 2000) # down: 3000 - 1000
  expect_equal(result$Frequency3[1], 1000) # down: 2000 - 1000
  expect_equal(result$Frequency4[1], 1000) # down: 2000 - 1000 (corrected back to 1000)
  expect_equal(result$Frequency5[1], 1000) # down: 2000 - 1000 (corrected back to 1000)
  expect_equal(result$Frequency6[1], 3000) # back to starting point

  expect_equal(result$Frequency1[2], 3000) # starting frequency
  expect_equal(result$Frequency2[2], 2000) # down: 3000 - 1000
  expect_equal(result$Frequency3[2], 1000) # down: 2000 - 1000
  expect_equal(result$Frequency4[2], 1000) # down: 1000 - 1000 (corrected back to 1000)
  expect_equal(result$Frequency5[2], 2000) # up: 1000 + 1000
  expect_equal(result$Frequency6[2], 3000) # back to starting point

cat("Frequency5[1]", result$Frequency5[1], "\n")
cat("Frequency5[2]", result$Frequency5[2], "it should be 2000 \n") #this is where the error happens. the function seems to keep correcting the negative frequency once it happens
cat("Frequency6[2]", result$Frequency6[2],"\n")

})
