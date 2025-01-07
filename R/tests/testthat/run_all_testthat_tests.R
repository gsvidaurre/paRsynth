# G.A. Juarez
# 4 Dec 2024

rm(list = ls())

if (!require(testthat)) install.packages('testthat')
library(testthat)
library(dplyr)
library(stringr)
library(rlang)
library(pbapply)

# Change the path for testing to reflect where the package is installed on your local machine
testing_path <- "~/Desktop/GitHub_repos/paRsynth/R"

# Change the desktop path to reflect where temporary directories for testing will be created to save files generated during testing
desktop_path <- "~/Desktop"

# Load the paRsynth functions that will be tested below
source(file.path(testing_path, "generate_strings.R"))
source(file.path(testing_path, "parsons_code.R"))
source(file.path(testing_path, "frequency_anchors.R"))
source(file.path(testing_path, "write_audio.R"))

# Run the testthat tests for each function
test_file(file.path(testing_path, "tests/testthat", "test_generate_strings.R"))

test_file(file.path(testing_path, "tests/testthat", "test_parsons_code.R"))

test_file(file.path(testing_path, "tests/testthat", "test_frequency_anchors.R"))

test_file(file.path(testing_path, "tests/testthat", "test_write_audio.R"))
