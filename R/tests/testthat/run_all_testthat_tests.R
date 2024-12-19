# G.A. Juarez
# 4 Dec 2024

rm(list = ls())

if (!require(testthat)) install.packages('testthat')
library(testthat)

# Change "Desktop/.../GitHub_repos" based on where paRsynth is stored on your local machine
testing_path <- "~/Desktop/BIRDS/GitHub_repos/paRsynth/R"

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