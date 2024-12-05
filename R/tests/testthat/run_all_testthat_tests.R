# G.A. Juarez
# 4 Dec 2024

rm(list = ls())

if (!require(testthat)) install.packages('testthat')
library(testthat)

# Change "Desktop/.../GitHub_repos" based on where paRsynth is stored
testing_path <- "~/Desktop/BIRDS/GitHub_repos/paRsynth/R/tests/testthat/"

# Run the testthat tests for each function
test_file(file.path(testing_path, "test_generate_strings.R"))

test_file(file.path(testing_path, "test_parsons_code.R"))

test_file(file.path(testing_path, "test_frequency_anchors.R"))

test_file(file.path(testing_path, "test_write_audio.R"))
