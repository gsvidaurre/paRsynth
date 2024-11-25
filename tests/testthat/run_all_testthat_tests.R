# G.A. Juarez
# 25 Nov 2024

rm(list = ls())

if (!require(testthat)) install.packages('testthat')
library(testthat)

testing_path <- "~/Desktop/GitHub_repos/paRsynth/tests/testthat/"

# Run the testthat tests for each function

test_file(file.path(testing_path, "test_generate_strings_new.R"))

test_file(file.path(testing_path, "test_parsons_code.R"))

test_file(file.path(testing_path, "test_frequency_anchors.R"))

test_file(file.path(testing_path, "test_write_audio.R"))