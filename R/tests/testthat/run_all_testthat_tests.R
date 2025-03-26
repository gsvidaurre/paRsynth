# Author: G.A. Juarez and Raneem Samman
# Date created: December 4, 2024

rm(list = ls())

# make a list of packages to install
pkgs <- c("testthat", "soundgen", "dplyr", "stringr", "rlang", "tidyverse", "lubridate", "pbapply")

# check if the packages are installed, if not, install them
for (pkg in pkgs) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg, dependencies = TRUE)
    }
}
# load the packages (for software development purposes)
# lapply(pkgs, library, character.only = TRUE)

# Change the path for testing to reflect where the package is installed on your local machine
testing_path <- "/Users/raneemsamman/Documents/GitHub/paRsynth/R" #raneem's
# testing_path <- "~/Desktop/GitHub_repos/paRsynth/R" #alexandra's 

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

test_file(file.path(testing_path, "tests/testthat", "test_generate_strings_error_handling.R"))
test_file(file.path(testing_path, "tests/testthat", "test_parsons_code_error_handling.R"))
test_file(file.path(testing_path, "tests/testthat", "test_frequency_anchors_error_handling.R"))
test_file(file.path(testing_path, "tests/testthat", "test_write_audio_error_handling.R"))
