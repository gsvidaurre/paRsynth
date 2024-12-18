# R. Samman
# 2024-12-17

rm(list = ls())

if (!require(testthat)) {
    install.packages("testthat")
}
library(testthat)

source("/Users/raneemsamman/Documents/GitHub/paRsynth/R/frequency_anchors.R")

test_that("Error handling for frequency_anchors", {
    test_df <- data.frame(
        Group = c(1, 2),
        Individual = c(1, 2),
        Call_ID = c(1, 2),
        Call = c(1, 2),
        Parsons_Code = c("up-down-constant", "constant-up-up"),
        stringsAsFactors = FALSE
    )
    test_df2 <- data.frame(
        Group = c(1, 2),
        Individual = c(1, 2),
        # missing Call_ID
        Call = c(1, 2),
        Parsons_Code = c("up-down-constant", "constant-up-up"),
        stringsAsFactors = FALSE
    )
    # test that the starting_frequency is greater than 0
    expect_error(
        frequency_anchors(
            df = test_df,
            parsons_col = "Parsons_Code",
            group_id_col = "Group",
            individual_id_col = "Individual",
            call_id_col = "Call_ID",
            call_string_col = "Call",
            starting_frequency = 0, # starting frequency is 0
            frequency_shift = 1000
        ),
        "starting_frequency must be a positive value"
    )
    expect_error(
        frequency_anchors(
            df = test_df,
            parsons_col = "Parsons_Code",
            group_id_col = "Group",
            individual_id_col = "Individual",
            call_id_col = "Call_ID",
            call_string_col = "Call",
            starting_frequency = -200, # starting frequency is negative
            frequency_shift = 1000
        ),
        "starting_frequency must be a positive value"
    )
    # test that frequency_shift must be a positive value
    expect_error(
        frequency_anchors(
            df = test_df,
            parsons_col = "Parsons_Code",
            group_id_col = "Group",
            individual_id_col = "Individual",
            call_id_col = "Call_ID",
            call_string_col = "Call",
            starting_frequency = 4000,
            frequency_shift = 0 # frequency shift is 0
        ),
        "frequency_shift must be a positive value"
    )
    expect_error(
        frequency_anchors(
            df = test_df,
            parsons_col = "Parsons_Code",
            group_id_col = "Group",
            individual_id_col = "Individual",
            call_id_col = "Call_ID",
            call_string_col = "Call",
            starting_frequency = 4000,
            frequency_shift = -200 # frequency shift is 0
        ),
        "frequency_shift must be a positive value"
    )
    # test that Input data frame is not empty
    expect_error(
        frequency_anchors(
            df = data.frame(), # empty data frame
            parsons_col = "Parsons_Code",
            group_id_col = "Group",
            individual_id_col = "Individual",
            call_id_col = "Call_ID",
            call_string_col = "Call",
            starting_frequency = 4000,
            frequency_shift = 1000
        ),
        "Input data frame is empty"
    )
    # test that One or more columns were not found in the data frame
    expect_error(
        frequency_anchors(
            df = test_df2,
            parsons_col = "Parsons_Code",
            group_id_col = "Group",
            individual_id_col = "Individual",
            call_id_col = "Call_ID", # missing Call_ID
            call_string_col = "Call",
            starting_frequency = 4000,
            frequency_shift = 1000
        ),
        "One or more columns were not found in the data frame"
    )
    # test that section_transition must be 'starting_frequency' or 'continuous_trajectory'
    expect_error(
        frequency_anchors(
            df = test_df,
            parsons_col = "Parsons_Code",
            group_id_col = "Group",
            individual_id_col = "Individual",
            call_id_col = "Call_ID",
            call_string_col = "Call",
            starting_frequency = 4000,
            frequency_shift = 1000,
            section_transition = "continuous" # not a valid section_transition option
        ),
        "section_transition must be 'starting_frequency' or 'continuous_trajectory'"
    )
    expect_error(
        frequency_anchors(
            df = test_df,
            parsons_col = "Parsons_Code",
            group_id_col = "Group",
            individual_id_col = "Individual",
            call_id_col = "Call_ID",
            call_string_col = "Call",
            starting_frequency = 4000,
            frequency_shift = 1000,
            section_transition = "starting frequency" # not a valid section_transition option
        ),
        "section_transition must be 'starting_frequency' or 'continuous_trajectory'"
    )
})
