# R. Samman
# 2024-12-17
# updated: RS, 2025-01-31

rm(list = ls())
if (!require("testthat")) {
    install.packages("testthat")
}
library(testthat)

source("/Users/raneemsamman/Documents/GitHub/paRsynth/R/parsons_code.R")

# create a data frame for testing
test_df <- data.frame(
    Group = c(1, 2),
    Individual = c(1, 2),
    Call_ID = c(1, 2),
    Call = c(1, 2),
    Parsons_Code = c("up-down-constant", "constant-up-up"),
    stringsAsFactors = FALSE
)

run_check_error <- function(df, string_col, mapping, error_msg) {
    expect_error(
        parsons_code(df, string_col, global_head_col = "Group", group_head_col = "Individual", 
                     individual_middle_col = "Call_ID", random_variation_col = "Call", 
                     group_tail_col = "Parsons_Code", global_tail_col = "Parsons_Code",
                     mapping = mapping),
        error_msg
    )
}

test_that("Error handling for parsons_code", {
    # test that the input for df is actually a data frame
    run_check_error(
        df = "df", # not a data frame
        string_col = "Call",
        mapping = list("A" = "up", "B" = "down", "C" = "constant"),
        error_msg = "The 'df' argument must be a data frame."
    )
    # test that the input for mapping is actually a list
    run_check_error(
        df = test_df,
        string_col = "Call",
        mapping = "A", # not a list
        error_msg = "The 'mapping' argument must be a list."
    )
    # test that the column specified in string_col exists in the data frame
    run_check_error(
        df = test_df,
        string_col = "Call1", # column does not exist
        mapping = list("A" = "up", "B" = "down", "C" = "constant"),
        error_msg = "string_col provided does not exist in the data frame."
    )
    # test that the input for string_col is actually a character string

    run_check_error(
        df = test_df,
        string_col = 1, # not a character string
        mapping = list("A" = "up", "B" = "down", "C" = "constant"),
        error_msg = "The 'string_col' argument must be a character string."
    )
    run_check_error(
        df = test_df,
        string_col = call, # not a character string
        mapping = list("A" = "up", "B" = "down", "C" = "constant"),
        error_msg = "The 'string_col' argument must be a character string."
    )
    # test that the input data frame is not empty
    run_check_error(
        df = data.frame(), # empty data frame
        string_col = "Call",
        mapping = list("A" = "up", "B" = "down", "C" = "constant"),
        error_msg = "Input data frame is empty"
    )
    # test that the mapping list contains at least one element
    run_check_error(
        df = test_df,
        string_col = "Call",
        mapping = list(),
        error_msg = "The 'mapping' list must contain at least one element."
    )
})
