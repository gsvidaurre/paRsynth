# R. Samman
# 2024-12-17

rm(list = ls())
if (!require("testthat")) {
    install.packages("testthat")
}
library(testthat)

source("/Users/raneemsamman/Documents/GitHub/paRsynth/R/parsons_code.R")

test_that("Error handling for parsons_code", {
    test_df <- data.frame(
        Group = c(1, 2),
        Individual = c(1, 2),
        Call_ID = c(1, 2),
        Call = c(1, 2),
        Parsons_Code = c("up-down-constant", "constant-up-up"),
        stringsAsFactors = FALSE
    )
    # test that the input for df is actually a data frame
    expect_error(
        parsons_code(
            "df", # not a data frame
            "Call",
            list("A" = "up", "B" = "down", "C" = "constant")
        ),
        "The 'df' argument must be a data frame."
    )
    # test that the input for mapping is actually a list
    expect_error(
        parsons_code(
            test_df,
            "Call", 
            mapping = "A" # not a list
        ),
        "The 'mapping' argument must be a list."
    )
    # test that the column specified in string_col exists in the data frame
    expect_error(
        parsons_code(
            test_df,
            "Call1", # column does not exist
            list("A" = "up", "B" = "down", "C" = "constant")
        ),
        "string_col provided does not exist in the data frame."
    )
    # test that the input for string_col is actually a character string
    expect_error(
        parsons_code(
            test_df,
            call, # not a character string
            list("A" = "up", "B" = "down", "C" = "constant")
        ),
        "The 'string_col' argument must be a character string."
    )
    expect_error(
        parsons_code(
            test_df,
            1, # not a character string
            list("A" = "up", "B" = "down", "C" = "constant")
        ),
        "The 'string_col' argument must be a character string."
    )
    # test that the input data frame is not empty
    expect_error(
        parsons_code(
            df = data.frame(), # empty data frame
            "Call",
            list("A" = "up", "B" = "down", "C" = "constant")
        ),
        "Input data frame is empty"
    )
    # test that the mapping list contains at least one element
    expect_error(
        parsons_code(
            test_df,
            "Call",
            mapping = list() # empty list
        ),
        "The 'mapping' list must contain at least one element."
    )
})