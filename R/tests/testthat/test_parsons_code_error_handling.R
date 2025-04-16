# Author: Raneem Samman
# Date created: December 17, 2024

# create a data frame for testing
test_df <- data.frame(
    Call = c("up-down-constant", "constant-up-up"),
    Global = c("up", "down"),
    Group = c("down-down-constant", "down-up-up"),
    Individual = c("up-down-constant", "constant-up-up"),
    Random = c("up-down-constant", "constant-up-up"),
    groupT = c("down-down-constant", "down-up-up"),
    tail = c("up-down-constant", "constant-up-up"),
    Individual = c(1, 2),
    Call_ID = c(1, 2),
    Call = c(1, 2),
    stringsAsFactors = FALSE
)

run_check_error <- function(df = "test_df", string_col = "Call",
                            global_head_col = "Global", group_head_col = "Group", 
                            individual_middle_col = "Individual", random_variation_col = "Random", 
                            group_tail_col = "groupT", global_tail_col = "tail", mapping = list("A" = "up", "B" = "down", "C" = "constant"), 
                            error_msg)
                    {
    expect_error(
        parsons_code(df, string_col, global_head_col, group_head_col,
                     individual_middle_col, random_variation_col,
                     group_tail_col, global_tail_col, mapping),
        error_msg
    )
}

test_that("Error handling for parsons_code", {
    # test that the input for df is actually a data frame
    run_check_error(
        df = "df", # not a data frame
        error_msg = "The 'df' argument must be a data frame."
    )
    # test that the input for mapping is actually a list
    run_check_error(
        df = test_df,
        mapping = "A", # not a list
        error_msg = "The 'mapping' argument must be a list."
    )
    # test that the string column specified exists in the data frame
    invalid_string_cols <- list(
        list("string_col", "Call1"),
        list("global_head_col", "Gloval"),
        list("group_head_col", "Group2"),
        list("individual_middle_col", "individual2"),
        list("random_variation_col", "rando"),
        list("group_tail_col", "Group_tail"),
        list("global_tail_col", "Global_tail")
    )

    lapply(invalid_string_cols, function(invalid_string_col) {
        param <- invalid_string_col[[1]]
        value <- invalid_string_col[[2]]

        run_check_error(
            df = test_df,
            string_col = if (param == "string_col") value else "Call",
            global_head_col = if (param == "global_head_col") value else "Global",
            group_head_col = if (param == "group_head_col") value else "Group",
            individual_middle_col = if (param == "individual_middle_col") value else "Individual",
            random_variation_col = if (param == "random_variation_col") value else "Random",
            group_tail_col = if (param == "group_tail_col") value else "groupT",
            global_tail_col = if (param == "global_tail_col") value else "tail",
            error_msg = "One of the string columns provided does not exist in the data frame."
        )
    })

    # test that the input for string_col and other columns are character strings
    invalid_cases <- list(
        list("string_col" = 1),
        list("string_col" = call),
        list("global_head_col" = 1),
        list("group_head_col" = call),
        list("individual_middle_col" = 1),
        list("individual_middle_col" = call),
        list("random_variation_col" = 1),
        list("random_variation_col" = call),
        list("group_tail_col" = 1),
        list("group_tail_col" = call),
        list("global_tail_col" = 1),
        list("global_tail_col" = call)
    )
    lapply(invalid_cases, function(invalid_case) {
        run_check_error(
            df = test_df,
            string_col = invalid_case$string_col,
            global_head_col = invalid_case$global_head_col,
            group_head_col = invalid_case$group_head_col,
            individual_middle_col = invalid_case$individual_middle_col,
            random_variation_col = invalid_case$random_variation_col,
            group_tail_col = invalid_case$group_tail_col,
            global_tail_col = invalid_case$global_tail_col,
            error_msg = "All vocalization columns argument must be a character string."
        )
    })
    
    # test that the input data frame is not empty
    run_check_error(
        df = data.frame(), # empty data frame
        error_msg = "Input data frame is empty"
    )
    # test that the mapping list contains at least one element
    run_check_error(
        df = test_df,
        mapping = list(),
        error_msg = "The 'mapping' list must contain at least one element."
    )
})
