# Author: Raneem Samman
# Date created: December 17, 2024

# create a data frame for testing
test_df <- data.frame(
    Call = c("up-down-down-up-down-up-down-constant-up-up", "down-down-up-constant-up-constant-up-up-constant-down"),
    Global = c("up", "down"),
    Group = c("down-down-constant-up", "down-up-up-constant"),
    Individual = c("up-down", "constant-up"),
    Individual_head = c("up", "constant"),
    Individual_tail = c("down", "up"),
    Group_head = c("down-down", "down-up"),
    Group_tail = c("constant-up", "up-constant"),
    Random = c("up-down", "constant-up"),
    Individual = c(1, 2),
    Call_ID = c(1, 2),
    Call = c(1, 2),
    Structure = "GI-II-RV-GI",
    mapping = list("A" = "up", "B" = "down", "C" = "constant"),
    stringsAsFactors = FALSE
)

run_check_error <- function(df = "test_df", 
                            string_col = "Call",
                            global_head_col = "Global_head", 
                            group_head_col = "Group_head", 
                            individual_head_col = "Individual_head", 
                            individual_tail_col = "Individual_tail",
                            individual_complete_col = "Individual_complete",
                            group_complete_col = "Group_complete",
                            random_variation_col = "Random_variation", 
                            group_tail_col = "Group_tail", 
                            global_tail_col = "Global_tail",
                            string_structure_col = "Structure",
                            mapping = list("A" = "up", "B" = "down", "C" = "constant"), 
                            error_msg)
                    {
    expect_error(
        parsons_code(df, 
                     string_col, 
                     global_head_col, 
                     group_head_col, 
                     individual_head_col, 
                     individual_tail_col,
                     individual_complete_col, 
                     group_complete_col, 
                     random_variation_col,
                     group_tail_col, 
                     global_tail_col, 
                     string_structure_col,
                     mapping),
        error_msg
    )
}

test_that("Error handling for parsons_code", {
    # test that the input for df is actually a data frame
    run_check_error(
        df = "df", # not a data frame
        error_msg = "The 'df' argument must be a data frame."
    )
    # test that the mapping argument cannot be NULL
    run_check_error(
      mapping = NULL,
      error_msg = "The 'mapping' argument must not be NULL."
    )
    # test that the input for mapping is actually a list
    run_check_error(
        df = test_df,
        mapping = "A", # not a list
        error_msg = "The 'mapping' argument must be a list."
    )
    # test that the string structure is valid
    run_check_error(
      df = test_df,
      string_structure = "GI-GI-GI", # invalid string structure
      error_msg = "At least one of the string columns provided does not exist in the data frame."
    )
    # test that the string column specified exists in the data frame
    invalid_string_cols <- list(
        list("string_col", "Call1"),
        list("global_head_col", "Gloval"),
        list("group_head_col", "Group2"),
        list("individual_head_col", "individual2"),
        list("individual_tail_col", "individual2"),
        list("individual_complete_col", "individual2"),
        list("group_complete_col", "Group2"),
        list("random_variation_col", "rando"),
        list("group_tail_col", "Group2"),
        list("global_tail_col", "Gloval")
    )

    lapply(invalid_string_cols, function(invalid_string_col) {
        param <- invalid_string_col[[1]]
        value <- invalid_string_col[[2]]

        run_check_error(
            df = test_df,
            string_col = if (param == "string_col") value else "Call",
            global_head_col = if (param == "global_head_col") value else "Global_head",
            group_head_col = if (param == "group_head_col") value else "Group_head",
            individual_head_col = if (param == "individual_head_col") value else "Indiviudal_head",
            individual_tail_col = if (param == "individual_tail_col") value else "Individual_tail",
            individual_complete_col = if (param == "individual_complete_col") value else "Individual_complete",
            group_complete_col = if (param == "group_complete_col") value else "Group_complete",
            random_variation_col = if (param == "random_variation_col") value else "Random_variation",
            group_tail_col = if (param == "group_tail_col") value else "Group_tail",
            global_tail_col = if (param == "global_tail_col") value else "Global_tail",
            string_structure_col = if (param == "string_structure_col") value else "Structure",
            mapping = list("A" = "up", "B" = "down", "C" = "constant"),
            error_msg = "At least one of the string columns provided does not exist in the data frame."
        )
    })
    
    # test that the input for string_col and other columns are character strings
    invalid_cases <- list(
        list("string_col" = 1),
        list("string_col" = call),
        list("global_head_col" = 1),
        list("global_head_col" = call),
        list("group_head_col" = 1),
        list("group_head_col" = call),
        list("individual_head_col" = 1),
        list("individual_head_col" = call),
        list("individual_tail_col" = 1),
        list("individual_tail_col" = call),
        list("individual_complete_col" = 1),
        list("individual_complete_col" = call),
        list("group_complete_col" = 1),
        list("group_complete_col" = call),
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
            individual_head_col = invalid_case$individual_head_col,
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
        error_msg = "The 'mapping' argument must contain at least three elements."
    )
    
    # test that mapping list is characters
    run_check_error(
      df = test_df,
      mapping = list(1, 2, 3),
      error_msg = "All elements in the 'mapping' list must be character strings"
    )
    
})
