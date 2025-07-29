# Author: Raneem Samman
# Date created: December 17, 2024

test_that("Error handling for frequency_anchors", {
    test_df <- data.frame(
        Group = c(1, 2),
        Individual = c(1, 2),
        Call_ID = c(1, 2),
        Call = c(1, 2),
        Parsons_Code = c("up-down-constant", "constant-up-up"),
        stringsAsFactors = FALSE
    )
    # include everything from df_test except Call_ID
    test_df2 <- test_df[, !names(test_df) %in% "Call_ID"]

    test_cases <- list(
        # test that the starting_frequency is not a positive value
        list(param = list(starting_frequency = 0, frequency_shift = 1000), expected_error = "starting_frequency must be a positive value"),
        list(param = list(starting_frequency = -200, frequency_shift = 1000), expected_error = "starting_frequency must be a positive value"),

        # test that frequency_shift must be a positive value
        list(param = list(starting_frequency = 4000, frequency_shift = 0), expected_error = "frequency_shift must be a positive value"),
        list(param = list(starting_frequency = 4000, frequency_shift = -200), expected_error = "frequency_shift must be a positive value"),

        # test that Input data frame is not empty
        list(param = list(starting_frequency = 4000, frequency_shift = 1000, df = data.frame()), expected_error = "Input data frame is empty"),

        # test that One or more columns were not found in the data frame
        list(param = list(starting_frequency = 4000, frequency_shift = 1000, df = test_df2), expected_error = "One or more columns were not found in the data frame"),

        # test that section_transition must be 'starting_frequency' or 'continuous_trajectory'
        list(param = list(starting_frequency = 4000, frequency_shift = 1000, section_transition = "continuous"), expected_error = "section_transition must be 'starting_frequency' or 'continuous_trajectory'"),
        list(param = list(starting_frequency = 4000, frequency_shift = 1000, section_transition = "starting frequency"), expected_error = "section_transition must be 'starting_frequency' or 'continuous_trajectory'")
    )

    for (case in test_cases) {
        params <- case$param # get the parameters for the test case
        expected_error <- case$expected_error # get the expected error message

        # provide default df if not defined in a case
        df <- if (is.null(params$df)) test_df else params$df
        # provide default section_transition if not defined in a case
        section_transition <- if (is.null(params$section_transition)) "starting_frequency" else params$section_transition 

        # test that the function throws an error
        expect_error(
            frequency_anchors(
                df = df,
                parsons_col = "Parsons_Code",
                group_id_col = "Group",
                individual_id_col = "Individual",
                call_id_col = "Call_ID",
                call_string_col = "Call",
                string_structure_col = "String_structure",
                starting_frequency = params$starting_frequency,
                frequency_shift = params$frequency_shift,
                section_transition = section_transition
            ),
            expected_error
        )
    }
})
