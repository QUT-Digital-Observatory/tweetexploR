# Tests for hourly plot ####

test_that("warning is thrown when input for hourly plot is not valid", {

  expect_silent(check_for_valid_subset_input(period = "hour",
                                             input = "2022-06-20 06:00:00"))

  expect_warning(check_for_valid_subset_input(period = "hour",
                                              input = "2022-06-33 06:00:00"))

  expect_warning(check_for_valid_subset_input(period = "hour",
                                              input = "2022-06-30"))

  expect_warning(check_for_valid_subset_input(period = "hour",
                                              input = "2022-06"))
})


# Tests for daily plot ####


test_that("warning is thrown when input for daily plot is not valid", {

  expect_silent(check_for_valid_subset_input(period = "day",
                                             input = "2022-06-20"))

  expect_warning(check_for_valid_subset_input(period = "day",
                                              input = "2022-06-30 06:00:00"))

  expect_warning(check_for_valid_subset_input(period = "day",
                                              input = "2022-06-33"))

  expect_warning(check_for_valid_subset_input(period = "day",
                                              input = "2022-06"))
})


# Tests for monthly plot ####

test_that("warning is thrown when input for monthly plot is not valid", {

  expect_silent(check_for_valid_subset_input(period = "month",
                                             input = "2022-06"))

  expect_warning(check_for_valid_subset_input(period = "month",
                                              input = "2022-13"))

  expect_warning(check_for_valid_subset_input(period = "month",
                                              input = "2022-06-01"))

  expect_warning(check_for_valid_subset_input(period = "month",
                                              input = "2022-06-01 01:00:00"))
})
