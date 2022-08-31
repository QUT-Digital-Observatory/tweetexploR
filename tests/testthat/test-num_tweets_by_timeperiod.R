# Database connection ####

# Connect to sqlite .db file
sqlite_con <- connect_to_sqlite_db(test_path("fixtures", "auspol-test.db"))


# Tests for when return_data = FALSE ####

test_that("result is a ggplot2 object (hourly plot)", {
  expect_true(ggplot2::is.ggplot(num_tweets_by_timeperiod(sqlite_con, "hour")))
})


test_that("result is a ggplot2 object (daily plot)", {
  expect_true(ggplot2::is.ggplot(num_tweets_by_timeperiod(sqlite_con, "day")))
})


test_that("result is a ggplot2 object (monthly plot)", {
  expect_true(ggplot2::is.ggplot(num_tweets_by_timeperiod(sqlite_con, "month")))
})


test_that("ggplot2 plot has expected output (hourly plot)", {
  vdiffr::expect_doppelganger("num_tweets_by_timeperiod_hour",
                              num_tweets_by_timeperiod(sqlite_con, "hour"))
})


test_that("ggplot2 plot has expected output (daily plot)", {
  vdiffr::expect_doppelganger("num_tweets_by_timeperiod_day",
                              num_tweets_by_timeperiod(sqlite_con, "day"))
})


test_that("ggplot2 plot has expected output (monthly plot)", {
  vdiffr::expect_doppelganger("num_tweets_by_timeperiod_month",
                             num_tweets_by_timeperiod(sqlite_con, "month"))
})


# Tests for when return_data = TRUE ####

results_hourly <- num_tweets_by_timeperiod(sqlite_con,
                                           period = "hour",
                                           return_data = TRUE)

results_daily <- num_tweets_by_timeperiod(sqlite_con,
                                          period = "day",
                                          return_data = TRUE)

results_monthly <- num_tweets_by_timeperiod(sqlite_con,
                                            period = "month",
                                            return_data = TRUE)


test_that("list of length 2 is created as expected (hourly plot)", {
  expect_type(results_hourly, "list")
  expect_equal(2, length(results_hourly))
})


test_that("first element of list (chart) is a list (hourly plot)", {
  expect_type(results_hourly$chart, "list")
})


test_that("second element of list (data) is a data frame (hourly plot)", {
  expect_true(is.data.frame(results_hourly$data))
})


test_that("ggplot2 plot has expected output (hourly plot)", {
  vdiffr::expect_doppelganger("num_tweets_by_timeperiod_hour",
                              results_hourly$chart)
})


test_that("list of length 2 is created as expected (daily plot)", {
  expect_type(results_daily, "list")
  expect_equal(2, length(results_daily))
})


test_that("first element of list (chart) is a list (daily plot)", {
  expect_type(results_daily$chart, "list")
})


test_that("second element of list (data) is a data frame (daily plot)", {
  expect_true(is.data.frame(results_daily$data))
})


test_that("ggplot2 plot has expected output (daily plot)", {
  vdiffr::expect_doppelganger("num_tweets_by_timeperiod_day",
                              results_daily$chart)
})


test_that("list of length 2 is created as expected (monthly plot)", {
  expect_type(results_monthly, "list")
  expect_equal(2, length(results_monthly))
})


test_that("first element of list (chart) is a list (monthly plot)", {
  expect_type(results_monthly$chart, "list")
})


test_that("second element of list (data) is a data frame (monthly plot)", {
  expect_true(is.data.frame(results_monthly$data))
})


test_that("ggplot2 plot has expected output (monthly plot)", {
  vdiffr::expect_doppelganger("num_tweets_by_timeperiod_month",
                              results_monthly$chart)
})


# Tests for when exclude_RT == TRUE ####

test_that("ggplot2 plot has expected output (hourly plot excl RT)", {
  vdiffr::expect_doppelganger("num_tweets_by_timeperiod_hour_excl_RT",
                              num_tweets_by_timeperiod(sqlite_con,
                                                       period = "hour",
                                                       exclude_RT = TRUE))
})


test_that("ggplot2 plot has expected output (daily plot excl RT)", {
  vdiffr::expect_doppelganger("num_tweets_by_timeperiod_day_excl_RT",
                              num_tweets_by_timeperiod(sqlite_con,
                                                       period = "day",
                                                       exclude_RT = TRUE))
})


test_that("ggplot2 plot has expected output (monthly plot excl RT)", {
  vdiffr::expect_doppelganger("num_tweets_by_timeperiod_month_excl_RT",
                              num_tweets_by_timeperiod(sqlite_con,
                                                       period = "month",
                                                       exclude_RT = TRUE))
})


# Disconnect from database ####

DBI::dbDisconnect(sqlite_con)
