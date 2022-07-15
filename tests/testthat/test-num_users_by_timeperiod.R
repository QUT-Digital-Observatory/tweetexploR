# Connect to sqlite .db file
sqlite_con <- connect_to_sqlite_db(test_path("fixtures", "auspol-test.db"))


test_that("result is a ggplot2 object (hourly plot)", {
  expect_true(ggplot2::is.ggplot(num_users_by_timeperiod(sqlite_con, "hour")))
})


test_that("result is a ggplot2 object (daily plot)", {
  expect_true(ggplot2::is.ggplot(num_users_by_timeperiod(sqlite_con, "day")))
})


# test_that("result is a ggplot2 object (monthly plot)", {
#   expect_true(ggplot2::is.ggplot(num_users_by_timeperiod(sqlite_con, "month")))
# })


test_that("ggplot2 plot has expected output (hourly plot)", {
  vdiffr::expect_doppelganger("num_users_by_timeperiod_hour",
                              num_users_by_timeperiod(sqlite_con, "hour"))
})


test_that("ggplot2 plot has expected output (daily plot)", {
  vdiffr::expect_doppelganger("num_users_by_timeperiod_day",
                              num_users_by_timeperiod(sqlite_con, "day"))
})


# test_that("ggplot2 plot has expected output (monthly plot)", {
#   vdiffr::expect_doppelganger("num_users_by_timeperiod_month",
#                               num_users_by_timeperiod(sqlite_con, "month"))
# })


# Disconnect from database
DBI::dbDisconnect(sqlite_con)
