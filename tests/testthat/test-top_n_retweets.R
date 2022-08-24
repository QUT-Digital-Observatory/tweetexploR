# Connect to sqlite .db file
sqlite_con <- connect_to_sqlite_db(test_path("fixtures", "auspol-test.db"))


# Tests for when return_data = FALSE

test_that("result is a ggplot2 object", {
  expect_true(ggplot2::is.ggplot(top_n_retweets(sqlite_con, 10)))
})


test_that("ggplot2 plot has expected output", {
  vdiffr::expect_doppelganger("top_n_retweets_10_metrics_false",
                              top_n_retweets(sqlite_con, 10, metrics = FALSE))
})


test_that("ggplot2 plot has expected output", {
  vdiffr::expect_doppelganger("top_n_retweets_10_metrics_true",
                              top_n_retweets(sqlite_con, 10, metrics = TRUE))
})


# Tests for when return_data = TRUE

test_that("list of length 2 is created as expected", {
  top_10_retweets <- top_n_retweets(sqlite_con, return_data = TRUE)
  expect_type(top_10_retweets, "list")
  expect_equal(2, length(top_10_retweets))
})


test_that("first element of list (chart) is a list", {
  top_10_retweets <- top_n_retweets(sqlite_con, return_data = TRUE)
  expect_type(top_10_retweets$chart, "list")
})


test_that("second element of list (data) is a data frame", {
  top_10_retweets <- top_n_retweets(sqlite_con, return_data = TRUE)
  expect_true(is.data.frame(top_10_retweets$data))
})


test_that("ggplot2 plot has expected output", {
  top_10_retweets <- top_n_retweets(sqlite_con,
                                    return_data = TRUE,
                                    metrics = FALSE)
  vdiffr::expect_doppelganger("top_n_retweets_10_metrics_false",
                              top_10_retweets$chart)
})

test_that("ggplot2 plot has expected output", {
  top_10_retweets <- top_n_retweets(sqlite_con,
                                    return_data = TRUE,
                                    metrics = TRUE)
  vdiffr::expect_doppelganger("top_n_retweets_10_metrics_true",
                              top_10_retweets$chart)
})


# Disconnect from database
DBI::dbDisconnect(sqlite_con)
