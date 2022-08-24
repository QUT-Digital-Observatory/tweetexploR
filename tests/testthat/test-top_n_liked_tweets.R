# Connect to sqlite .db file
sqlite_con <- connect_to_sqlite_db(test_path("fixtures", "auspol-test.db"))


# Tests for when return_data = FALSE

test_that("result is a ggplot2 object", {
  expect_true(ggplot2::is.ggplot(top_n_liked_tweets(sqlite_con, 10)))
})


test_that("ggplot2 plot has expected output", {
  vdiffr::expect_doppelganger("top_n_liked_tweets_10",
                              top_n_liked_tweets(sqlite_con, 10))
})


# Tests for when return_data = TRUE

test_that("list of length 2 is created as expected", {
  top_50_liked_tweets <- top_n_liked_tweets(sqlite_con, n = 50,
                                            return_data = TRUE)
  expect_type(top_50_liked_tweets, "list")
  expect_equal(2, length(top_50_liked_tweets))
})

test_that("first element of list (chart) is a list", {
  top_50_liked_tweets <- top_n_liked_tweets(sqlite_con, n = 50,
                                            return_data = TRUE)
  expect_type(top_50_liked_tweets$chart, "list")
})


test_that("second element of list (data) is a data frame", {
  top_50_liked_tweets <- top_n_liked_tweets(sqlite_con, n = 50,
                                            return_data = TRUE)
  expect_true(is.data.frame(top_50_liked_tweets$data))
})


test_that("ggplot2 plot has expected output", {
  top_20_liked_tweets <- top_n_liked_tweets(sqlite_con, n = 20,
                                            return_data = TRUE)
  vdiffr::expect_doppelganger("top_20_liked_tweets",
                              top_20_liked_tweets$chart)
})


# Disconnect from database
DBI::dbDisconnect(sqlite_con)
