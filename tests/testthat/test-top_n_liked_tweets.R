# Database connection ####

# Connect to sqlite .db file
sqlite_con <- connect_to_sqlite_db(test_path("fixtures", "auspol-test.db"))


# Tests for when return_data = FALSE ####

test_that("result is a ggplot2 object", {
  expect_true(ggplot2::is.ggplot(top_n_liked_tweets(sqlite_con, n = 10)))
})


test_that("ggplot2 plot has expected output", {
  vdiffr::expect_doppelganger("top_n_liked_tweets_10",
                              top_n_liked_tweets(sqlite_con, n = 10))
})


# Tests for when return_data = TRUE ####

results <- top_n_liked_tweets(sqlite_con, n = 10, return_data = TRUE)

test_that("list of length 2 is created as expected", {
  expect_type(results, "list")
  expect_equal(length(results), 2)
})


test_that("first element of list (chart) is a list", {
  expect_type(results$chart, "list")
})


test_that("second element of list (data) is a data frame", {
  expect_true(is.data.frame(results$data))
})


test_that("ggplot2 plot has expected output", {
  vdiffr::expect_doppelganger("top_n_liked_tweets_10", results$chart)
})


# Tests for when exclude_RT = TRUE ####

test_that("result is a ggplot2 object (exclude_RT = TRUE)", {
  expect_true(ggplot2::is.ggplot(top_n_liked_tweets(sqlite_con,
                                                    exclude_RT = TRUE)))
})


test_that("ggplot2 plot has expected output (exclude_RT = TRUE)", {
  vdiffr::expect_doppelganger("top_n_non_retweets",
                              top_n_liked_tweets(sqlite_con,
                                                 exclude_RT = TRUE))
})


# Disconnect from database ####

DBI::dbDisconnect(sqlite_con)
