# Database connection ####

# Connect to sqlite .db file
sqlite_con <- connect_to_sqlite_db(test_path("fixtures", "auspol-test.db"))


# Tests for when return_data = FALSE ####

test_that("result is a ggplot2 object", {
  expect_true(ggplot2::is.ggplot(top_n_replied_to_tweets(sqlite_con, 10)))
})


test_that("ggplot2 plot has expected output", {
  vdiffr::expect_doppelganger("top_n_replied_to_tweets_10",
                              top_n_replied_to_tweets(sqlite_con, 10))
})


# Tests for when return_data = TRUE ####

results <- top_n_replied_to_tweets(sqlite_con, n = 10, return_data = TRUE)

test_that("list of length 2 is created as expected", {
  expect_type(results, "list")
  expect_equal(2, length(results))
})


test_that("first element of list (chart) is a list", {
  expect_type(results, "list")
})


test_that("second element of list (data) is a data frame", {
  expect_true(is.data.frame(results$data))
})


test_that("ggplot2 plot has expected output", {
  vdiffr::expect_doppelganger("top_n_replied_to_tweets_10", results$chart)
})


# Disconnect from database ####

DBI::dbDisconnect(sqlite_con)
