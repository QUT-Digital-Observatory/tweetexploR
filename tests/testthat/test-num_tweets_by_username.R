test_that("result is a ggplot2 object", {
  sqlite_con <- tweetexploR::connect_to_sqlite_db(test_path("fixtures", "auspol-test.db"))
  p <- tweetexploR::num_tweets_by_username(sqlite_con, 10)
  expect_true(ggplot2::is.ggplot(p))
})
