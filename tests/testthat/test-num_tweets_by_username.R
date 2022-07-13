# Connect to sqlite .db file
sqlite_con <- connect_to_sqlite_db(test_path("fixtures", "auspol-test.db"))


test_that("result is a ggplot2 object", {
  expect_true(ggplot2::is.ggplot(num_tweets_by_username(sqlite_con, 10)))
})


test_that("ggplot2 plot has expected output", {
  vdiffr::expect_doppelganger("num_tweets_by_username_10",
                              num_tweets_by_username(sqlite_con, 10))
})
