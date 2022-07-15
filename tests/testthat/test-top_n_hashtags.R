# Connect to sqlite .db file
sqlite_con <- connect_to_sqlite_db(test_path("fixtures", "auspol-test.db"))


test_that("result is a ggplot2 object", {
  expect_true(ggplot2::is.ggplot(top_n_hashtags(sqlite_con, 10)))
})


test_that("ggplot2 plot has expected output", {
  vdiffr::expect_doppelganger("top_n_hashtags_10",
                              top_n_hashtags(sqlite_con, 10))
})


# Disconnect from database
DBI::dbDisconnect(sqlite_con)
