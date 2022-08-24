# Connect to sqlite .db file
sqlite_con <- connect_to_sqlite_db(test_path("fixtures", "auspol-test.db"))


# Tests for when return_data = FALSE

test_that("result is a ggplot2 object", {
  expect_true(ggplot2::is.ggplot(top_n_mentions(sqlite_con, 10)))
})


test_that("ggplot2 plot has expected output", {
  vdiffr::expect_doppelganger("top_n_mentions_10",
                              top_n_mentions(sqlite_con, 10))
})


# Tests for when return_data = TRUE

test_that("list of length 2 is created as expected", {
  top_10_mentions <- top_n_mentions(sqlite_con, return_data = TRUE)
  expect_type(top_10_mentions, "list")
  expect_equal(2, length(top_10_mentions))
})


test_that("first element of list (chart) is a list", {
  top_10_mentions <- top_n_mentions(sqlite_con, return_data = TRUE)
  expect_type(top_10_mentions$chart, "list")
})


test_that("second element of list (data) is a tibble", {
  top_10_mentions <- top_n_mentions(sqlite_con, return_data = TRUE)
  expect_true(tibble::is_tibble(top_10_mentions$data))
})


test_that("ggplot2 plot has expected output", {
  top_10_mentions <- top_n_mentions(sqlite_con, return_data = TRUE)
  vdiffr::expect_doppelganger("top_n_mentions_10",
                              top_10_mentions$chart)
})


# Disconnect from database
DBI::dbDisconnect(sqlite_con)
