# Database connection ####

# Connect to sqlite .db file
sqlite_con <- connect_to_sqlite_db(test_path("fixtures", "auspol-test.db"))


# Tests for when return_data = FALSE ####

test_that("result is a ggplot2 object", {
  expect_true(ggplot2::is.ggplot(top_n_hashtags(sqlite_con)))
})


test_that("ggplot2 plot has expected output", {
  vdiffr::expect_doppelganger("top_n_hashtags_10",
                              top_n_hashtags(sqlite_con, n = 10))
})


# Tests for when return_data = TRUE ####

results <- top_n_hashtags(sqlite_con, n = 10, return_data = TRUE)


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
  vdiffr::expect_doppelganger("top_n_hashtags_10", results$chart)
})


# Tests for when exclude_RT = TRUE ####

test_that("ggplot2 plot has expected output (excl RT)", {
  vdiffr::expect_doppelganger("top_n_hashtags_10_excl_RT",
                              top_n_hashtags(sqlite_con,
                                             n = 10,
                                             exclude_RT = TRUE))
})

# Tests for when hashtag_source = "users" ####

test_that("ggplot2 plot has expected output (user hashtags", {
  vdiffr::expect_doppelganger("top_n_hashtags_users",
                              top_n_hashtags(sqlite_con,
                                             n = 10,
                                             hashtag_source = "users"))
})


# Disconnect from database ####

DBI::dbDisconnect(sqlite_con)
