# Database connection ####

# Connect to sqlite .db file
sqlite_con <- connect_to_sqlite_db(test_path("fixtures", "auspol-test.db"))


# Tests for when return_data = FALSE ####

test_that("result is a ggplot2 object", {
  expect_true(ggplot2::is.ggplot(top_n_retweeted_accounts(sqlite_con, n = 10)))
})


test_that("ggplot2 plot has expected output", {
  vdiffr::expect_doppelganger("top_n_RT_accounts_10_m_false",
                              top_n_retweeted_accounts(sqlite_con,
                                                       n = 10,
                                                       metrics = FALSE))
})


test_that("ggplot2 plot has expected output", {
  vdiffr::expect_doppelganger("top_n_RT_accounts_10_m_true",
                              top_n_retweeted_accounts(sqlite_con,
                                                       n = 10,
                                                       metrics = TRUE))
})


# Tests for when return_data = TRUE ####

results_metrics_true <- top_n_retweeted_accounts(sqlite_con,
                                                 n = 10,
                                                 metrics = TRUE,
                                                 return_data = TRUE)

results_metrics_false <- top_n_retweeted_accounts(sqlite_con,
                                                  n = 10,
                                                  metrics = FALSE,
                                                  return_data = TRUE)


test_that("list of length 2 is created as expected (metrics = TRUE)", {
  expect_type(results_metrics_true, "list")
  expect_equal(length(results_metrics_true), 2)
})


test_that("first element of list (chart) is a list (metrics = TRUE)", {
  expect_type(results_metrics_true$chart, "list")
})


test_that("second element of list (data) is a data frame (metrics = TRUE)", {
  expect_true(is.data.frame(results_metrics_true$data))
})


# test_that("ggplot2 plot has expected output (metrics = TRUE)", {
#   vdiffr::expect_doppelganger("top_n_RT_accounts_10_m_true",
#                               results_metrics_true$chart)
# })


# test_that("ggplot2 plot has expected output (metrics = FALSE)", {
#   vdiffr::expect_doppelganger("top_n_RT_accounts_10_m_false",
#                               results_metrics_false$chart)
# })


# Disconnect from database ####
DBI::dbDisconnect(sqlite_con)
