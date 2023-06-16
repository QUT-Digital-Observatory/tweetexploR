# Database connection ####

# Connect to sqlite .db file
sqlite_con <- connect_to_sqlite_db(test_path("fixtures", "auspol-test.db"))


test_that("result is a dataframe", {
  expect_true(is.data.frame(engagement_summary(sqlite_con)))
})


test_that("data frame has expected number of columns", {
  expect_equal(dim(engagement_summary(sqlite_con))[2], 11)
})


# Disconnect from database ####

DBI::dbDisconnect(sqlite_con)
