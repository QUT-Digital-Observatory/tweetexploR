# Database connection ####

# Connect to sqlite .db file
sqlite_con <- connect_to_sqlite_db(test_path("fixtures", "auspol-test.db"))


test_that("connection to SQLite database can be established", {
  expect_s4_class(sqlite_con, "SQLiteConnection")
})


test_that("known results of a query are returned", {
  expect_equal(DBI::dbGetQuery(sqlite_con, "SELECT count(*) from tweet;")[[1]],
               1369)
})


# Disconnect from database ####

DBI::dbDisconnect(sqlite_con)
