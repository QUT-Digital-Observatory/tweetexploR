# Connect to sqlite .db file
sqlite_con <- connect_to_sqlite_db(test_path("fixtures", "auspol-test.db"))


test_that("connection to SQLite database can be established", {
  expect_s4_class(sqlite_con, "SQLiteConnection")
})


# Disconnect from database
DBI::dbDisconnect(sqlite_con)
