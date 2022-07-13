test_that("connection to SQLite database can be established", {
  sqlite_con <- connect_to_sqlite_db(test_path("fixtures", "auspol-test.db"))
  expect_s4_class(sqlite_con, "SQLiteConnection")
})

# Disconnect from database
DBI::dbDisconnect(sqlite_con)
