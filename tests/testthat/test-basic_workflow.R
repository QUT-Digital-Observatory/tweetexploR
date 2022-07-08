test_that("connection to SQLite database can be established", {
  sqlitecon <- tweetexploR::connect_to_sqlite_db(test_path("fixtures", "auspol-test.db"))
  expect_s4_class(sqlitecon, "SQLiteConnection")
})
