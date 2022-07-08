test_that("tables can be read from connection to .db file", {
  sqlitecon <- tweetexploR::connect_to_sqlite_db(test_path("fixtures", "auspol-test.db"))
  expect_s4_class(sqlitecon, "SQLiteConnection")
})
