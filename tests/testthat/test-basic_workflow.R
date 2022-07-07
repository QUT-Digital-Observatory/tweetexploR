test_that("tables can be read from connection to .db file", {
  sqlitecon <- tweetexploR::connect_to_sqlite_db(test_path("fixtures", "auspol-test.db"))
  tables <- DBI::dbListTables(sqlite_con)
  expect_length(tables, 7)
})
# This test doesn't pass yet, still trying to get it to work
