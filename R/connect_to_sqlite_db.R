#' Connect to tidy-tweet SQLite database
#'
#' @description Connect to SQLite database file created by the
#' [tidy-tweet package](https://github.com/QUT-Digital-Observatory/tidy_tweet).
#'   The database contains a collection of tweets in relational tables.
#'
#'   It is recommended to create a variable for the `SQLiteConnection` created
#'   by `connect_to_sqlite_db()` so that you can use it for other tweetexploR
#'   functions.
#'
#' @param sqlite_file Path to SQLite .db file created by the
#' [tidy-tweet package](https://github.com/QUT-Digital-Observatory/tidy_tweet).
#'   The path should end with `.db`
#'
#' @return Class SQLiteConnection object (more information is available
#'   [here](https://rsqlite.r-dbi.org/reference/sqliteconnection-class)).
#'
#' @examples
#' \dontrun{
#'
#' sqlite_con <- connect_to_sqlite_db("my_database.db")
#' sqlite_con <- connect_to_sqlite_db("my_subfolder\\my_database.db")
#' }
#'
#' @export

connect_to_sqlite_db <- function(sqlite_file) {

  # Check that filename ends in .db
  stopifnot("Path must end in .db" = tools::file_ext(sqlite_file) == "db")

  # Make connection to database
  sqlite_con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_file)

  # Find out if there are tweets in the database
  tryCatch(
    expr = {
      number_of_tweets <-
        DBI::dbGetQuery(sqlite_con, "SELECT count(*) FROM tweet;")
      # Tell the user how many tweets are in the database
      cat("There are",
          format(number_of_tweets[[1]], big.mark = ","),
          "tweets in the database. \nHappy exploRing!")
      return(sqlite_con)
    },
    error = function(e) {
      # Tell the user that there are no tweets in the database
      message("There are no tweets in the database.")
    }
  )

}
