#' Generate a summary of engagement metrics for all tweets
#'
#' @description Generate a summary of engagement metrics for all tweets. The
#'   summary will be in the form of a data frame, and will include the
#'   following Twitter metrics: likes, quotes, replies, and retweets. A total of
#'   all Twitter metrics will also be included. The summary will be sorted in
#'   descending order by total, and will include the tweet text, tweet ID,
#'   author ID, and author username. Crucially, there are two date/time columns
#'   returned (as POSIXct in UTC time): created_at and retrieved_at. This allows
#'   the user to normalise engagement metrics against the difference between
#'   created_at and retrieved_at (since more time between a tweet being created
#'   and retrieved means more opportunity for the tweet to accumulate
#'   engagement).
#'
#' @param sqlite_con [Class SQLiteConnection](https://rsqlite.r-dbi.org/reference/sqliteconnection-class)
#'   object that is a connection to an SQLite .db file created by the
#'   [tidy-tweet package](https://github.com/QUT-Digital-Observatory/tidy_tweet).
#'   The database contains a collection of tweets in relational tables. This can
#'   be created with [tweetexploR::connect_to_sqlite_db()].
#'
#' @return data frame containing a summary of engagement metrics.
#'
#' @importFrom dplyr mutate arrange desc
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#'
#' engagement_summary(sqlite_con)
#'
#' engagement_summary <- engagement_summary(sqlite_con)
#'
#' }
#'
#' @export

engagement_summary <- function(sqlite_con) {

  DBI::dbGetQuery(sqlite_con,
  "SELECT tweet.id as `tweet_id`, author_id, user.name as `username`, text,
    like_count, quote_count, reply_count, retweet_count,
    tweet.created_at as `created_at`, tweet.retrieved_at as `retrieved_at`
  FROM tweet
  LEFT JOIN user ON tweet.author_id = user.id;") %>%
    mutate(total = .data$like_count + .data$quote_count + .data$reply_count
           + .data$retweet_count,
           created_at = as.POSIXct(.data$created_at,
                                   tz = "UTC",
                                   format = "%Y-%m-%dT%H:%M:%OS"),
           retrieved_at = as.POSIXct(.data$retrieved_at,
                                     tz = "UTC",
                                     format = "%Y-%m-%dT%H:%M:%OS")) %>%
    arrange(desc(.data$total)) %>%
    as.data.frame()

}
