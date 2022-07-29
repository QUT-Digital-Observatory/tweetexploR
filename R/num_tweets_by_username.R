#' Plot number of tweets for top n usernames
#'
#' @description Create a ggplot2 bar chart of the number of tweets per username
#'   for the top `n` usernames (ties for `n`th position will be included).
#'
#' @param sqlite_con [Class SQLiteConnection](https://rsqlite.r-dbi.org/reference/sqliteconnection-class)
#'   object that is a connection to an SQLite .db file created by the
#'   [tidy-tweet package](https://github.com/QUT-Digital-Observatory/tidy_tweet).
#'   The database contains a collection of tweets in relational tables. This can
#'   be created with [tweetexploR::connect_to_sqlite_db()].
#'
#' @param n Number of usernames to be plotted. Note, ties will be included.
#'   Default value is 10.
#'
#' @param ... Other arguments passed on to [ggplot2::geom_col()].
#'
#' @return ggplot2 plot.
#'
#' @importFrom dplyr slice_max
#'
#' @importFrom ggplot2 ggplot aes geom_col labs theme element_blank coord_flip
#'
#' @importFrom stats reorder
#'
#' @examples
#' \dontrun{
#'
#' num_tweets_by_username(sqlite_con, n = 12)
#'
#' my_plot <- num_tweets_by_username(sqlite_con, 20)
#'
#' num_tweets_by_username(sqlite_con, fill = "blue")
#'
#' }
#'
#' @export

num_tweets_by_username <- function(sqlite_con, n = 10, ...) {
  DBI::dbGetQuery(sqlite_con,
                  "SELECT count(*) as `tweet_count`, username
                  FROM tweet
                  LEFT JOIN (
                    SELECT username, id
                    FROM user ) user
                  ON user.id = tweet.author_id
                  GROUP BY username;") %>%
    slice_max(n = n, order_by = .data$tweet_count, with_ties = TRUE) %>%
    ggplot(aes(x = reorder(.data$username, .data$tweet_count),
               y = .data$tweet_count)) +
    geom_col(...) +
    labs(title = paste0("Top ", n, " tweet authors by number of tweets"),
         x = "Username",
         y = "Number of tweets") +
    coord_flip() +
    configure_y_axis() +
    configure_ggplot_theme() +
    theme(axis.title.y = element_blank())
}
