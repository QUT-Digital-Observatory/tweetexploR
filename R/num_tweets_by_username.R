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
#'   Default value is 10. Recommended value to ensure clear visualisation is
#'   <= 30.
#'
#' @param return_data Should the data underlying the chart be returned?
#'   The default is `FALSE`. If `return_data = TRUE`, the data can be accessed
#'   in the second element, `data`, of the returned list.
#'
#' @param exclude_RT Should retweets be excluded from the calculations?
#'   The default is `FALSE`.
#'
#' @param ... Other arguments passed on to [ggplot2::geom_col()].
#'
#' @return ggplot2 plot. If `return_data = TRUE`, returns a named list with the
#'   first element, `chart`, being a ggplot2 plot, and the second element,
#'   `data`, being the underlying data for the ggplot2 plot in the form of a
#'   data frame.
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
#' results <- num_tweets_by_username(sqlite_con, return_data = TRUE)
#'
#' results <- num_tweets_by_username(sqlite_con, exclude_RT = TRUE)
#'
#' }
#'
#' @export

num_tweets_by_username <- function(sqlite_con,
                                   n = 10,
                                   return_data = FALSE,
                                   exclude_RT = FALSE, ...) {

  # When exclude_RT == FALSE construct query and chart title
  if (exclude_RT == FALSE) {

    query <- "SELECT count(*) as `tweet_count`, username
    FROM tweet
    LEFT JOIN (
      SELECT username, id
      FROM user ) user
    ON user.id = tweet.author_id
    GROUP BY username;"

    title <- paste0("Top ", n, " tweet authors by number of tweets")

  }

  # When exclude_RT == TRUE construct query and chart title
  if (exclude_RT == TRUE) {

    query <- "SELECT count(*) as `tweet_count`, username
    FROM tweet
    LEFT JOIN (
      SELECT username, id
      FROM user ) user
    ON user.id = tweet.author_id
    WHERE retweeted_tweet_id IS NULL
    GROUP BY username;"

    title <- paste0("Top ",
                    n,
                    "tweet authors by number of tweets (excluding retweets)")

  }

  chart_data <- DBI::dbGetQuery(sqlite_con, query) %>%
    unique() %>%
    slice_max(n = n, order_by = .data$tweet_count, with_ties = TRUE) %>%
    as.data.frame()

  chart <- ggplot(chart_data,
                  aes(x = reorder(.data$username, .data$tweet_count),
                      y = .data$tweet_count)) +
    geom_col(...) +
    labs(title = title,
         x = "Username",
         y = "Number of tweets") +
    coord_flip() +
    configure_y_axis() +
    configure_ggplot_theme() +
    theme(axis.title.y = element_blank())

  if (return_data == TRUE) {
    return(list(chart = chart, data = chart_data))
  }

  else if (return_data == FALSE) {
    return(chart)
  }

}
