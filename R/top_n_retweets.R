#' Plot top n retweeted tweets
#'
#' @description Create a ggplot2 bar chart of the number of times the top `n`
#'   retweeted tweets (ties for `n`th position will be included) were retweeted.
#'
#' @param sqlite_con [Class SQLiteConnection](https://rsqlite.r-dbi.org/reference/sqliteconnection-class)
#'   object that is a connection to an SQLite .db file created by the
#'   [tidy-tweet package](https://github.com/QUT-Digital-Observatory/tidy_tweet).
#'   The database contains a collection of tweets in relational tables. This can
#'   be created with [tweetexploR::connect_to_sqlite_db()].
#'
#' @param n Number of retweets to be plotted. Note, ties will be included.
#'
#' @param metrics Should Twitter metrics be used to calculate the number of
#'   times each tweet was retweeted? The default, `FALSE`, calculates the number
#'   of retweets from within the database. `TRUE` calculates the number of
#'   retweets based on Twitter metrics at the time the data were collected.
#'
#' @param tweet_chars Number of characters of the tweet text to be displayed.
#'
#' @param chars_per_line How many characters of the tweet text to be displayed
#'   per line without breaking a word.
#'
#' @return ggplot2 plot.
#'
#' @importFrom dplyr slice_max
#'
#' @importFrom ggplot2 aes geom_col labs scale_x_discrete theme
#'
#' @importFrom rlang .data
#'
#' @importFrom stats reorder
#'
#' @examples
#' \dontrun{
#'
#' top_n_retweets(sqlite_con, 10)
#'
#' }
#'
#' @export

top_n_retweets <- function(sqlite_con, n, metrics = FALSE, tweet_chars = 80,
                           chars_per_line = 50) {
  if (metrics == FALSE) {
    DBI::dbGetQuery(sqlite_con,
    "SELECT retweeted_tweet_id, count(*) as `retweets`, text
    FROM tweet
    WHERE retweeted_tweet_id IS NOT NULL
    GROUP BY retweeted_tweet_id;") %>%
      slice_max(n = n, order_by = .data$retweets, with_ties = TRUE) %>%
      ggplot(aes(reorder(substr(.data$text, 1, tweet_chars),
                         .data$retweets), .data$retweets)) +
      geom_col() +
      labs(title = paste0("Top ", n, " retweeted tweets (within collection)"),
           y = "Number of retweets") +
      scale_x_discrete(labels = scales::label_wrap(chars_per_line)) +
      configure_y_axis() +
      ggplot2::coord_flip() +
      configure_ggplot_theme() +
      theme(axis.title.y = ggplot2::element_blank())
  }

  else if (metrics == TRUE) {
    # Plot based on Twitter metrics
  }

}

## Top n retweeted tweets ####

### Based on number of retweets inside the tweets that were collected ####

# dbGetQuery(con,
#            "SELECT retweeted_tweet_id, count(*) as `retweets`, text
#             FROM tweet
#             WHERE retweeted_tweet_id IS NOT NULL
#             GROUP BY retweeted_tweet_id;") %>%
#   slice_max(n = n, order_by = retweets, with_ties = TRUE) %>%
#   ggplot(aes(reorder(substr(text, 1, tweet_chars), retweets), retweets)) +
#   geom_col() +
#   labs(title = paste0("Top ", n, " retweeted tweets (within collection)"),
#        y = "Number of retweets") +
#   scale_x_discrete(labels = label_wrap(chars_per_line))+
#   my_y_axis +
#   coord_flip() +
#   my_theme +
#   theme(axis.title.y = element_blank())


### Based on tweet.retweet_count (Twitter metrics) ####

# dbGetQuery(con,
#            "SELECT retweet_count, text
#             FROM tweet;") %>%
#   distinct() %>%
#   slice_max(n = n, order_by = retweet_count, with_ties = TRUE) %>%
#   ggplot(aes(reorder(substr(text, 1, tweet_chars), retweet_count), retweet_count)) +
#   geom_col() +
#   labs(title = paste0("Top ", n, " retweeted tweets (Twitter metrics)"),
#        y = "Number of retweets") +
#   scale_x_discrete(labels = label_wrap(chars_per_line)) +
#   my_y_axis +
#   coord_flip() +
#   my_theme +
#   theme(axis.title.y = element_blank())
