#' Plot top n retweeted accounts
#'
#' @description Create a ggplot2 bar chart of the number of times the top `n`
#'   retweeted accounts (ties for `n`th position will be included) were
#'   retweeted.
#'
#' @param sqlite_con [Class SQLiteConnection](https://rsqlite.r-dbi.org/reference/sqliteconnection-class)
#'   object that is a connection to an SQLite .db file created by the
#'   [tidy-tweet package](https://github.com/QUT-Digital-Observatory/tidy_tweet).
#'   The database contains a collection of tweets in relational tables. This can
#'   be created with [tweetexploR::connect_to_sqlite_db()].
#'
#' @param n Number of accounts to be plotted. Note, ties will be included.
#'   Default value is 10. Recommended value to ensure clear visualisation is
#'   <= 30.
#'
#' @param metrics Should Twitter metrics be used to calculate the number of
#'   times each tweet was retweeted? The default, `FALSE`, calculates the number
#'   of retweets from within the database. `TRUE` calculates the number of
#'   retweets based on Twitter metrics at the time the data were collected.
#'
#' @param return_data Should the data underlying the chart be returned?
#'   The default is `FALSE`. If `return_data = TRUE`, the data can be accessed
#'   in the second element, `data`, of the returned list.
#'
#' @param ... Other arguments passed to [ggplot2::geom_col()].
#'
#' @return ggplot2 plot. If `return_data = TRUE`, returns a named list with the
#'   first element, `chart`, being a ggplot2 plot, and the second element,
#'   `data`, being the underlying data for the ggplot2 plot in the form of a
#'   data frame.
#'
#' @importFrom dplyr mutate rename group_by summarise n slice_max n_distinct
#'
#' @importFrom ggplot2 ggplot aes geom_col labs theme coord_flip element_blank
#'
#' @importFrom rlang .data
#'
#' @importFrom stats reorder
#'
#' @examples
#' \dontrun{
#'
#' top_n_retweeted_accounts(sqlite_con, n = 12)
#'
#' my_plot <- top_n_retweeted_accounts(sqlite_con, 20)
#'
#' top_n_retweeted_accounts(sqlite_con, fill = "blue")
#'
#' top_10_RT_accounts <- top_n_retweeted_accounts(sqlite_con,
#'   return_data = TRUE)
#'
#' top_10_RT_accounts <- top_n_retweeted_accounts(sqlite_con,
#'   return_data = TRUE, metrics = TRUE)
#'
#' }
#'
#' @export

top_n_retweeted_accounts <- function(sqlite_con,
                                     n = 10,
                                     metrics = FALSE,
                                     return_data = FALSE, ...) {

  if (metrics == FALSE) {

    query <- "SELECT tweet.id,
               retweeted_tweet_id,
               retweeted_tweets.author_id as `retweeted_author_id`,
               user.username as `username`,
               retweeted_tweets.text as `retweeted_text`
             FROM tweet
             LEFT JOIN (
               SELECT id as `primary_id`, author_id, text
               FROM tweet ) retweeted_tweets
             ON tweet.retweeted_tweet_id = retweeted_tweets.primary_id
             LEFT JOIN (
               SELECT id, username
               FROM user ) user
             ON user.id = retweeted_tweets.author_id
             WHERE retweeted_tweet_id IS NOT NULL;"

    title <- paste0("Top ", n, " retweeted accounts (within collection)")

    chart_data <- DBI::dbGetQuery(sqlite_con, query) %>%
      group_by(.data$username) %>%
      summarise(tweets = n_distinct(.data$retweeted_tweet_id),
                retweets = n(),
                avg_retweets_per_tweet = round(.data$retweets/.data$tweets, 1)) %>%
      slice_max(n = n, order_by = .data$avg_retweets_per_tweet, with_ties = TRUE) %>%
      as.data.frame()

  }

  else if (metrics == TRUE) {

    query <- "SELECT tweet.id, author_id, username, retweet_count
             FROM tweet
             LEFT JOIN (
               SELECT id, username
               FROM user ) user
             ON user.id = tweet.author_id
             WHERE retweet_count > 0;"

    title <- paste0("Top ", n, " retweeted accounts (Twitter metrics)")

    chart_data <- DBI::dbGetQuery(sqlite_con, query) %>%
      group_by(.data$username) %>%
      summarise(tweets = n(),
                retweets = sum(.data$retweet_count),
                avg_retweets_per_tweet = round(.data$retweets/.data$tweets, 1)) %>%
      slice_max(n = n, order_by = .data$avg_retweets_per_tweet, with_ties = TRUE) %>%
      as.data.frame()

  }

  chart <- ggplot(chart_data,
    aes(x = reorder(.data$username, .data$avg_retweets_per_tweet),
        y = .data$avg_retweets_per_tweet)) +
    geom_col(...) +
    labs(title = title,
         x = "Username",
         y = "Average retweets per tweet") +
    configure_y_axis() +
    coord_flip() +
    configure_ggplot_theme() +
    theme(axis.title.y = element_blank())

  if (return_data == TRUE) {
    return(list(chart = chart, data = chart_data))
  }

  else if (return_data == FALSE) {
    return(chart)
  }

}
