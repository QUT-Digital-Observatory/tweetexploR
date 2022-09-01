#' Plot top n replied to tweets
#'
#' @description Create a ggplot2 bar chart of the top `n` replied to tweets
#'   (ties for `n`th position will be included), based on Twitter metrics.
#'
#' @param sqlite_con [Class SQLiteConnection](https://rsqlite.r-dbi.org/reference/sqliteconnection-class)
#'   object that is a connection to an SQLite .db file created by the
#'   [tidy-tweet package](https://github.com/QUT-Digital-Observatory/tidy_tweet).
#'   The database contains a collection of tweets in relational tables. This can
#'   be created with [tweetexploR::connect_to_sqlite_db()].
#'
#' @param n Number of tweets to be plotted. Note, ties will be included.
#'   Default value is 10. Recommended value to ensure clear visualisation is
#'   <= 10.
#'
#' @param tweet_chars Number of characters of the tweet text to be displayed.
#'
#' @param chars_per_line How many characters of the tweet text to be displayed
#'   per line without breaking a word.
#'
#' @param return_data Should the data underlying the chart be returned?
#'   The default is `FALSE`. If `return_data = TRUE`, the data can be accessed
#'   in the second element, `data`, of the returned list.
#'
#' @param exclude_RT Should retweets be excluded from the calculations?
#'   The default is `FALSE`.
#'
#' @param ... Other arguments to be passed to [ggplot2::geom_col()].
#'
#' @return ggplot2 plot. If `return_data = TRUE`, returns a named list with the
#'   first element, `chart`, being a ggplot2 plot, and the second element,
#'   `data`, being the underlying data for the ggplot2 plot in the form of a
#'   data frame.
#'
#' @importFrom dplyr slice_max
#'
#' @importFrom ggplot2 ggplot geom_col labs scale_x_discrete theme coord_flip
#'   element_blank
#'
#' @importFrom rlang .data
#'
#' @importFrom stats reorder
#'
#' @importFrom scales label_wrap
#'
#' @examples
#' \dontrun{
#'
#' top_n_replied_to_tweets(sqlite_con, 12)
#'
#' top_n_replied_to_tweets(sqlite_con, fill = "blue")
#'
#' top_10_replied_to_tweets <- top_n_replied_to_tweets(sqlite_con,
#'   return_data = TRUE)
#'
#' top_10_replied_to_tweets <- top_n_replied_to_tweets(sqlite_con,
#'   return_data = TRUE, exclude_RT = TRUE)
#'
#' }
#'
#' @export

top_n_replied_to_tweets <- function(sqlite_con, n = 10,
                                    tweet_chars = 80,
                                    chars_per_line = 50,
                                    return_data = FALSE,
                                    exclude_RT = FALSE, ...) {

  # When exclude_RT == FALSE construct query and chart title
  if (exclude_RT == FALSE) {

    query <- "SELECT id, reply_count, text
             FROM tweet;"

    title <- paste0("Top ", n, " replied to tweets (Twitter metrics)")

  }

  # When exclude_RT == TRUE construct query and chart title
  if (exclude_RT == TRUE) {

    query <- "SELECT id, reply_count, text
              FROM tweet
              WHERE retweeted_tweet_id IS NULL;"

    title <- paste0("Top ",
                    n,
                    " replied to tweets (Twitter metrics; excluding retweets)")

  }

  chart_data <- DBI::dbGetQuery(sqlite_con, query) %>%
    unique() %>%
    slice_max(n = n, order_by = .data$reply_count, with_ties = TRUE) %>%
    as.data.frame()

  chart <- ggplot(chart_data,
    aes(x = reorder(substr(.data$text, 1, tweet_chars), .data$reply_count),
        y = .data$reply_count)) +
    geom_col(...) +
    labs(title = title,
         x = "Tweet",
         y = "Number of replies") +
    scale_x_discrete(labels = label_wrap(chars_per_line)) +
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
