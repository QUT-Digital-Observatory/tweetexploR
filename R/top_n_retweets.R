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
#'   Default value is 10.
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
#' @importFrom dplyr slice_max distinct
#'
#' @importFrom ggplot2 aes geom_col labs scale_x_discrete theme coord_flip
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
#' top_n_retweets(sqlite_con)
#'
#' top_n_retweets(sqlite_con, n = 12, fill = "blue")
#'
#' top_10_retweets <- (sqlite_con, return_data = TRUE)
#'
#' }
#'
#' @export

top_n_retweets <- function(sqlite_con, n = 10, metrics = FALSE,
                           tweet_chars = 80, chars_per_line = 50,
                           return_data = FALSE, ...) {

  if (metrics == FALSE) {

    chart_data <- DBI::dbGetQuery(sqlite_con,
    "SELECT retweeted_tweet_id, count(*) as `retweets`, text
    FROM tweet
    WHERE retweeted_tweet_id IS NOT NULL
    GROUP BY retweeted_tweet_id;") %>%
      slice_max(n = n, order_by = .data$retweets, with_ties = TRUE) %>%
      as.data.frame()

    chart <- ggplot(chart_data,
      aes(x = reorder(substr(.data$text, 1, tweet_chars), .data$retweets),
          y = .data$retweets)) +
      geom_col(...) +
      labs(title = paste0("Top ", n, " retweeted tweets (within collection)"),
           x = "Tweet",
           y = "Number of retweets") +
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

  else if (metrics == TRUE) {

    chart_data <- DBI::dbGetQuery(sqlite_con,
    "SELECT retweet_count, text
    FROM tweet;") %>%
      distinct() %>%
      slice_max(n = n, order_by = .data$retweet_count, with_ties = TRUE) %>%
      as.data.frame()

    chart <- ggplot(chart_data,
      aes(x = reorder(substr(.data$text, 1, tweet_chars), .data$retweet_count),
          y = .data$retweet_count)) +
      geom_col(...) +
      labs(title = paste0("Top ", n, " retweeted tweets (Twitter metrics)"),
           y = "Number of retweets") +
      scale_x_discrete(labels = scales::label_wrap(chars_per_line)) +
      configure_y_axis() +
      ggplot2::coord_flip() +
      configure_ggplot_theme() +
      theme(axis.title.y = ggplot2::element_blank())

    if (return_data == TRUE) {
      return(list(chart = chart, data = chart_data))
    }

    else if (return_data == FALSE) {
      return(chart)
    }

  }

}
