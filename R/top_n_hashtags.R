#' Plot top n hashtags
#'
#' @description Create a ggplot2 bar chart of the number of times the top `n`
#'   hashtags (ties for `n`th position will be included) were used in tweets.
#'
#' @param sqlite_con [Class SQLiteConnection](https://rsqlite.r-dbi.org/reference/sqliteconnection-class)
#'   object that is a connection to an SQLite .db file created by the
#'   [tidy-tweet package](https://github.com/QUT-Digital-Observatory/tidy_tweet).
#'   The database contains a collection of tweets in relational tables. This can
#'   be created with [tweetexploR::connect_to_sqlite_db()].
#'
#' @param n Number of hashtags to be plotted. Note, ties will be included.
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
#' @importFrom dplyr mutate rename group_by summarise n slice_max
#'
#' @importFrom ggplot2 ggplot aes geom_col labs theme coord_flip element_blank
#'
#' @importFrom stringr str_to_lower
#'
#' @importFrom rlang .data
#'
#' @importFrom stats reorder
#'
#' @examples
#' \dontrun{
#'
#' top_n_hashtags(sqlite_con, n = 12)
#'
#' my_plot <- top_n_hashtags(sqlite_con, 20)
#'
#' top_n_hashtags(sqlite_con, fill = "blue")
#'
#' top_10_hashtags <- top_n_hashtags(sqlite_con, return_data = TRUE)
#'
#' top_10_hashtags <- top_n_hashtags(sqlite_con, return_data = TRUE,
#'   exclude_RT = TRUE)
#'
#' }
#'
#' @export

top_n_hashtags <- function(sqlite_con,
                           n = 10,
                           return_data = FALSE,
                           exclude_RT = FALSE, ...) {

  # When exclude_RT == FALSE construct query and chart title
  if (exclude_RT == FALSE) {

    query <- "SELECT hashtag as `tag`, tweet_id
             FROM tweet_hashtag
             LEFT JOIN (
               SELECT id
               FROM tweet ) tweet
             ON tweet.id = tweet_hashtag.tweet_id"

    title <- paste0("Top ", n, " hashtags")

  }

  # When exclude_RT == TRUE construct query and chart title
  if (exclude_RT == TRUE) {

    query <- "SELECT hashtag as `tag`, tweet_id
             FROM tweet_hashtag
             LEFT JOIN (
               SELECT id, retweeted_tweet_id
               FROM tweet ) tweet
             ON tweet.id = tweet_hashtag.tweet_id
             WHERE retweeted_tweet_id IS NULL;"

    title <- paste0("Top ", n, " hashtags (excluding retweets)")

  }

  chart_data <- DBI::dbGetQuery(sqlite_con, query) %>%
    unique() %>%
    mutate(tag = str_to_lower(.data$tag)) %>%
    rename(hashtag = .data$tag) %>%
    group_by(.data$hashtag) %>%
    summarise(tags = n()) %>%
    slice_max(n = n, order_by = .data$tags, with_ties = TRUE) %>%
    as.data.frame()

  chart <- ggplot(chart_data, aes(x = reorder(.data$hashtag, .data$tags),
                                  y = .data$tags)) +
    geom_col(...) +
    labs(title = title,
         x = "Hashtag",
         y = "Number of tweets") +
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
