#' Plot top n account mentions
#'
#' @description Create a ggplot2 bar chart of the number of times the top `n`
#'   accounts (ties for `n`th position will be included) were mentioned in
#'   tweets.
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
#' @param return_data Should the data underlying the chart be returned?
#'   The default is `FALSE`. If `return_data = TRUE`, the data can be accessed
#'   in the second element, `data`, of the returned list.
#'
#' @param exclude_RT Should retweets be excluded from the calculations?
#'   The default is `FALSE`.
#'
#' @param ... Other arguments passed to [ggplot2::geom_col()].
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
#' top_n_mentions(sqlite_con, n = 12)
#'
#' my_plot <- top_n_mentions(sqlite_con, 20)
#'
#' top_n_mentions(sqlite_con, fill = "blue")
#'
#' top_10_mentions <- top_n_mentions(sqlite_con, return_data = TRUE)
#'
#' top_10_mentions <- top_n_mentions(sqlite_con, return_data = TRUE,
#'   exclude_RT = TRUE)
#'
#' }
#'
#' @export

top_n_mentions <- function(sqlite_con,
                           n = 10,
                           return_data = FALSE,
                           exclude_RT = FALSE, ...) {

  # When exclude_RT == FALSE construct query and chart title
  if (exclude_RT == FALSE) {

    query <- "SELECT username, source_id
             FROM mention
             LEFT JOIN (
               SELECT id
               FROM tweet ) tweet
             ON tweet.id = mention.source_id
             WHERE source_type = 'tweet';"

    title <- paste0("Top ", n, " accounts mentioned in tweets")

  }

  # When exclude_RT == TRUE construct query and chart title
  if (exclude_RT == TRUE) {

    query <- "SELECT username, source_id
             FROM mention
             LEFT JOIN (
               SELECT id, retweeted_tweet_id
               FROM tweet ) tweet
             ON tweet.id = mention.source_id
             WHERE source_type = 'tweet'
               AND retweeted_tweet_id IS NULL;"

    title <- paste0("Top ",
                    n,
                    " accounts mentioned in tweets (excluding retweets")

  }

  chart_data <- DBI::dbGetQuery(sqlite_con, query) %>%
    unique() %>%
    mutate(tag = str_to_lower(username)) %>%
    rename(account = username) %>%
    group_by(account) %>%
    summarise(mentions = n()) %>%
    slice_max(n = n, order_by = mentions, with_ties = TRUE) %>%
    as.data.frame()

  chart <- ggplot(chart_data, aes(x = reorder(account, mentions),
                                  y = mentions)) +
    geom_col(...) +
    labs(title = title,
         x = "Username",
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
