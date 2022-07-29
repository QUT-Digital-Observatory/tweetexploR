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
#'   Default value is 10.
#'
#' @param ... Other arguments passed to [ggplot2::geom_col()].
#'
#' @return ggplot2 plot.
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
#' top_n_mentions(sqlite_con, n = 10)
#'
#' my_plot <- top_n_mentions(sqlite_con, 20)
#'
#' top_n_mentions(sqlite_con, fill = "blue")
#'
#' }
#'
#' @export

top_n_mentions <- function(sqlite_con, n = 10, ...) {
  DBI::dbGetQuery(sqlite_con,
  "SELECT username, source_id
  FROM mention
  WHERE source_type = 'tweet';") %>%
    mutate(tag = str_to_lower(.data$username)) %>%
    rename(account = .data$username) %>%
    group_by(.data$account) %>%
    summarise(mentions = n()) %>%
    slice_max(n = n, order_by = .data$mentions, with_ties = TRUE) %>%
    ggplot(aes(x = reorder(.data$account, .data$mentions), .data$mentions)) +
    geom_col(...) +
    labs(title = paste0("Top ", n, " accounts mentioned in tweets"),
         x = "Username",
         y = "Number of tweets") +
    configure_y_axis() +
    coord_flip() +
    configure_ggplot_theme() +
    theme(axis.title.y = element_blank())
}
