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
#'
#' @return ggplot2 plot.
#'
#' @importFrom dplyr mutate rename group_by summarise n slice_max
#'
#' @importFrom ggplot2 ggplot aes geom_col labs theme
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
#' top_n_hashtags(sqlite_con, n = 10)
#' my_plot <- top_n_hashtags(sqlite_con, 20)
#'
#' }
#'
#' @export

top_n_hashtags <- function(sqlite_con, n) {
  DBI::dbGetQuery(sqlite_con,
  "SELECT tag, source_id
  FROM hashtag
  WHERE source_type = 'tweet';") %>%
    mutate(tag = str_to_lower(.data$tag)) %>%
    rename(hashtag = .data$tag) %>%
    group_by(.data$hashtag) %>%
    summarise(tags = n()) %>%
    slice_max(n = n, order_by = .data$tags, with_ties = TRUE) %>%
    ggplot(aes(x = reorder(.data$hashtag, .data$tags), .data$tags)) +
    geom_col() +
    labs(title = paste0("Top ", n, " hashtags"),
         y = "Number of tweets") +
    configure_y_axis() +
    ggplot2::coord_flip() +
    configure_ggplot_theme() +
    theme(axis.title.y = ggplot2::element_blank())
}