library(DBI)
library(RSQLite)
library(ggplot2)
library(scales)
library(dplyr)


# ggplot2 theme
configure_ggplot_theme <- function() {
  my_theme <- ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 10, face = "plain"),
                   axis.title = ggplot2::element_text(size = 10, face = "plain"),
                   plot.title = ggplot2::element_text(size = 11, face = "plain"))
}


# ggplot2 y axis
configure_y_axis <- function() {
  my_y_axis <- ggplot2::scale_y_continuous(labels = scales::number_format(accuracy = 1, big.mark = ","))
}

#' @export
# Connect to existing .db file in current working directory
connect_to_sqlite_db <- function(sqlite_file) {
  sqlite_con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_file)
}

#' @export
# Number of tweets by username (top n usernames)
num_tweets_by_username <- function(sqlite_con, n) {
  DBI::dbGetQuery(sqlite_con,
    "SELECT count(*) as `tweet_count`, username
    FROM tweet
    LEFT JOIN (
      SELECT username, id
      FROM user ) user
    ON user.id = tweet.author_id
    GROUP BY username;") %>%
    dplyr::slice_max(n = n, order_by = tweet_count, with_ties = TRUE) %>%
    ggplot2::ggplot(ggplot2::aes(x = reorder(username, tweet_count), y = tweet_count)) +
    ggplot2::geom_col() +
    ggplot2::labs(title = paste0("Top ", n, " tweet authors by number of tweets"),
                  y = "Number of tweets") +
    ggplot2::coord_flip() +
    configure_y_axis() +
    configure_ggplot_theme() +
    ggplot2::theme(axis.title.y = ggplot2::element_blank())
}

sqlite_con <- connect_to_sqlite_db("auspol.db")
num_tweets_by_username(sqlite_con, 10)
