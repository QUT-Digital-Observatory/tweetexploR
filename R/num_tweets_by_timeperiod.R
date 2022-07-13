#' Plot number of tweets per hour, day, or month
#'
#' @description Create a ggplot2 chart of the number of tweets per
#'   hour, day or month.
#'
#'   Hourly and daily charts will be plotted as a line graph, and monthly charts
#'   will be plotted as a bar graph.
#'
#' @param sqlite_con [Class SQLiteConnection](https://rsqlite.r-dbi.org/reference/sqliteconnection-class)
#'   object that is a connection to an SQLite .db file created by the
#'   [tidy-tweet package](https://github.com/QUT-Digital-Observatory/tidy_tweet).
#'   The database contains a collection of tweets in relational tables. This can
#'   be created with [tweetexploR::connect_to_sqlite_db()].
#'
#' @param period Time period for which the tweet frequency will be calculated.
#'   Accepted values are `"hour"`, `"day"`, or `"month"`. Defaults to `"day"`.
#'
#' @param from Optional parameter to subset the data to tweets that were created
#'   on or after the specified date and/or time in [Coordinated Universal Time
#'   (UCT)](https://en.wikipedia.org/wiki/Coordinated_Universal_Time).
#'
#'   For hourly charts, provide a character string in the format
#'   `"%Y-%m-%d %H:%M:%S"`. For example `"2022-06-20 06:00:00"`
#'
#'   For daily charts, provide a character string in the format
#'   `"%Y-%m-%d"`. For example `"2022-06-20"`
#'
#'   For monthly charts, provide a character string in the format
#'   `"%Y-%m"`. For example `"2022-06"`
#'
#' @param to Optional parameter to subset the data to tweets that were created
#'   on or before the specified date and/or time in [Coordinated Universal Time
#'   (UCT)](https://en.wikipedia.org/wiki/Coordinated_Universal_Time).
#'
#'   For hourly charts, provide a character string in the format
#'   `"%Y-%m-%d %H:%M:%S"`. For example `"2022-06-20 06:00:00"`
#'
#'   For daily charts, provide a character string in the format
#'   `"%Y-%m-%d"`. For example `"2022-06-20"`
#'
#'   For monthly charts, provide a character string in the format
#'   `"%Y-%m"`. For example `"2022-06"`
#'
#' @return ggplot2 plot.
#'
#' @importFrom ggplot2 ggplot aes geom_line labs scale_x_date geom_col
#'
#' @importFrom dplyr mutate filter group_by summarise n
#'
#' @importFrom lubridate now ymd_hms floor_date ymd
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#'
#' num_tweets_by_timeperiod(sqlite_con, period = "day", from = "2022-06-14",
#'   to = "2022-06-20")
#'
#' my_plot <- num_tweets_by_timeperiod(sqlite_con, period = "hour",
#'   from = "2022-06-20 06:00:00", to = "2022-06-21 06:00:00")
#'
#' my_plot <- num_tweets_by_timeperiod(sqlite_con, period = "hour")
#' }
#'
#' @export
# Number of tweets by username (top n usernames)

num_tweets_by_timeperiod <- function(sqlite_con, period, from, to) {

  # Check if `period` is valid
  stopifnot(
    "Please provide a valid value for `period`.
    Accepted values are \"day\", \"hour\", or \"month\"."
    = period %in% c("hour", "day", "month")
  )

  # If `from` is missing, substitute with a very old date
  if (missing(from) == TRUE) {
    if (period == "hour") {
      from <- "0001-01-01 01:00:00"
    }
    if (period == "day") {
      from <- "0001-01-01"
    }
    if (period == "month") {
      from <- "0001-01"
    }
  }

  # Check if `from` is valid
  check_for_valid_subset_input(period, from)

  # If `to` is missing, substitute with the current datetime
  if (missing(to) == TRUE) {
    if (period == "hour") {
      to <- now("UTC")
    }
    if (period == "day") {
      to <- substr(now("UTC"), 1, 10)
    }
    if (period == "month") {
      to <- substr(now("UTC"), 1, 7)
    }
  }

  # Check if `to` is valid
  check_for_valid_subset_input(period, to)

  # Plot the data (for hourly)
  if (period == "hour") {
    DBI::dbGetQuery(sqlite_con,
                    "SELECT id, datetime(created_at) as `created_at_datetime`
                    FROM tweet;") %>%
      mutate(created_at_datetime = ymd_hms(.data$created_at_datetime)) %>%
      mutate(created_at_hour = floor_date(.data$created_at_datetime, unit = "hour")) %>%
      filter(.data$created_at_hour >= ymd_hms(from) & .data$created_at_hour <= ymd_hms(to)) %>%
      group_by(.data$created_at_hour) %>%
      summarise(tweets = n()) %>%
      ggplot(aes(x = .data$created_at_hour, y = .data$tweets)) +
      geom_line(group = 1) +
      labs(title = "Number of tweets per hour",
           x = "Hour",
           y = "Number of tweets") +
      configure_y_axis() +
      configure_ggplot_theme()
  }

  # Plot the data (for daily)
  else if (period == "day") {
    DBI::dbGetQuery(sqlite_con,
                    "SELECT count(*) as `tweets`, date(created_at) as `day`
                    FROM tweet
                    GROUP BY day;") %>%
      filter(.data$day >= ymd(from) & .data$day <= ymd(to)) %>%
      ggplot(aes(x = ymd(.data$day), y = .data$tweets)) +
      geom_line(group = 1) +
      labs(title = "Number of tweets per day",
           x = "Day",
           y = "Number of tweets") +
      scale_x_date(date_labels = "%d/%m/%Y") +
      configure_y_axis() +
      configure_ggplot_theme()
  }

  # Plot the data (for monthly)
  else if (period == "month") {
    DBI::dbGetQuery(sqlite_con,
                    "SELECT count(*) as `tweets`, date(created_at) as `day`
                    FROM tweet
                    GROUP BY day;") %>%
      mutate(month = floor_date(ymd(.data$day), "month")) %>%
      group_by(.data$month) %>%
      summarise(tweets = sum(.data$tweets)) %>%
      filter(.data$month >= ymd(from) & .data$month <= ymd(to)) %>%
      ggplot(aes(x = .data$month, y = .data$tweets)) +
      geom_col() +
      labs(title = "Number of tweets per month",
           x = "Month",
           y = "Number of tweets") +
      scale_x_date(date_labels = "%b %Y") +
      configure_y_axis() +
      configure_ggplot_theme()
  }
}
