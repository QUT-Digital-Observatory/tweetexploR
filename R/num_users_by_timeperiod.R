#' Plot number of unique users that tweeted per hour, day or month
#'
#' @description Create a ggplot2 chart of the number of unique users that
#'   tweeted per hour, day or month
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
#' @importFrom dplyr mutate filter n_distinct group_by summarise
#'
#' @importFrom lubridate ymd_hms floor_date ymd
#'
#' @importFrom ggplot2 ggplot aes geom_line labs scale_x_date geom_col
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#'
#' num_users_by_timeperiod(sqlite_con, period = "hour",
#' from = "2022-06-20 06:00:00", to = "2022-06-21 06:00:00")
#'
#' num_users_by_timeperiod(sqlite_con, period = "day",
#' from = "2022-06-20")
#'
#' my_plot <- num_users_by_timeperiod(sqlite_con, period = "day",
#' to = "2022-06-30")
#'
#' my_plot <- num_users_by_timeperiod(sqlite_con, period = "month")
#'
#' my_plot <- num_users_by_timeperiod(sqlite_con, period = "month",
#'   to = "2022-07")
#'
#' }
#'
#' @export

num_users_by_timeperiod <- function(sqlite_con, period, from, to) {

  # Check if `period` is valid
  check_if_period_is_valid(period)

  # If `from` is missing, substitute with a very old date
  if (missing(from) == TRUE) {
    from <- substitute_missing_from_input(period)
  }

  # Check if `from` is valid
  check_for_valid_subset_input(period, from)

  # If `to` is missing, substitute with the current datetime
  if (missing(to) == TRUE) {
    to <- substitute_missing_to_input(period)
  }

  # Check if `to` is valid
  check_for_valid_subset_input(period, to)

  # Plot the data (for hourly)
  if (period == "hour") {
    DBI::dbGetQuery(sqlite_con,
      "SELECT author_id, datetime(created_at) as `created_at_datetime`
      FROM tweet;") %>%
      mutate(created_at_datetime = ymd_hms(.data$created_at_datetime)) %>%
      mutate(created_at_hour =
               floor_date(.data$created_at_datetime, unit = "hour")) %>%
      filter(.data$created_at_hour >= ymd_hms(from) &
               .data$created_at_hour <= ymd_hms(to)) %>%
      group_by(.data$created_at_hour) %>%
      summarise(accounts = n_distinct(.data$author_id)) %>%
      ggplot(aes(x = .data$created_at_hour, y = .data$accounts)) +
      geom_line(group = 1) +
      labs(title = "Number of unique accounts that tweeted per hour",
           x = "Hour",
           y = "Number of accounts") +
      configure_y_axis() +
      configure_ggplot_theme()
  }

  # Plot the data (for daily)
  else if (period == "day") {
    DBI::dbGetQuery(sqlite_con,
    "SELECT count(distinct(author_id)) as `accounts`, date(created_at) as `day`
    FROM tweet
    GROUP BY day;") %>%
      filter(.data$day >= ymd(from) & .data$day <= ymd(to)) %>%
      ggplot(aes(x = ymd(.data$day), y = .data$accounts)) +
      geom_line(group = 1) +
      labs(title = "Number of unique accounts that tweeted per day",
           x = "Day",
           y = "Number of accounts") +
      scale_x_date(date_labels = "%d/%m/%Y") +
      configure_y_axis() +
      configure_ggplot_theme()
  }

  # Plot the data (for monthly)
  else if (period == "month") {
    DBI::dbGetQuery(sqlite_con,
    "SELECT count(distinct(author_id)) as `accounts`, date(created_at) as `day`
    FROM tweet
    GROUP BY day;") %>%
      mutate(month = floor_date(ymd(.data$day), "month")) %>%
      group_by(.data$month) %>%
      summarise(accounts = sum(.data$accounts)) %>%
      filter(.data$month >= ymd(paste0(from, "-01")) &
               .data$month <= ymd(paste0(to, "-01"))) %>%
      ggplot(aes(x = .data$month, y = .data$accounts)) +
      geom_col() +
      labs(title = "Number of unique accounts that tweeted per month",
           x = "Month",
           y = "Number of accounts") +
      scale_x_date(date_labels = "%b %Y") +
      configure_y_axis() +
      configure_ggplot_theme()
  }

}
