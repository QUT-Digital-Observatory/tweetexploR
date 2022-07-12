utils::globalVariables(c("id",
                         "created_at_datetime",
                         "created_at_hour",
                         "tweets",
                         "n"))

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
  check_for_valid_period(period)

  # If `from` is missing, substitute with a very old date
  if(missing(from) == TRUE) {
    from <- "0001-01-01 01:00:00"
  }
  # Check if `from` is valid
  check_for_valid_subset_input(period, from)

  # If `to` is missing, substitute with the current datetime
  if(missing(to) == TRUE) {
    to <- lubridate::now()
  }
  # Check if `to` is valid
  check_for_valid_subset_input(period, to)

  # Plot the data (for hourly)
  if(period == "hour") {
  DBI::dbGetQuery(sqlite_con,
                  "SELECT id, datetime(created_at) as `created_at_datetime`
                  FROM tweet;") %>%
    dplyr::mutate(created_at_datetime = lubridate::ymd_hms(created_at_datetime)) %>%
    dplyr::mutate(created_at_hour = lubridate::floor_date(created_at_datetime, unit = "hour")) %>%
    dplyr::filter(created_at_hour >= lubridate::ymd_hms(from) & created_at_hour <= lubridate::ymd_hms(to)) %>%
    dplyr::group_by(created_at_hour) %>%
    dplyr::summarise(tweets = dplyr::n()) %>%
    ggplot2::ggplot(ggplot2::aes(x = created_at_hour, y = tweets)) +
    ggplot2::geom_line(group = 1) +
    ggplot2::labs(title = "Number of tweets per hour",
                  x = "Hour",
                  y = "Number of tweets") +
    configure_y_axis() +
    configure_ggplot_theme()
  }

  # Plot the data (for daily)
  else if(period == "day") {
    print("daily plot not yet completed")
  }

  # Plot the data (for monthly)
  else if(period == "month") {
    print("monthly plot not yet completed")
  # Use lubridate::floor_date(ym(input), unit = "month")
  }
}


# ## Number of tweets per hour/day/month ####
#
# ### Hour ####
#
# # User supplied parameters
# lower_limit <- "2022-06-20 06:00:00"
# upper_limit <- "2022-06-21 00:00:00"
#
# # Plot
# dbGetQuery(con,
#            "SELECT id, datetime(created_at) as `created_at_datetime`
#             FROM tweet;") %>%
#   mutate(created_at_datetime = ymd_hms(created_at_datetime)) %>%
#   mutate(created_at_hour = floor_date(created_at_datetime, unit = "hour")) %>%
#   filter(created_at_hour >= ymd_hms(lower_limit) & created_at_hour <= ymd_hms(upper_limit)) %>%
#   group_by(created_at_hour) %>%
#   summarise(tweets = n()) %>%
#   ggplot(aes(created_at_hour, tweets)) +
#   geom_line(group = 1) +
#   labs(title = "Number of tweets per hour",
#        x = "Hour",
#        y = "Number of tweets") +
#   my_y_axis +
#   my_theme
#
#
# ### Day ####
#
# # User supplier parameters
# lower_limit <- "2022-06-14"
# upper_limit <- "2022-06-20"
#
# # Plot
# dbGetQuery(con,
#            "SELECT count(*) as `tweets`, date(created_at) as `day`
#             FROM tweet
#             GROUP BY day;") %>%
#   filter(day >= ymd(lower_limit) & day <= ymd(upper_limit)) %>%
#   ggplot(aes(ymd(day), tweets)) +
#   geom_line(group = 1) +
#   labs(title = "Number of tweets per day",
#        x = "Day",
#        y = "Number of tweets") +
#   scale_x_date(date_labels = "%d/%m/%Y") +
#   my_y_axis +
#   my_theme
#
#
# ### Month ####
#
# # User supplied parameters
# # Must be the first of the month
# lower_limit <- "2022-05-01"
# upper_limit <- "2022-06-01"
#
# # Plot
# dbGetQuery(con,
#            "SELECT count(*) as `tweets`, date(created_at) as `day`
#             FROM tweet
#             GROUP BY day;") %>%
#   mutate(month = floor_date(ymd(day), "month")) %>%
#   group_by(month) %>%
#   summarise(tweets = sum(tweets)) %>%
#   filter(month >= ymd(lower_limit) & month <= ymd(upper_limit)) %>%
#   ggplot(aes(month, tweets)) +
#   geom_col() +
#   labs(title = "Number of tweets per month",
#        x = "Month",
#        y = "Number of tweets") +
#   scale_x_date(date_labels = "%b %Y") +
#   my_y_axis +
#   my_theme
