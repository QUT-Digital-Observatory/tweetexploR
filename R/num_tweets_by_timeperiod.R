#' Plot number of tweets per hour, day, or month
#'
#' @description Create a ggplot2 chart of the number of tweets per hour, day
#'   or month.
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
#'   Recommended subsets for each time period in order to ensure clear
#'   visualisations are as follows:
#'   - Hourly: 1-3 days, ideally no more than 7 days;
#'   - Daily: Up to 1 month;
#'   - Monthly: 1-2 years.
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
#'   Recommended subsets for each time period in order to ensure clear
#'   visualisations are as follows:
#'   - Hourly: 1-3 days, ideally no more than 7 days;
#'   - Daily: Up to 1 month;
#'   - Monthly: 1-2 years.
#'
#' @param return_data Should the data underlying the chart be returned?
#'   The default is `FALSE`. If `return_data = TRUE`, the data can be accessed
#'   in the second element, `data`, of the returned list.
#'
#' @param exclude_RT Should retweets be excluded from the calculations?
#'   The default is `FALSE`.
#'
#' @param ... Other arguments passed on to [ggplot2::geom_line()] for hourly and
#'   daily charts, or [ggplot2::geom_col()] for monthly charts.
#'
#' @return ggplot2 plot. If `return_data = TRUE`, returns a named list with the
#'   first element, `chart`, being a ggplot2 plot, and the second element,
#'   `data`, being the underlying data for the ggplot2 plot in the form of a
#'   data frame.
#'
#' @importFrom ggplot2 ggplot aes geom_line labs scale_x_date geom_col
#'
#' @importFrom dplyr mutate filter group_by summarise n
#'
#' @importFrom lubridate ymd_hms floor_date ymd
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
#'
#' my_plot <- num_tweets_by_timeperiod(sqlite_con, period = "month",
#'   from = "2022-06")
#'
#' num_tweets_by_timeperiod(sqlite_con, period = "hour", colour = "blue")
#'
#' num_tweets_by_timeperiod(sqlite_con, period = "month", fill = "blue")
#'
#' results <- num_tweets_by_timeperiod(sqlite_con, period = "hour",
#'   return_data = TRUE)
#'
#' results <- num_tweets_by_timeperiod(sqlite_con, period = "day",
#'   exclude_RT == TRUE)
#'
#' }
#'
#' @export

num_tweets_by_timeperiod <- function(sqlite_con,
                                     period,
                                     from,
                                     to,
                                     return_data = FALSE,
                                     exclude_RT = FALSE, ...) {

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

    # When exclude_RT == FALSE construct query and chart title
    if (exclude_RT == FALSE) {

      query <- "SELECT id, datetime(created_at) as `created_at_datetime`
               FROM tweet;"

      title <- "Number of tweets per hour"

    }

    # When exclude_RT == TRUE construct query and chart title
    else if (exclude_RT == TRUE) {

      query <- "SELECT id, datetime(created_at) as `created_at_datetime`
               FROM tweet
               WHERE retweeted_tweet_id IS NULL;"

      title <- "Number of tweets per hour (excluding retweets)"

    }

    chart_data <- DBI::dbGetQuery(sqlite_con, query) %>%
      unique() %>%
      mutate(created_at_datetime = ymd_hms(.data$created_at_datetime)) %>%
      mutate(created_at_hour = floor_date(.data$created_at_datetime,
                                          unit = "hour")) %>%
      filter(.data$created_at_hour >= ymd_hms(from) &
               .data$created_at_hour <= ymd_hms(to)) %>%
      group_by(.data$created_at_hour) %>%
      summarise(tweets = n()) %>%
      as.data.frame()

    chart <- ggplot(chart_data,
                    aes(x = .data$created_at_hour, y = .data$tweets)) +
      geom_line(group = 1, ...) +
      labs(title = title,
           x = "Hour",
           y = "Number of tweets") +
      configure_y_axis() +
      configure_ggplot_theme()

    if (return_data == TRUE) {
      return(list(chart = chart, data = chart_data))
    }

    else if (return_data == FALSE) {
      return(chart)
    }

  }

  # Plot the data (for daily)
  else if (period == "day") {

    # When exclude_RT == FALSE construct query and chart title
    if (exclude_RT == FALSE) {

      query <- "SELECT count(*) as `tweets`, date(created_at) as `day`
      FROM tweet
      GROUP BY day;"

      title <- "Number of tweets per day"

    }

    # When exclude_RT == TRUE construct query and chart title
    else if (exclude_RT == TRUE) {

      query <- "SELECT count(*) as `tweets`, date(created_at) as `day`
      FROM tweet
      WHERE retweeted_tweet_id IS NULL
      GROUP BY day;"

      title <- "Number of tweets per day (excluding retweets)"

    }

    chart_data <- DBI::dbGetQuery(sqlite_con, query) %>%
      unique() %>%
      filter(.data$day >= ymd(from) & .data$day <= ymd(to))

    chart <- ggplot(chart_data, aes(x = ymd(.data$day), y = .data$tweets)) +
      geom_line(group = 1, ...) +
      labs(title = title,
           x = "Day",
           y = "Number of tweets") +
      scale_x_date(date_labels = "%d/%m/%Y") +
      configure_y_axis() +
      configure_ggplot_theme()

    if (return_data == TRUE) {
      return(list(chart = chart, data = chart_data))
    }

    else if (return_data == FALSE) {
      return(chart)
    }

  }

  # Plot the data (for monthly)
  else if (period == "month") {

    # When exclude_RT == FALSE construct query and chart title
    if (exclude_RT == FALSE) {

      query <- "SELECT count(*) as `tweets`, date(created_at) as `day`
      FROM tweet
      GROUP BY day;"

      title <- "Number of tweets per month"

    }

    # When exclude_RT == TRUE construct query and chart title
    else if (exclude_RT == TRUE) {

      query <- "SELECT count(*) as `tweets`, date(created_at) as `day`
      FROM tweet
      WHERE retweeted_tweet_id IS NULL
      GROUP BY day;"

      title <- "Number of tweets per month (excluding retweets)"

    }

    chart_data <- DBI::dbGetQuery(sqlite_con, query) %>%
      unique() %>%
      mutate(month = floor_date(ymd(.data$day), "month")) %>%
      group_by(.data$month) %>%
      summarise(tweets = sum(.data$tweets)) %>%
      filter(
       .data$month >= ymd(paste0(from, "-01")) &
         .data$month <= ymd(paste0(to, "-01"))) %>%
      as.data.frame()

    chart <- ggplot(chart_data, aes(x = .data$month, y = .data$tweets)) +
      geom_col(...) +
      labs(title = title,
          x = "Month",
          y = "Number of tweets") +
      scale_x_date(date_labels = "%b %Y") +
      configure_y_axis() +
      configure_ggplot_theme()

    if (return_data == TRUE) {
      return(list(chart = chart, data = chart_data))
    }

    else if (return_data == FALSE) {
      return(chart)
    }

  }
}
