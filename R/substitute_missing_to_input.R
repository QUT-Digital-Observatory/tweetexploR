#' Internal function to substitute missing value of `to` (optional parameter) to
#'   the current datetime in the appropriate format
#'
#' @param period Time period for which the tweet frequency will be calculated.
#'   Accepted values are `"hour"`, `"day"`, or `"month"`. Defaults to `"day"`.
#'
#' @return to Optional parameter to subset the data to tweets that were created
#'   on or before the specified date and/or time in UTC.
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
#' @importFrom lubridate now

substitute_missing_to_input <- function(period) {
  if (period == "hour") {
    to <- now("UTC")
  }
  if (period == "day") {
    to <- substr(now("UTC"), 1, 10)
  }
  if (period == "month") {
    to <- substr(now("UTC"), 1, 7)
  }
  return(to)
}
