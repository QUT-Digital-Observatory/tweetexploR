#' Internal function to substitute missing value of `from` (optional parameter)
#'   to a very old date in the appropriate format
#'
#' @param period Time period for which the tweet frequency will be calculated.
#'   Accepted values are `"hour"`, `"day"`, or `"month"`. Defaults to `"day"`.
#'
#' @return from Optional parameter to subset the data to tweets that were
#'   created on or before the specified date and/or time in UTC.
#'   For hourly charts, provide a character string in the format
#'   `"%Y-%m-%d %H:%M:%S"`. For example `"2022-06-20 06:00:00"`
#'
#'   For daily charts, provide a character string in the format
#'   `"%Y-%m-%d"`. For example `"2022-06-20"`
#'
#'   For monthly charts, provide a character string in the format
#'   `"%Y-%m"`. For example `"2022-06"`

substitute_missing_from_input <- function(period) {
  if (period == "hour") {
    from <- "0001-01-01 01:00:00"
  }
  if (period == "day") {
    from <- "0001-01-01"
  }
  if (period == "month") {
    from <- "0001-01"
  }
  return(from)
}
