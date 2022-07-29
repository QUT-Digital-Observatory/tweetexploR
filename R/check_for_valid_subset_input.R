#' Internal function to check for valid subset input
#'
#' @description Many tweetexploR functions allow the user to subset the data
#'   via the `to` and `from` parameters. Depending on the time period for the
#'   chart they are creating with one of the tweetexploR functions (e.g.,
#'   `num_tweets_by_timeperiod()`), a specific date/time format is required, as
#'   follows:
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
#' @param period Period corresponding to the input to be checked
#'
#' @param input Input to be checked
#'
#' @importFrom lubridate ymd_hms ymd ym floor_date

check_for_valid_subset_input <- function(period, input) {

  if(period == "hour") {
    tryCatch(
      expr = {
        if(typeof(ymd_hms(input)) == "double") {
        }
      },
      error = function(e) {
        print(e)
      },
      warning = function(w) {
        print(w)
        warning("lubridate::ymd_hms() could not parse the input.\nEnsure it is in the format '%Y-%m-%d %H:%M:%S'.\nFor example: '2022-06-20 06:00:00'")
      }
    )
  }

  else if(period == "day") {
    tryCatch(
      expr = {
        if(typeof(ymd(input)) == "double") {
        }
      },
      error = function(e) {
        print(e)
      },
      warning = function(w) {
        print(w)
        warning("lubridate::ymd() could not parse the input.\nEnsure it is in the format '%Y-%m-%d'.\nFor example: '2022-06-20'")
      }
    )
  }

  else if(period == "month") {
    tryCatch(
      expr = {
        if(typeof(floor_date(ym(input), unit = "month")) == "double") {
        }
      },
      error = function(e) {
        print(e)
      },
      warning = function(w) {
        print(w)
        warning("lubridate::ym() could not parse the input.\nEnsure it is in the format '%Y-%m'.\nFor example: '2022-06'")
      }
    )
  }
}
