#' Internal function to check if `period` is valid
#'
#' @description If `period` is not valid, the program will stop and give a
#'   message. If `period` is valid, the program will continue.
#'
#' @param period Time period to be checked

check_if_period_is_valid <- function(period) {
  stopifnot(
    "Please provide a valid value for `period`.
      Accepted values are \"day\", \"hour\", or \"month\"."
    = period %in% c("hour", "day", "month")
  )
}
