# Check if `period` is valid
check_if_period_is_valid <- function(period) {
  stopifnot(
    "Please provide a valid value for `period`.
      Accepted values are \"day\", \"hour\", or \"month\"."
    = period %in% c("hour", "day", "month")
  )
}
