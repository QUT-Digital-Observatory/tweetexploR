# Check for valid `period`

check_for_valid_period <- function(period) {
  stopifnot(
    "Please provide a valid value for `period`.
      Accepted values are \"day\", \"hour\", or \"month\"."
    = period %in% c("hour", "day", "month")
  )
}
