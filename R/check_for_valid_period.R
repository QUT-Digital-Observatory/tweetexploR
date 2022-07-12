# Check for valid period

check_for_valid_period <- function(period) {

  if(period == "day" || period == "hour" || period == "month"){
    } else {
      stop("Please provide a valid value for `period`. Accepted values are 'day', 'hour', or 'month'.")
      }
  }
