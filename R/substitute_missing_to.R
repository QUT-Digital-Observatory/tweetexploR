# If `to` is missing, substitute with the current datetime

substitute_missing_to <- function(period) {
  if (period == "hour") {
    to <- now("UTC")
  }
  if (period == "day") {
    to <- substr(now("UTC"), 1, 10)
  }
  if (period == "month") {
    to <- substr(now("UTC"), 1, 7)
  }
}

# Do I need return(to)?
