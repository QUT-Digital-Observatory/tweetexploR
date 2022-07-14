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
