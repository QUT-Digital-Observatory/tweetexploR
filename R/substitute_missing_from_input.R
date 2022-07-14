substitute_missing_from_input <- function(period, from) {
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
