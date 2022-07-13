# If `from` is missing, substitute with a very old date

substitute_missing_from <- function(period) {
  if (period == "hour") {
    from <- "0001-01-01 01:00:00"
  }
  if (period == "day") {
    from <- "0001-01-01"
  }
  if (period == "month") {
    from <- "0001-01"
  }
}

# Do I need return(from)?
