# Check for valid subset input

check_for_valid_subset_input <- function(period, input) {

  if(period == "hour") {

    tryCatch(

      expr = {
        if(typeof(lubridate::ymd_hms(input)) == "double") {
        }
      },

      warning = function(w) {
        message("lubridate::ymd_hms() could not parse the input.\nEnsure it is in the format '%Y-%m-%d %H:%M:%S'.\nFor example: '2022-06-20 06:00:00'")
      }
    )
  }

  else if(period == "day") {
  # Check for valid `input`
  }

  else if(period == "month") {
  # Check for valid `input`
  # Use lubridate::floor_date(ym(input), unit = "month")
  }

}
