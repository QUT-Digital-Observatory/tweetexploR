# Check for valid subset input

check_for_valid_subset_input <- function(period, input) {

  if(period == "hour") {

    tryCatch(

      expr = {
        if(typeof(lubridate::ymd_hms(input)) == "double") {
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
        if(typeof(lubridate::ymd(input)) == "double") {
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
        if(typeof(lubridate::floor_date(lubridate::ym(input), unit = "month")) == "double") {
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
