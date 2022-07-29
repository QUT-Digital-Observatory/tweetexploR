#' Internal function to configure ggplot2 y axis
#'
#' @return ggplot2 y axis
#'
#' @importFrom ggplot2 scale_y_continuous
#'
#' @importFrom scales number_format

configure_y_axis <- function() {
  my_y_axis <- scale_y_continuous(labels = number_format(accuracy = 1,
                                                         big.mark = ","))
}
