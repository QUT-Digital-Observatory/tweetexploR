#' Internal function to configure ggplot2 y axis
#'
#' @return ggplot2 y axis

configure_y_axis <- function() {
  my_y_axis <- ggplot2::scale_y_continuous(labels = scales::number_format(accuracy = 1, big.mark = ","))
}
