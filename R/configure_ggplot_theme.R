#' Internal function to configure ggplot2 theme
#'
#' @return ggplot2 theme

configure_ggplot_theme <- function() {
  my_theme <- ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 10, face = "plain"),
                   axis.title = ggplot2::element_text(size = 10, face = "plain"),
                   plot.title = ggplot2::element_text(size = 11, face = "plain"))
}
