# ggplot2 theme
configure_ggplot_theme <- function() {
  my_theme <- ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 10, face = "plain"),
                   axis.title = ggplot2::element_text(size = 10, face = "plain"),
                   plot.title = ggplot2::element_text(size = 11, face = "plain"))
}


# ggplot2 y axis
configure_y_axis <- function() {
  my_y_axis <- ggplot2::scale_y_continuous(labels = scales::number_format(accuracy = 1, big.mark = ","))
}
