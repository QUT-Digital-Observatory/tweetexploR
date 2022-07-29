#' Internal function to configure ggplot2 theme
#'
#' @return ggplot2 theme
#'
#' @importFrom ggplot2 theme_bw theme element_blank element_text

configure_ggplot_theme <- function() {
  my_theme <- theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.text = element_text(size = 10, face = "plain"),
          axis.title = element_text(size = 10, face = "plain"),
          plot.title = element_text(size = 11, face = "plain"))
}
