#' Custom ggplot2 theme
#'
#' This function provides a custom ggplot2 theme for consistent styling across plots.
#'
#' @return A ggplot2 theme object.
#' @examples
#' library(ggplot2)
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   plot_theme()
#'
#' @export
plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
    plot.title = ggplot2::element_text(size = 16, face = "bold"),
    axis.title = ggplot2::element_text(size = 14),
    axis.text = ggplot2::element_text(size = 10),
    legend.title = ggplot2::element_text(size = 12, face = "bold"),
    legend.text = ggplot2::element_text(size = 10),
    #panel.background = ggplot2::element_rect(fill = "cornsilk", color = NA),
    panel.grid.major = ggplot2::element_line(color = "#b29a9a", linetype = "dashed"),
    panel.grid.minor = ggplot2::element_blank(),
    #plot.background = ggplot2::element_rect(fill = "cornsilk", color = NA),
    plot.margin = ggplot2::margin(1, 1, 1, 1, "cm"),
    #strip.background = ggplot2::element_rect(fill = "cornsilk", color = "#ff975d"),
    strip.text = ggplot2::element_text(size = 12, face = "bold"),
    plot.title.position = "plot",
    legend.position = "right",
    #legend.box.background = ggplot2::element_rect(color = "cornsilk"),
    legend.key.size = ggplot2::unit(1, "cm")
  )
}

# Example usage:
# ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#   ggplot2::geom_point() +
#   plot_theme()
