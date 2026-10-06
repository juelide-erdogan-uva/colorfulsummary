utils::globalVariables("Statistic")
#' Rainbow histogram
#'
#' @param x A numeric vector.
#' @param bins Number of bars.
#' @param colors Colors for the gradient, from low to high values.
#' @param title Plot title.
#' @return A ggplot object.
#' @importFrom stats median
#' @export
rainbow_hist <- function(x, bins = 30,
                         colors = c("#ff6ec7", "#ffb347", "#fdfd96",
                                    "#77dd77", "#6ec6ff", "#b19cd9"),
                         title = "Look at this distribution!") {
  x <- x[!is.na(x)]
  df <- data.frame(x = x)
  ggplot2::ggplot(df, ggplot2::aes(x = x, fill = ggplot2::after_stat(x))) +
    ggplot2::geom_histogram(bins = bins, color = "white") +
    ggplot2::scale_fill_gradientn(colors = colors, guide = "none") +
    ggplot2::geom_vline(xintercept = median(x), linetype = "dotted", linewidth = 1) +
    ggplot2::labs(title = title,
                  subtitle = paste0("Dotted line = median (", round(median(x), 1), ")"),
                  x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   panel.grid.minor = ggplot2::element_blank())
}
