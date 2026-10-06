utils::globalVariables("Statistic")
#' Colored summary table
#'
#' @param x A numeric vector.
#' @param digits Number of decimals to show.
#' @return A gt table.
#' @importFrom stats median sd
#' @export
colorful_summary_table <- function(x, digits = 2) {
  x <- x[!is.na(x)]
  df <- data.frame(
    Statistic = c("Mean", "Median", "SD", "Min", "Max"),
    Value = c(mean(x), median(x), sd(x), min(x), max(x))
  )

  gt::gt(df) |>
    gt::fmt_number(columns = "Value", decimals = digits) |>
    gt::tab_header(title = "Summary statistics") |>
    gt::tab_style(gt::cell_fill(color = "#ff6ec7"),
                  gt::cells_body(rows = Statistic == "Mean")) |>
    gt::tab_style(gt::cell_fill(color = "#ffb347"),
                  gt::cells_body(rows = Statistic == "Median")) |>
    gt::tab_style(gt::cell_fill(color = "#fdfd96"),
                  gt::cells_body(rows = Statistic == "SD")) |>
    gt::tab_style(gt::cell_fill(color = "#77dd77"),
                  gt::cells_body(rows = Statistic == "Min")) |>
    gt::tab_style(gt::cell_fill(color = "#6ec6ff"),
                  gt::cells_body(rows = Statistic == "Max"))
}
