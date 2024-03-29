#' Funcao para criar barplot usando plotly
#'
#' @param data,x,y,name,tickx,ticky,titlex,titley,barmode parametros passados para plotly
#'
#' @noRd
bar_plot <- function(data, x, y, name, tickx = NULL, ticky = NULL, titlex = NULL, titley = NULL, barmode = c("group", "stack")) {
  barmode <- match.arg(barmode)
  stopifnot(!is.null(x))
  stopifnot(!is.null(y))
  x <- stats::formula(glue::glue("~`{x}`"))
  y <- stats::formula(glue::glue("~`{y}`"))
  name <- if (is.null(name)) name else stats::formula(glue::glue("~`{name}`"))
  fig <- plotly::plot_ly(data, x = x, y = y, name = name, type = "bar")
  plotly::layout(fig, yaxis = list(tickformat = ticky, title = titley), xaxis = list(tickformat = tickx, title = titlex), barmode = barmode)
}
