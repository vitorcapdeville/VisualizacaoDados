#' Helper for javascript code
#'
#' @param type Character. Select start, end or sketch
#' @param df data.frame. Needed for sketch only
#' @export
js_op_aux <- function(type, df = NULL, align) {
  if (type == "start") {
    aux <- "function(tfoot, data, start, end, display ) {var api = this.api(), data;"
  }
  if (type == "end") {
    aux <- ";}"
  }
  if (type == "sketch") {
    if (!is.null(df)[1]) {
      aux <- htmltools::withTags(div(
        align = align,
        htmltools::tags$table(
          tableHeader(names(df)),
          tableFooter(rep("", ncol(df)))
        )
      ))
    } else {
      stop("You must define df parameter with your data!")
    }
  }
  return(aux)
}


#' Totals Row for datatables
#'
#' @param column Integer. Starting from 0, which column to operate
#' @param operation Character. Select from sum, mean, count, custom
#' @param txt Character. Insert text before (or instead) operation
#' @param signif Integer. How many decimals to consider when operating
#' @export
js_op <- function(column, operation, txt = "", signif = 3) {

  # Auxiliar function for mean
  aux <- ifelse(
    operation == "mean",
    paste0("map(function(num) { return num/data.length; })."), ""
  )

  # Decimals to consider
  signif <- 10^signif

  # Operation
  if (operation %in% c("sum", "mean")) {
    operation <- paste0("Math.round((a+b)*", signif, ")/", signif)
  }
  if (operation == "count") {
    operation <- "data.length"
  }
  if (operation == "custom") {
    return(paste0("$(api.column(", column, ").footer()).html('", txt, "')"))
  }
  # Result
  res <- paste0(
    "$(api.column(", column, ").footer()).html('", txt, "'+",
    "api.column(", column, ").data().", aux, "reduce( function ( a, b ) {",
    "return ", operation, ";",
    "} ).toLocaleString('pt-BR',{minimumFractionDigits: 2, maximumFractionDigits: 2}));"
  )
  return(res)
}
