#' barplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_barplot_ui <- function(id, nome){
  ns <- NS(id)
  tabPanel(
    title = strong(nome),
    value = nome,
    fluidRow(
      column(12, shinycssloaders::withSpinner(plotly::plotlyOutput(ns("barplot"))))
    )
  )
}

#' barplot Server Functions
#'
#' @noRd
mod_barplot_server <- function(id, nome_tabela, x, y, name, tickx, ticky, titlex, titley, barmode, filtro, preTable){
  stopifnot(is.reactive(filtro))
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$barplot <- plotly::renderPlotly({
      bar_plot(data = preTable()$tabela, x = x, y = y, name = name, tickx = tickx, ticky = ticky, titlex = titlex, titley = titley, barmode = barmode)
    })
  })
}

## To be copied in the UI
# mod_barplot_ui("barplot_1")

## To be copied in the server
# mod_barplot_server("barplot_1")
