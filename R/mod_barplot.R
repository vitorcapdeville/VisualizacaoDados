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
mod_barplot_server <- function(id, x, y, name, tickx, ticky, titlex, titley, barmode, value1, value2, name1, name2, table1, table2, filtro, colunas_transformadas, colunas_transformadas_nome){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    preTable <- mod_query_data_server(
      id = glue::glue_collapse(c(x, name), sep = "_"),
      con = con,
      group = c(x, name),
      value1 = value1, value2 = value2,
      name1 = name1, name2 = name2,
      table1 = table1, table2 = table2, filtro = filtro,
      colunas_transformadas = colunas_transformadas, colunas_transformadas_nome = colunas_transformadas_nome,
      cuts = NULL
    )

    output$barplot <- plotly::renderPlotly({
      bar_plot(data = preTable()$tabela, x = x, y = y, name = name, tickx = tickx, ticky = ticky, titlex = titlex, titley = titley, barmode = barmode)
    })
  })
}

## To be copied in the UI
# mod_barplot_ui("barplot_1")

## To be copied in the server
# mod_barplot_server("barplot_1")
