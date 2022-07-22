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
mod_barplot_server <- function(id, nome_tabela, x, y, name, tickx, ticky, titlex, titley, barmode, filtro){
  stopifnot(is.reactive(filtro))
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    preGraph <- reactive({
      query = subquery_padrao(
        con = con,
        group = c(x,name),
        value1 = y,
        name1 = y,
        table1 = nome_tabela,
        filtro = filtro()$filtro
      )

      tabela <- DBI::dbGetQuery(
        con,
        query
      ) %>% dplyr::arrange(dplyr::across(dplyr::all_of(x)))

      # Remove coisas q podem causas problemas no filtro.
      tabela[is.na(tabela)] <- 0
      tabela[tabela == Inf | tabela == -Inf] <- 0
      list(tabela = tabela)
    })


    output$barplot <- plotly::renderPlotly({
      bar_plot(data = preGraph()$tabela, x = x, y = y, name = name, tickx = tickx, ticky = ticky, titlex = titlex, titley = titley, barmode = barmode)
    })
    return(preGraph)
  })
}

## To be copied in the UI
# mod_barplot_ui("barplot_1")

## To be copied in the server
# mod_barplot_server("barplot_1")
