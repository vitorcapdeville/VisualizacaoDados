#' create_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param nome titulo da tabela para ser exibido na UI.
#' @param group variavel (ou variaveis) que serão usadas para o agrupamento
#' @param value variaveis que serão criadas para cada grupo, usando soma
#' @param table1 Nome da tabela 1 (usualmente, premio)
#' @param table2 Nome da tabela 1 (usualmente, sinistro)
#' @param filtro String com a estrutura do filtro em linguagem SQL
#' @param fixed Numero de colunas a serem fixadas na esquerda
#' @param widths Tamanho das colunas. Deve ser um vetor com 3 entradas, a 3 ira determinar
#' o tamanho das colunas restantes
#' @param align Usualmente 'left' ou 'center'
#' @param pageLenth Numero de entradas por página. Se for menor do que o numero de linhas da
#' tabela final, irá criar páginas.
#' @param scrollY TRUE ou FALSE, indica se ira ocorrer scroll no eixo Y
#' @param footer TRUE ou FALSE, indica se vai colocar o footer. Atualmente o footer só
#' funciona para as entradas exibidas na pagina.
#'
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_create_table_ui <- function(id, nome) {
  ns <- NS(id)
  shinydashboardPlus::box(
    title = strong(nome),
    id = ns("tabelaPadraoBox"),
    collapsible = TRUE,
    collapsed = T,
    closable = F,
    shinycssloaders::withSpinner(DT::DTOutput(ns("tabelaPadrao"))),
    width = 12,
    dropdownMenu = shinydashboardPlus::boxDropdown(shinydashboardPlus::boxDropdownItem(
      downloadButton(ns("tabelaPadraoDownload"), "Download"),
      id = ns("tabelaPadraoDownload2")
    ))
  )
}
#' create_table Server Functions
#'
#' @noRd
mod_create_table_server <- function(id, group, value1, value2, table1, table2, filtro, fixed = 1,
                                    widths = c("400px","200px","200px"), align = "left", scrollY = "600px", footer = T) {
  stopifnot(is.reactive(filtro))
  moduleServer(
    id,
    function(input, output, session) {

      preTable <- reactive({
        req(!input$tabelaPadraoBox$collapsed)
        tabela = getSQLTable(
          group,
          value1,
          value2,
          table1,
          table2,
          filtro()$filtro
        )
        list(tabela = tabela)
      })

      output$tabelaPadrao <- DT::renderDT({
        createDT(
          preTable()$tabela, group, fixed,
          widths, align, nrow(preTable()$tabela), scrollY, footer
        )

      })

      output$tabelaPadraoDownload <- downloadHandler(
        filename = function() {
          glue::glue("Resultado por {toString(group)}.xlsx")
        },
        content = function(file) {
          dados = preTable()$tabela
          dados[is.na(dados)] = 0
          openxlsx::write.xlsx(dados, file, row.names = F)
        }
      )

      return(preTable)
    }
  )
}

## To be copied in the UI
# mod_create_table_ui("create_table_1")

## To be copied in the server
# mod_create_table_server("create_table_1")
