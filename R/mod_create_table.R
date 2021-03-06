#' create_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param nome titulo da tabela para ser exibido na UI.
#' @param group variavel (ou variaveis) que serão usadas para o agrupamento
#' @param value1 variaveis que serão criadas para cada grupo, usando soma, originadas da tabela1
#' @param value2 variaveis que serão criadas para cada grupo, usando soma, originadas da tabela2
#' @param formats1 "perc" ou "number". Define o tipo de formatacao a ser aplicada as colunas definidas em value1
#' @param formats2 "perc" ou "number". Define o tipo de formatacao a ser aplicada as colunas definidas em value1
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

  tabPanel(
    title = strong(nome),
    value = nome,
    fluidRow(
      column(11, shinycssloaders::withSpinner(DT::DTOutput(ns("tabelaPadrao")))),
      column(
        1,
        div(
          align = "right",
          uiOutput(ns("tabelaFiltros"))
        )
      )
    )
  )
}
#' create_table Server Functions
#'
#' @noRd
mod_create_table_server <- function(id, group, name1, name2, formats1, formats2, colunas_transformadas_nome,
                                    formato_colunas_transformadas, fixed = 1, widths = c("400px","200px","200px"),
                                    align = "left", footer = T, filtro = NULL, preTable = NULL) {
  stopifnot(is.reactive(filtro))
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
            # alguns auxiliares q vao ser precisos em diversos lugares mais p frente
      col_names = c(name1, name2, colunas_transformadas_nome)


      output$tabelaFiltros <- renderUI({

        dados = preTable()$tabela

        tagList(
          shinyWidgets::dropdown(
            div(align = "center",
                downloadButton(ns("tabelaPadraoDownload"), "Download"),
            ),
            size = "md",icon = icon("gear", verify_fa = F), right = T, width = "400px"
          )
        )
      })

      output$tabelaPadrao <- DT::renderDT({
        createDT(
          data = preTable()$tabela, fixed = fixed, cols = col_names,
          formats = c(formats1, formats2,formato_colunas_transformadas),
          widths = widths, align = align, footer = footer
        )
      })


      output$tabelaPadraoDownload <- downloadHandler(
        filename = function() {
          glue::glue("Resultado por {toString(group)}.xlsx")
        },
        content = function(file) {
          # Aqui eu me pergunto se o melhor seria baixar o arquivo com os filtros de tabela
          # isto e, os filtros de valor, ou se devo usar so os filtros principais.
          # Talvez incluir um checkbox pra voce marcar se quiser filtrado?
          dados = preTable()$tabela
          dados[is.na(dados)] = 0
          wb <- openxlsx::createWorkbook()

          openxlsx::addWorksheet(wb, glue::glue("Resultado por {toString(group)}"))
          openxlsx::writeData(wb, 1, dados, startRow = 3, startCol = 1)
          openxlsx::writeData(wb, 1, filtro()$filtro, startRow = 1, startCol = 1, colNames = F, rowNames = F)

          openxlsx::saveWorkbook(wb, file)
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
