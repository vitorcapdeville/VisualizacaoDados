#' download_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    downloadButton(ns("data_download"), "Download")
  )
}

#' download_data Server Functions
#'
#' @noRd
mod_download_data_server <- function(id, group, preTable, filtro){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$data_download <- downloadHandler(
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

  })
}

## To be copied in the UI
# mod_download_data_ui("download_data_1")

## To be copied in the server
# mod_download_data_server("download_data_1")
