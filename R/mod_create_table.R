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
  uiOutput(ns("tabela_final"))
  # tabPanel(;
  #   title = strong(nome),
  #   value = nome,
  #   fluidRow(
  #     div(
  #       align = "center",
  #       uiOutput(ns("downloadTable")),
  #       uiOutput(ns("dynamic_groups"))
  #     ),
  #     column(12, shinycssloaders::withSpinner(DT::DTOutput(ns("tabelaPadrao"))))
  #   )
  # )
}
#' create_table Server Functions
#'
#' @noRd
mod_create_table_server <- function(id, group, value1, value2, name1, name2, formats1, formats2, table1, table2, colunas_transformadas, colunas_transformadas_nome,
                                    formato_colunas_transformadas, fixed = 1, widths = c("400px","200px","200px"),
                                    align = "left", footer = T, filtro = NULL,
                                    downloadable = T, dynamic_groups = F, nome = NULL, collapsed = T) {
  stopifnot(is.reactive(filtro))
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
            # alguns auxiliares q vao ser precisos em diversos lugares mais p frente
      col_names = c(name1, name2, colunas_transformadas_nome)

      if (dynamic_groups) {
        breaks = mod_dynamic_groups_server(id = id, min = 0, max = 120, step = 0.1, label = group)
        pontos_corte <- reactive({
          req(length(breaks()$export) > 0)
          cuts = breaks()$export#purrr::map(sort(names(breaks)[startsWith(names(breaks), "fx")]), ~`[[`(breaks, .x))
          list(cuts = c(-Inf,cuts[[1]][1], sapply(cuts, `[[`, 2), +Inf))
        })
      } else {
        pontos_corte = NULL
      }

      # browser()
      preTable <- mod_query_data_server(
        id = "dados_tab",
        con = con,
        group = group,
        value1 = value1, value2 = value2,
        name1 = name1, name2 = name2,
        table1 = table1, table2 = table2, filtro = filtro,
        colunas_transformadas = colunas_transformadas, colunas_transformadas_nome = colunas_transformadas_nome,
        cuts = pontos_corte
      )


      if (downloadable) {
        mod_download_data_server(id = id, group = group, preTable = preTable, filtro = filtro)
      }


      output$tabelaPadrao <- DT::renderDT({
        if(dynamic_groups) req(pontos_corte)
        createDT(
          data = preTable()$tabela, fixed = fixed, cols = col_names,
          formats = c(formats1, formats2,formato_colunas_transformadas),
          widths = widths, align = align, footer = footer
        )
      })

      output$tabela_final <- renderUI({
        shinydashboardPlus::box(
          title = strong(nome),
          id = ns(nome),
          collapsible = TRUE,
          collapsed = collapsed,
          closable = F,
          shinycssloaders::withSpinner(DT::DTOutput(ns("tabelaPadrao"))),
          width = 12,
          sidebar = if(dynamic_groups) {
            shinydashboardPlus::boxSidebar(
              id = ns(paste0(nome, "_sidebar")),
              width = 25,
              mod_dynamic_groups_ui(ns(id))
            )
          } else {
            NULL
          },
          dropdownMenu = if(downloadable) shinydashboardPlus::boxDropdown(shinydashboardPlus::boxDropdownItem(mod_download_data_ui(ns(id)))) else NULL
        )
      })

      return(preTable)
    }
  )
}

## To be copied in the UI
# mod_create_table_ui("create_table_1")

## To be copied in the server
# mod_create_table_server("create_table_1")
