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
mod_create_table_ui <- function(id, nome, value1, value2, name1, name2) {
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
mod_create_table_server <- function(id, group, group_name, con, value1, value2, name1, name2, formats1, formats2, table1, table2, filtro,
                                    colunas_transformadas, colunas_transformadas_nome,formato_colunas_transformadas,
                                    fixed = 1, widths = c("400px","200px","200px"), align = "left", footer = T, current_tab = NULL) {
  stopifnot(is.reactive(filtro))
  moduleServer(
    id,
    function(input, output, session) {
      # alguns auxiliares q vao ser precisos em diversos lugares mais p frente
      ids = c(value1, value2,colunas_transformadas)
      col_names = c(name1, name2, colunas_transformadas_nome)

      # Tras os dados do SQL, com as colunas iniciais
      preTable <- reactive({
        # Isso evita rodar esse reactive para as tabs q nao estao abertas no momento
        req(current_tab() == group_name)
        tabela = query_padrao(
          con = con,
          group = group,
          value1 = value1,
          value2 = value2,
          name1 = name1,
          name2 = name2,
          table1 = table1,
          table2 = table2,
          filtro = filtro()$filtro
        )

        # Adiciona as colunas que sao criadas pos agregacao.
        tabela <- purrr::reduce2(
          .x = colunas_transformadas,
          .y = colunas_transformadas_nome,
          ~dplyr::mutate(..1, "{..3}" := eval(parse(text = ..2))),
          .init = tabela
        )
        # Remove coisas q podem causas problemas no filtro.
        tabela[is.na(tabela)] <- 0
        tabela[tabela == Inf | tabela == -Inf] <- 0
        list(tabela = tabela)
      })

      output$tabelaFiltros <- renderUI({
        # Esse render UI evita q eu precise criar os rangeInputs vazios
        # e depois preencher com o update. Isso tava me causando problema com o estado inicial.
        # Assim eu ja crio eles populados e consigo garantir que a primeira execucao do createDT
        # sera baseada nos valores populados desses filtros, que devem ser a base completa.
        ns <- session$ns
        dados = preTable()$tabela

        # cols = dados %>% dplyr::select(dplyr::all_of(col_names))
        # min_max = purrr::map(cols, function(x) c(floor(min(x,na.rm = T)*100)/100, ceiling(max(x, na.rm = T)*100)/100))
        tagList(
          shinyWidgets::dropdown(
            div(align = "center",
                downloadButton(ns("tabelaPadraoDownload"), "Download"),
                # hr(),
                # purrr::pmap(
                #   list(ids, col_names, min_max),
                #   ~shinyWidgets::numericRangeInput(ns(..1), ..2, value = ..3, width = "90%", separator = " at\u00E9 ")
                # )
            ),
            size = "md",icon = icon("gear", verify_fa = F), right = T, width = "400px"
          )
        )
      })

      output$tabelaPadrao <- DT::renderDT({
        # lidando com os filtros dessa forma, eu consigo criar dependencia so nos exatos inputs q eu preciso,
        # isto e, aqueles criados com o renderUI acima.
        # filtros = purrr::map(ids, ~`[[`(input, .x))
        # names(filtros) = ids
        # # Esse req evita q ele tente criar o DT antes de renderizar os filtros.
        # req(filtros[[1]])

        # dados = isolate(preTable()$tabela)
        dados = preTable()$tabela
        # ids = c(value1, value2)
        # col_names <- c(name1, name2)
        # Aplica os filtros de tabela
        # dados <- dados %>%
        #   dplyr::filter((dplyr::if_all(col_names, ~dplyr::between(.x, filtros[[ids[col_names == dplyr::cur_column()]]][1], filtros[[ids[col_names == dplyr::cur_column()]]][2]))))
        createDT(
          data = dados, fixed = fixed, cols = col_names,
          formats = c(formats1, formats2,formato_colunas_transformadas),
          widths = widths, align = align, footer = footer
        )
      })

      # proxy <- DT::dataTableProxy('tabelaPadrao', deferUntilFlush = F)
      # observe({
      #   # Isso evita rodar esse observe para as tabs q nao estao abertas no momento
      #   req(current_tab() == group_name)
      #   dados = preTable()$tabela
      #   DT::replaceData(proxy, data = dados, rownames = F)
      # })


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
