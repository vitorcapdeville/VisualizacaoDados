#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  #------------------------------Estrutura para salvar selecao--------------------------------------
  # Valor inicial do savedChoices
  savedChoices <- reactiveValues()


  for (i in colunasFiltro) {
    savedChoices[[i]] <- defaultValues[[i]]
  }
  mod_saved_choices_server("filtros", savedChoices, colunasFiltro, defaultValues, colunasFiltroTipo)

  #--------------------------------------------------Calculos da aba -------------------------------
  # Criação dos filtros necessário. Se não for necessário nenhum filtro, não filtrar.
  dados_filt <- reactive({
    filtro <- unlist(purrr::pmap(list(colunasFiltro, colunasFiltroTipo, rvtl(savedChoices)[colunasFiltro], defaultValues[colunasFiltro]), criacao_filtro, con))

    if (!is.null(filtro)) {
      filtro <- glue::glue_sql("where ", glue::glue_sql_collapse(filtro, sep = " and "), .con = con)
    } else {
      filtro <- glue::glue_sql("", .con = con)
    }
    list(filtro = filtro)
  })

  # Save current value of tabset to pass do create table module
  current_tabset <- reactiveVal()
  observe({
    current_tabset(input$tabset1)
  })

  purrr::pmap(
    list(colunasTabela, colunasTabela, colunasTabelaNome),
    ~mod_create_table_server(
      id = ..1, group = ..2, group_name = ..3, con = con,
      value1 = colunasValorTabela1, value2 = colunasValorTabela2,
      name1 = colunasValorNomeTabela1, name2 = colunasValorNomeTabela2,
      formats1 = formatosValorTabela1, formats2 = formatosValorTabela2,
      table1 = tabela1, table2 = tabela2, filtro = dados_filt, colunas_transformadas = colunasTransformadas,
      colunas_transformadas_nome = colunasTransformadasNome,formato_colunas_transformadas = formatosTransformadas,
      fixed = 1, widths = c("200px", "120px", "120px"), current_tab = current_tabset
    )
  )

  purrr::pmap(
    list(glue::glue("{colunas_x}x{colunas_y}"), colunas_x, colunas_y, colunas_name, tickx, ticky, titlex, titley, barmode),
    ~mod_barplot_server(
      id = ..1, nome_tabela = "bill", x = ..2, y = ..3, name = ..4, tickx = ..5, ticky = ..6,
      titlex = ..7, titley = ..8, barmode = ..9, filtro = dados_filt
    )
  )

  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  } else {
    onStop(function() {
      DBI::dbDisconnect(con)
      stopApp()
    })
  }
}
