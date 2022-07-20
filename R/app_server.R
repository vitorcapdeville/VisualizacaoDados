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

  purrr::pmap(
    list(colunasTabela, colunasTabela),
    ~mod_create_table_server(
      id = ..1, group = ..2, con = con,
      value1 = colunasValorTabela1, value2 = colunasValorTabela2,
      name1 = colunasValorNomeTabela1, name2 = colunasValorNomeTabela2,
      formats1 = formatosValorTabela1, formats2 = formatosValorTabela2,
      table1 = tabela1, table2 = tabela2, filtro = dados_filt, colunas_transformadas = colunasTransformadas,
      colunas_transformadas_nome = colunasTransformadasNome,formato_colunas_transformadas = formatosTransformadas,
      fixed = 1, widths = c("200px", "120px", "120px")
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
