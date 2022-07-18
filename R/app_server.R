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
  purrr::map2(
    colunasTabela, colunasTabela, mod_create_table_server, con,
    colunasValorTabela1, colunasValorTabela2,
    colunasValorNomeTabela1, colunasValorNomeTabela2,
    formatosValorTabela1, formatosValorTabela2,
    tabela1, tabela2,
    dados_filt, fixed = 1, widths = c("200px", "120px", "120px")
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
