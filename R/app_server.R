#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Essas variaveis vao definir o comportamento do app. Serao definidas em golem-config.yml
  con <- get_golem_config("con")
  colunasFiltro <- get_golem_config("colunasFiltro")
  colunasFiltroTipo <- get_golem_config("colunasFiltroTipo")
  colunasTabela <- get_golem_config("colunasTabela")
  tabela1 <- get_golem_config("tabela1")
  tabela2 <- get_golem_config("tabela2")
  colunasValorTabela1 <- get_golem_config("colunasValorTabela1")
  formatosValorTabela1 <- get_golem_config("formatosValorTabela1")
  colunasValorTabela2 <- get_golem_config("colunasValorTabela2")
  formatosValorTabela2 <- get_golem_config("formatosValorTabela2")
  defaultValues <- get_default_values(con = con, colunasFiltro = colunasFiltro, colunasFiltroTipo = colunasFiltroTipo)

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
    filtro <- unlist(purrr::map2(colunasFiltro, colunasFiltroTipo, criacao_filtro, savedChoices, defaultValues, con))

    if (!is.null(filtro)) {
      filtro <- paste0("where ", paste0(filtro, collapse = " and "))
    } else {
      filtro <- ""
    }
    list(filtro = filtro)
  })
  purrr::map2(colunasTabela, colunasTabela, mod_create_table_server, con, colunasValorTabela1, colunasValorTabela2, formatosValorTabela1, formatosValorTabela2, tabela1, tabela2, dados_filt, fixed = 1, widths = c("200px", "120px", "120px"))
  # mod_create_table_server("NomeProduto", "NomeProduto")

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
