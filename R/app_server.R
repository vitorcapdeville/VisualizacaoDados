#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  #------------------------------Estrutura para salvar seleção--------------------------------------
  # Valor inicial do savedChoices
  savedChoices <- reactiveValues()

  for (i in colunasExp) {
    savedChoices[[i]] <- defaultValues[[i]]
  }

  mod_saved_choices_server("filtros", savedChoices, colunasExp, defaultValues, colunasExpTipo)

  #--------------------------------------------------Calculos da aba -------------------------------
  # Criação dos filtros necessário. Se não for necessário nenhum filtro, não filtrar.
  dados_filt <- reactive({
    filtro <- unlist(purrr::map2(colunasExp, colunasExpTipo, criacao_filtro, savedChoices))

    if (!is.null(filtro)) {
      filtro <- paste0("where ", paste0(filtro, collapse = " and "))
    } else {
      filtro <- ""
    }
    list(filtro = filtro)
  })

  mod_create_table_server("NomeProduto", "NomeProduto", colunasValorTabela1, colunasValorTabela2, formatosValor1, formatosValor2, tabela1, tabela2, dados_filt, fixed = 1, widths = c("200px", "120px", "120px"))

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
