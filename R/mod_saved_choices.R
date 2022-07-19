#' saved_choices UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param colunas_filtro nome das colunas (identicos ao sql) nas quais serao criados os filtros.
#' @param colunas_filtro_nome nome que será de fato exibido próximo ao filtro.
#' @param colunas_filtro_tipo tipo de filtro associado a coluna.
#' @param ncols numero de colunas para a estrutura padrao
#' @param estruturaPadrao funcao que define a estrutura padrao
#' @param default_values valores default de todos os filtros.
#' @param step só é usado quando tipo = slider. Define o step do slider.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_saved_choices_ui <- function(id, colunas_filtro, colunas_filtro_nome, ncols,estruturaPadrao, default_values, colunas_filtro_tipo, step = 1){
  ns <- NS(id)
  tagList(
    filtrosEstrutura(
      id = purrr::map(colunas_filtro, ns), colunas_filtro_nome = colunas_filtro_nome, ncols = ncols,
      estruturaPadrao = estruturaPadrao, default_values = default_values,
      colunas_filtro_tipo = colunas_filtro_tipo, step = 1
    ),
    hr(),
    tagList(
      tags$div(
        align = "center",
        fluidRow(
          column(6, uiOutput(ns('reset_button'))),
          column(6, uiOutput(ns('save_button')))
        )
      )
    )
  )
}

#' saved_choices Server Functions
#'
#' @param saved_choices escolhas salvas pelo usuario. deve ser criado com reactiveValues
#'
#' @noRd
mod_saved_choices_server <- function(id, saved_choices, colunas_filtro, default_values, colunas_filtro_tipo){
  moduleServer(
    id,
    function(input, output, session) {
      # Renderizar botão de reset (apenas se houver alguma modificacao)
      output$reset_button <- renderUI({
        ns <- session$ns
        if (any(unlist(purrr::map(colunas_filtro, ~comparacao1(coluna_filtro = .x, defaultValues = default_values, input = input))))) {
          actionButton(
            inputId = ns('reset'),
            label = 'Resetar sele\u00E7\u00E3o'
          )
        }
      })

      # Efeito do botao reset
      observeEvent(input$reset, {
        purrr::walk2(colunas_filtro, colunas_filtro_tipo, ~updateXXXInput(coluna_filtro = .x,coluna_filtro_tipo = .y, session = session,default_values = default_values, input = input))
      })

      # Renderizar botao de salvar, azul se houver alteracao, branco caso contrario.
      output$save_button <- renderUI({
        ns <- session$ns
        status <- ifelse(all(unlist(purrr::map(colunas_filtro, ~comparacao2(coluna_filtro = .x , input = input, savedChoices = saved_choices)))),
                         'light', 'primary')
        actionButton(
          inputId = ns('save'),
          label = 'Salvar sele\u00E7\u00E3o',
          class = paste0('btn btn-', status)
        )
      })

      # Efeito do botao salvar
      observeEvent(input$save, {
        purrr::walk(colunas_filtro, saveInput, input, saved_choices)
      })

      return(saved_choices)
    }
  )
}

## To be copied in the UI
# mod_saved_choices_ui("saved_choices_1")

## To be copied in the server
# mod_saved_choices_server("saved_choices_1")
