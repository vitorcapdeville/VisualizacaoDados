#' saved_choices UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param saved_choices reactiveValues que mantera as escolhas salvas atualmente.
#' @param id_inputs id dos inputs que estao sob o efeito do modulo
#' @param choices escolhas padrao para cada input em id_inputs
#' @param tipo tipo de cada input em id_input
#' @param ncols numero de colunas onde os filtros sertao arranjados
#' @param label,step,input_list argumentos passados para cria_filtros
#'
#'
#' @noRd
#'
#' @export
#'
#' @importFrom shiny NS tagList
mod_saved_choices_ui <- function(id, id_inputs, label, ncols, choices, tipo, step = 1, input_list = NULL){
  ns <- NS(id)
  tagList(
    tags$div(
      align = "center",
      filtrosEstrutura(
        ncols = ncols,
        input_list = cria_filtros(id = purrr::map(id_inputs, ns), label = label, tipo = tipo, choices = choices, step = step, input_list = input_list)
      ),
      hr(),
      fluidRow(
        column(6, uiOutput(ns('reset_button'))),
        column(6, uiOutput(ns('save_button')))
      )
    )
  )

}

#' saved_choices Server Functions
#'
#' @param saved_choices escolhas salvas pelo usuario. deve ser criado com reactiveValues
#'
#' @export
#'
#' @noRd
mod_saved_choices_server <- function(id, saved_choices, id_inputs, choices, tipo){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Renderizar botão de reset (apenas se houver alguma modificacao)
      output$reset_button <- renderUI({
        ns <- session$ns
        if (any(unlist(purrr::map2(choices[id_inputs], purrr::map(id_inputs, ~`[[`(input, .x)), ~comparacao1(choices = .x, input = .y))))) {
          actionButton(
            inputId = ns('reset'),
            label = 'Resetar sele\u00E7\u00E3o'
          )
        }
      })

      # Efeito do botao reset
      observeEvent(input$reset, {
        purrr::pwalk(list(id_inputs, tipo, choices[id_inputs], purrr::map(id_inputs, ~`[[`(input, .x))), ~updateXXXInput(inputId = ..1, tipo = ..2, session = session, choices = ..3, input = ..4))
      })

      # Renderizar botao de salvar, azul se houver alteracao, branco caso contrario.
      output$save_button <- renderUI({

        # browser()
        status <- ifelse(all(unlist(purrr::map2(purrr::map(id_inputs, ~`[[`(input, .x)), purrr::map(id_inputs, ~`[[`(saved_choices, .x)), ~comparacao2(input = .x, saved_choices = .y)))),
                         'light', 'primary')
        actionButton(
          inputId = ns('save'),
          label = 'Salvar sele\u00E7\u00E3o',
          class = paste0('btn btn-', status)
        )
      })

      # Efeito do botao salvar
      observeEvent(input$save, {
        purrr::walk(id_inputs, ~saveInput(inputId = .x,input = input, saved_choices = saved_choices))
      })

      return(saved_choices)
    }
  )
}

## To be copied in the UI
# mod_saved_choices_ui("saved_choices_1")

## To be copied in the server
# mod_saved_choices_server("saved_choices_1")
