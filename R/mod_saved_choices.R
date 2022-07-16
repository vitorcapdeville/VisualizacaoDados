#' saved_choices UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param colunasExp,colunasExpNome,ncols,estruturaPadrao,defaultValues,tipo,step adicionar descricao
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_saved_choices_ui <- function(id, colunasExp, colunasExpNome, ncols,estruturaPadrao, defaultValues, tipo, step = 1){
  ns <- NS(id)
  tagList(
    filtrosEstrutura(purrr::map(colunasExp, ns), colunasExpNome, ncols, estruturaPadrao, defaultValues, tipo, step = 1),
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
#' @noRd
mod_saved_choices_server <- function(id, savedChoices,colunasExp, defaultValues, tipo){
  moduleServer(
    id,
    function(input, output, session) {

      # Renderizar botão de reset (apenas se houver alguma modificacao)
      output$reset_button <- renderUI({
        ns <- session$ns
        if (any(unlist(purrr::map(colunasExp,comparacao1, defaultValues, input)))) {
          actionButton(
            inputId = ns('reset'),
            label = 'Resetar sele\u00E7\u00E3o'
          )
        }
      })

      # Efeito do botao reset
      observeEvent(input$reset, {
        purrr::walk2(colunasExp, tipo, updateXXXInput, session, defaultValues, input)
      })

      # Renderizar botao de salvar, azul se houver alteracao, branco caso contrario.
      output$save_button <- renderUI({
        ns <- session$ns
        status <- ifelse(all(unlist(purrr::map(colunasExp,comparacao2, input, savedChoices))),
                         'light', 'primary')
        actionButton(
          inputId = ns('save'),
          label = 'Salvar sele\u00E7\u00E3o',
          class = paste0('btn btn-', status)
        )
      })

      # Efeito do botao salvar
      observeEvent(input$save, {
        purrr::walk(colunasExp, saveInput, input, savedChoices)
      })

      return(savedChoices)
    }
  )
}

## To be copied in the UI
# mod_saved_choices_ui("saved_choices_1")

## To be copied in the server
# mod_saved_choices_server("saved_choices_1")
