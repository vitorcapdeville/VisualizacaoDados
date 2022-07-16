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
    filtrosEstrutura(map(colunasExp, ns), colunasExpNome, ncols, estruturaPadrao, defaultValues, tipo, step = 1),
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

      # Renderizar botão de reset (apenas se houver alguma modificação)
      output$reset_button <- renderUI({
        ns <- session$ns
        if (any(unlist(map(colunasExp,comparacao1, defaultValues, input)))) {
          actionButton(
            inputId = ns('reset'),
            label = 'Resetar seleção'
          )
        }
      })

      # Efeito do botão reset
      observeEvent(input$reset, {
        walk2(colunasExp, tipo, updateXXXInput, session, defaultValues, input)
      })

      # Renderizar botão de salvar, azul se houver alteração, branco caso contrário.
      output$save_button <- renderUI({
        ns <- session$ns
        status <- ifelse(all(unlist(map(colunasExp,comparacao2, input, savedChoices))),
                         'light', 'primary')
        actionButton(
          inputId = ns('save'),
          label = 'Salvar seleção',
          class = paste0('btn btn-', status)
        )
      })

      # Efeito do botão salvar
      observeEvent(input$save, {
        walk(colunasExp, saveInput, input, savedChoices)
      })

      return(savedChoices)
    }
  )
}

## To be copied in the UI
# mod_saved_choices_ui("saved_choices_1")

## To be copied in the server
# mod_saved_choices_server("saved_choices_1")
