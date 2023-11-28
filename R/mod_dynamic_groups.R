#' dynamic_groups UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @export
#'
#' @importFrom shiny NS tagList
mod_dynamic_groups_ui <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("qntd_grupos"), "Quantidade de faixas", 4, min = 1, max = 10),
    uiOutput(ns("sliders_fx"))
  )
}

#' dynamic_groups Server Functions
#'
#' @param faixas lista com as faixas selecionadas
#'
#' @export
#'
#' @noRd
mod_dynamic_groups_server <- function(id, min, max, step, label = "") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    reac_aux <- reactive({
      req(input$qntd_grupos)
      aux1 <- seq(from = min, to = max, length.out = input$qntd_grupos + 1)
      aux2 <- dplyr::lead(aux1)
      default_values <- purrr::map2(aux1[seq(input$qntd_grupos)], aux2[seq(input$qntd_grupos)], ~ c(.x + step, .y))
      default_values[[1]][1] <- min
      sliders_it <- seq(length(default_values))
      list(default_values = default_values, sliders_it = sliders_it)
    })

    output$sliders_fx <- renderUI({
      req(input$qntd_grupos)
      tagList(
        purrr::pmap(
          list(reac_aux()$sliders_it, reac_aux()$default_values),
          ~ sliderInput(
            inputId = ns(paste("fx", ..1, sep = "")),
            label = ifelse(..1 == 1, label, ""),
            min = min, max = max, step = .1,
            value = ..2,
            width = "90%"
          )
        ),
        actionButton(ns("confirmar"), label = "Salvar")
      )
    })

    update_sliders <- function(i_max, i, input, step = .1) {
      current <- paste("fx", i, sep = "")
      ant <- paste("fx", i - 1, sep = "")
      prox <- paste("fx", i + 1, sep = "")
      observeEvent(purrr::map(current, ~ `[[`(input, .x)), ignoreInit = T, {
        if (i > 1) {
          values <- c(min(input[[ant]][1], input[[current]][1] - step * 2), input[[current]][1] - step)
          updateSliderInput(session, ant, value = values, step = step)
        }
        if (i < i_max) {
          # browser()
          values <- c(input[[current]][2] + step, max(input[[prox]][2], input[[current]][2] + step * 2))
          updateSliderInput(session, prox, value = values, step = step)
        }
      })
    }
    observe({
      req(reac_aux()$sliders_it)
      purrr::map(
        reac_aux()$sliders_it,
        ~ update_sliders(i = .x, input = input, i_max = max(reac_aux()$sliders_it))
      )
    })

    return(
      eventReactive(input$confirmar, {
        # Tomar cuidado aqui. Se o botao de confirmar for criado antes dos sliders, isso vai comecar
        # sem ser executado
        names_export = sort(names(input)[startsWith(names(input), "fx")])[seq(input$qntd_grupos)]
        list(export = purrr::map(names_export, ~`[[`(input, .x)))
      },ignoreNULL = F, ignoreInit = T)
    )
  })
}

## To be copied in the UI
# mod_dynamic_groups_ui("dynamic_groups_1")

## To be copied in the server
# mod_dynamic_groups_server("dynamic_groups_1")
