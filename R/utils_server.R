#' Update input
#'
#' atualiza um input de picker, slider, checkbox ou dateRange.
#'
#' @param coluna_filtro string com o nome da coluna de filtro
#' @param coluna_filtro_tipo string com o tipo de filtro.
#' @param session requerido pelo shiny.
#' @param default_values valores padrao para o input.
#' @param input valores q o usuario escolheu para o input.
#'
#' @noRd
updateXXXInput <- function(inputId, tipo, session, choices, input) {
  if (tipo == "picker") {
    if (comparacao1(choices, input)) {
      shinyWidgets::updatePickerInput(session, inputId, selected = choices)
    }
  } else if (tipo == "slider") {
    if (comparacao1(choices, input)) {
      updateSliderInput(session, inputId, value = choices)
    }
  } else if (tipo == "checkBox") {
    if (comparacao1(choices, input)) {
      shinyWidgets::updateCheckboxGroupButtons(session, inputId, selected = choices)
    }
  } else if (tipo == "dateRange") {
    if (comparacao1(choices, input)) {
      updateDateRangeInput(session, inputId, start = choices[1], end = choices[2])
    }
  }
}

#' Save input para o saved_choices
#'
#' @param coluna_filtro string com o nome da coluna de filtro.
#' @param input valores que o usuario escolheu
#' @param saved_choices valores escolhidos e confirmados anteriormente.
#'
#' @noRd
saveInput <- function(inputId, input, saved_choices) {
  if (!(comparacao2(input[[inputId]], saved_choices[[inputId]]))) {
    saved_choices[[inputId]] <- input[[inputId]]
  }
}
