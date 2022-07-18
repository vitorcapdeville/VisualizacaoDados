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
updateXXXInput <- function(coluna_filtro, coluna_filtro_tipo, session, default_values, input) {
  if (coluna_filtro_tipo == "picker") {
    if (comparacao1(coluna_filtro, default_values, input)) {
      shinyWidgets::updatePickerInput(session, coluna_filtro, selected = default_values[[coluna_filtro]])
    }
  } else if (coluna_filtro_tipo == "slider") {
    if (comparacao1(coluna_filtro, default_values, input)) {
      updateSliderInput(session, coluna_filtro, value = default_values[[coluna_filtro]])
    }
  } else if (coluna_filtro_tipo == "checkBox") {
    if (comparacao1(coluna_filtro, default_values, input)) {
      shinyWidgets::updateCheckboxGroupButtons(session, coluna_filtro, selected = default_values[[coluna_filtro]])
    }
  } else if (coluna_filtro_tipo == "dateRange") {
    if (comparacao1(coluna_filtro, default_values, input)) {
      updateDateRangeInput(session, coluna_filtro, start = default_values[[coluna_filtro]][1], end = default_values[[coluna_filtro]][2])
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
saveInput <- function(coluna_filtro, input, saved_choices) {
  if (!(comparacao2(coluna_filtro, input, saved_choices))) {
    saved_choices[[coluna_filtro]] <- input[[coluna_filtro]]
  }
}
