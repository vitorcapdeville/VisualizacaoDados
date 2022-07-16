#' server
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
updateXXXInput <- function(a, tipo, session, defaultValues, input) {
  if (tipo == "picker") {
    if (comparacao1(a, defaultValues, input)) {
      updatePickerInput(session, a, selected = defaultValues[[a]])
    }
  } else if (tipo == "slider") {
    if (comparacao1(a, defaultValues, input)) {
      updateSliderInput(session, a, value = defaultValues[[a]])
    }
  } else if (tipo == "checkBox") {
    if (comparacao1(a, defaultValues, input)) {
      updateCheckboxGroupButtons(session, a, selected = defaultValues[[a]])
    }
  } else if (tipo == "dateRange") {
    if (comparacao1(a, defaultValues, input)) {
      updateDateRangeInput(session, a, start = defaultValues[[a]][1], end = defaultValues[[a]][2])
    }
  }
}

saveInput <- function(a, input, savedChoices) {
  if (!(comparacao2(a, input, savedChoices))) {
    savedChoices[[a]] <- input[[a]]
  }
}
