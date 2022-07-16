#'
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
comparacao1 <- function(a, defaultValues, input) {
  return(!all(defaultValues[[a]] %in% input[[a]]))
}

comparacao2 <- function(a, input, savedChoices) {
  return(all(savedChoices[[a]] %in% input[[a]]) &
    all(input[[a]] %in% savedChoices[[a]]))
}
