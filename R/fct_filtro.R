#' Criacao dos filtros para as queries.
#'
#' Verifica se os valores padrão foram modificados e cria os filtros necessários. Se todos os valores
#' padrão estão selecionados, isto é, se nenhum filtro foi realizado, cria uma string vazia para
#' evitar filtro desnecessário.
#'
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
criacao_filtro <- function(a, tipo, savedChoices, defaultValues, con) {
  if (!identical(savedChoices[[a]], defaultValues[[a]])) {
    if (tipo == "dateRange") {
      aux <- savedChoices[[a]] %>% lubridate::ymd() %>% as.numeric()
    } else {
      aux <- savedChoices[[a]]
      aux = paste0("'", aux, "'")
    }
    if (tipo %in% c("slider", "dateRange")) {
      filtro <- glue::glue("{a} >= {aux[1]} and {a} <= {aux[2]}")
    } else {
      filtro <- glue::glue("{a} in ({toString(aux)})")
    }
  }
}
