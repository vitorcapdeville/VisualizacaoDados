#' filtro
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
criacao_filtro <- function(a, tipo, savedChoices) {
  if (!identical(savedChoices[[a]], defaultValues[[a]])) {
    if (tipo == "dateRange") {
      aux <- data.frame(savedChoices[[a]] %>% ymd() %>% as.numeric())
      names(aux) <- a
    } else {
      aux <- data.frame(savedChoices[[a]])
      names(aux) <- a
    }
    assign(a, aux %>%
      left_join(dbGetQuery(con, glue::glue("select * from {a}Id"))) %>%
      pull(Id))
    if (tipo %in% c("slider", "dateRange")) {
      filtro <- glue::glue("{a} >= {get(a)[1]} and {a} <= {get(a)[2]}")
    } else {
      filtro <- glue::glue("{a} in ({toString(get(a))})")
    }
  }
}
