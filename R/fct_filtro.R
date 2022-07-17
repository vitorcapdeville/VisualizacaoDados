#' filtro
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
criacao_filtro <- function(a, tipo, savedChoices, defaultValues, con) {
  Id <- NULL #Avoid R CMD CHECK notes
  if (!identical(savedChoices[[a]], defaultValues[[a]])) {
    if (tipo == "dateRange") {
      aux <- data.frame(savedChoices[[a]] %>% lubridate::ymd() %>% as.numeric())
      names(aux) <- a
    } else {
      aux <- data.frame(savedChoices[[a]])
      names(aux) <- a
    }
    assign(
      a,
      aux %>%
        dplyr::left_join(DBI::dbGetQuery(con, glue::glue("select * from {a}Id"))) %>%
        dplyr::pull(Id)
    )
    if (tipo %in% c("slider", "dateRange")) {
      filtro <- glue::glue("{a} >= {get(a)[1]} and {a} <= {get(a)[2]}")
    } else {
      filtro <- glue::glue("{a} in ({toString(get(a))})")
    }
  }
}
