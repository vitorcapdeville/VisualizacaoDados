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

get_default_values <- function(con, colunasFiltro, colunasFiltroTipo){
  defaultValues <- list()
  for (i in colunasFiltro) {
    if (!DBI::dbExistsTable(con, glue::glue("{i}Id"))) stop("Tabela ", i, "Id nao encontrada.")
    defaultValues[[i]] <- DBI::dbGetQuery(con, glue::glue("select {i} from {i}Id")) %>% dplyr::pull()
  }

  whichSlider <- which(colunasFiltroTipo == "slider")
  whichDateRange <- which(colunasFiltroTipo == "dateRange")

  for (i in c(whichSlider)) {
    defaultValues[[i]] <- c(
      min(defaultValues[[i]], na.rm = T),
      max(defaultValues[[i]], na.rm = T)
    )
  }
  for (i in whichDateRange) {
    defaultValues[[i]] <- c(
      min(defaultValues[[i]] %>% as.numeric() %>% as.Date("1970-01-01")),
      max(defaultValues[[i]] %>% as.numeric() %>% as.Date("1970-01-01"))
    )
  }
  return(defaultValues)
}
