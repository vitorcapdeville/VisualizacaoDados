#' Funcoes auxiliares para comparacao
#'
#' comparacao1 verifica se todos os valores padrao estao no input. Como
#' o input nao pode ter mais elementos q os valores padrao, essa comparacao
#' é equivalente a comparar se os dois sao iguais.
#' comparacao2 verifica se saved choices e input sao equivalentes, ou seja,
#' se houve alteracao no saved choices.
#'
#' @param colunas_filtro string com o nome da coluna da base cujo filtro esta sendo
#' comparado.
#'
#' @return T ou F
#'
#' @noRd
comparacao1 <- function(coluna_filtro, defaultValues, input) {
  return(!all(defaultValues[[coluna_filtro]] %in% input[[coluna_filtro]]))
}

comparacao2 <- function(coluna_filtro, input, savedChoices) {
  return(all(savedChoices[[coluna_filtro]] %in% input[[coluna_filtro]]) &
    all(input[[coluna_filtro]] %in% savedChoices[[coluna_filtro]]))
}

#' Funcao que define quais sao os valores padrao de cada filtro
#'
#' Serve para evitar de realizar o filtro quando nao houver nenhum filtro diferente do padrao selecionado
#' e para definir os valores dos inputs.
#'
#' @param con conexao com o sql
#' @param colunas_filtro vetor de string com o nome das colunas (todas) que serao utilizadas como filtro.
#' @param colunas_filtro_tipo vetor de string com o tipo de filtro de cada uma das colunas.
#' Pode ser picker, slider ou dateRange.
#'
#' @return uma lista com os valores default de cada filtro. picker exibe todos os valores unicos,
#' dateRange e slider exibem o maximo e o minimo.
#' @noRd
get_default_values <- function(con, colunas_filtro, colunas_filtro_tipo){
  default_values <- list()
  for (i in colunas_filtro) {
    if (!DBI::dbExistsTable(con, glue::glue("{i}Id"))) stop("Tabela ", i, "Id nao encontrada.")
    default_values[[i]] <- DBI::dbGetQuery(con, glue::glue("select {i} from {i}Id")) %>% dplyr::pull()
  }

  which_slider <- which(colunas_filtro_tipo == "slider")
  which_date_ranger <- which(colunas_filtro_tipo == "dateRange")

  for (i in c(which_slider)) {
    default_values[[i]] <- c(
      min(default_values[[i]], na.rm = T),
      max(default_values[[i]], na.rm = T)
    )
  }
  for (i in which_date_ranger) {
    default_values[[i]] <- c(
      min(default_values[[i]] %>% as.numeric() %>% as.Date("1970-01-01")),
      max(default_values[[i]] %>% as.numeric() %>% as.Date("1970-01-01"))
    )
  }
  return(default_values)
}
