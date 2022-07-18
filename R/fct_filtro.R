#' Criacao dos filtros para as queries.
#'
#' Verifica se os valores padrão foram modificados e cria os filtros necessários. Se todos os valores
#' padrão estão selecionados, isto é, se nenhum filtro foi realizado, cria uma string vazia para
#' evitar filtro desnecessário.
#'
#' @param coluna_filtro uma unica string com a coluna sobre o qual o filtro esta sendo aplicado (ou nao)
#' @param coluna_filtro_tipo uma unica string dizendo qual é o tipo de filtro desta coluna.
#' @param saved_choice escolhas atualmente salvas para essa coluna
#' @param default_value valores padrao para esta coluna.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
criacao_filtro <- function(coluna_filtro, coluna_filtro_tipo, saved_choice, default_value, con) {
  if (!identical(sort(saved_choice), sort(default_value))) {
    if (coluna_filtro_tipo == "dateRange") {
      aux <- saved_choice %>% lubridate::ymd()
    } else {
      aux <- saved_choice
    }
    if (coluna_filtro_tipo %in% c("slider", "dateRange")) {
      filtro <- glue::glue_sql("{`coluna_filtro`} >= {min_val} and {`coluna_filtro`} <= {max_val}", min_val = aux[1], max_val = aux[2], .con = con)
    } else {
      filtro <- glue::glue_sql("{`coluna_filtro`} in ({escolhas*})", escolhas = aux, .con = con)
    }
  }
}
