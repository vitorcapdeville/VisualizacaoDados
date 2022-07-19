#' formata_dados
#'
#' @description Funcao para auxiliar na formatacao necessaria para o app. Recebe dois data.frames, ou data.tables,
#' uma conexao com o SQL e as definicoes iguais aquelas que vao para o app e cria as tabelas e indices necessarios.
#'
#' @param con uma conexao com o SQL criada por `DBI::dbConnect`.
#' @param tabela1,tabela2 data.frame ou data.tables com os dados.
#' @param nometabela1,nometabela2 nome das tabelas a serem criadas no SQL.
#' @param colunas_exploratorias nome das colunas a serem usadas como filtro e/ou como tabela.
#' @param colunas_valor1,colunas_valor2 colunas com os valores a serem exibidos nas tabelas, provinientes
#' de cada uma das tabelas (1 e 2)
#' @param overwrite T ou F, ira sobrescrever as tabelas existentes se T e ira retornar erro se ja existirem
#' tabelas com os nomes fornecidos se F.
#'
#' @return nao retorna nada, apenas cria as tabelas na conexao.
#'
#' @export
formata_dados <- function(con, tabela1, tabela2, nometabela1, nometabela2, colunas_exploratorias, colunas_valor1, colunas_valor2, overwrite = F) {
  base_full <- dplyr::bind_rows(tabela1, tabela2)

  stopifnot(all(colunas_exploratorias %in% names(tabela1)))
  stopifnot(all(colunas_exploratorias %in% names(tabela2)))
  stopifnot(all(colunas_valor1 %in% names(tabela1)))
  stopifnot(all(colunas_valor2 %in% names(tabela2)))

  tabela1 <- tabela1 %>% dplyr::select(dplyr::all_of(c(colunas_exploratorias, colunas_valor1)))
  tabela2 <- tabela2 %>% dplyr::select(dplyr::all_of(c(colunas_exploratorias, colunas_valor2)))

  date_cols1 <- grep("Date", unlist(sapply(tabela1, class)))
  date_cols2 <- grep("Date", unlist(sapply(tabela2, class)))

  if (length(date_cols1) > 0 || length(date_cols2) > 0) {
    stop("Por favor, converta as colunas do tipo Date em character.")
  }

  # Essas tabelas servem para extrair os valores default de todas as colunas de filtro.
  id_tables <- map(colunas_exploratorias, ~ get_id(base_full, .x))
  names(id_tables) <- paste0(colunas_exploratorias, "Id")

  tabela_1 <- list(tabela1)
  names(tabela_1) <- nometabela1
  tabela_2 <- list(tabela2)
  names(tabela_2) <- nometabela2

  tabuas_pro_sql <- c(id_tables, tabela_1, tabela_2)

  purrr::walk2(.x = tabuas_pro_sql, .y = names(tabuas_pro_sql), ~ dbWriteTable(con, .y, .x, overwrite = overwrite))

  DBI::dbExecute(con, glue::glue("CREATE INDEX Idx_1 ON {nometabela1}({toString(unique(c(colunasExp, colunasValor1)))});"))
  DBI::dbExecute(con, glue::glue("CREATE INDEX Idx_2 ON {nometabela2}({toString(unique(c(colunasExp, colunasValor2)))});"))

  for (i in seq_len(length(colunas_exploratorias))) {
    DBI::dbExecute(con, glue::glue("CREATE INDEX Idx_1{i+1} ON {nometabela1}({toString(unique(c(colunasExp[i], colunasValor1)))});"))
    DBI::dbExecute(con, glue::glue("CREATE INDEX Idx_2{i+1} ON {nometabela2}({toString(unique(c(colunasExp[i], colunasValor2)))});"))
  }
}


get_id <- function(data, col) {
  dplyr::arrange(dplyr::distinct(data, .data[[col]]), .data[[col]])
}
