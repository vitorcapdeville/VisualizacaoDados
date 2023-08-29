# MOVER PARA O PACOTE QNDO TERMINAR DE AJEITAR

#' formata_dados
#'
#' @description Funcao para auxiliar na formatacao necessaria para o app. Garante que as colunas
#' exploratórias existem nas duas tabelas e cria tabelas com as combinações únicas dessas colunas
#' para auxiliar nos inputs. Após isso, cria indexes em todas as tabelas para acelerar as queries.
#'
#' Importante: as tabelas com nome {coluna_exploratoria}Id serão criadas no banco de dados, e se ja
#' existirem, serão dropadas.
#'
#' @param con uma conexao com o SQL criada por `DBI::dbConnect`.
#' @param nometabela1,nometabela2 nome das tabelas onde estão os dados.
#' @param colunas_exploratorias nome das colunas a serem usadas como filtro e/ou como tabela.
#' @param colunas_valor1,colunas_valor2 colunas com os valores a serem exibidos nas tabelas, provinientes
#' de cada uma das tabelas (1 e 2)
#'
#' @return nao retorna nada, apenas cria as tabelas na conexao.
#'
#' @export
formata_dados <- function(con, nometabela1, nometabela2, colunas_exploratorias, colunas_valor1, colunas_valor2) {

  stopifnot(all(tolower(colunas_exploratorias) %in% tolower(DBI::dbListFields(con, nometabela1))))
  stopifnot(all(tolower(colunas_exploratorias) %in% tolower(DBI::dbListFields(con, nometabela2))))

  stopifnot(all(tolower(colunas_valor1) %in% tolower(DBI::dbListFields(con, nometabela1))))
  stopifnot(all(tolower(colunas_valor2) %in% tolower(DBI::dbListFields(con, nometabela2))))

  DBI::dbExecute(con, glue::glue("IF IndexProperty(Object_Id('{nometabela1}'), 'Idx_1', 'IndexID') is not NULL drop INDEX {nometabela1}.Idx_1"))
  DBI::dbExecute(con, glue::glue("IF IndexProperty(Object_Id('{nometabela2}'), 'Idx_2', 'IndexID') is not NULL drop INDEX {nometabela2}.Idx_2"))
  DBI::dbExecute(con, glue::glue("CREATE INDEX Idx_1 ON {nometabela1}({toString(unique(c(colunas_exploratorias, colunas_valor1)))});"))
  DBI::dbExecute(con, glue::glue("CREATE INDEX Idx_2 ON {nometabela2}({toString(unique(c(colunas_exploratorias, colunas_valor2)))});"))

  # Essas tabelas servem para extrair os valores default de todas as colunas de filtro.
  id_tables <- purrr::walk(colunas_exploratorias, ~ criar_tabelas_id(nometabela1, nometabela2, .x, con))


  for (i in seq_len(length(colunas_exploratorias))) {
    DBI::dbExecute(con, glue::glue("IF IndexProperty(Object_Id('{nometabela1}'), 'Idx_1{i+1}', 'IndexID') is not NULL drop INDEX {nometabela1}.Idx_1{i+1}"))
    DBI::dbExecute(con, glue::glue("IF IndexProperty(Object_Id('{nometabela2}'), 'Idx_2{i+1}', 'IndexID') is not NULL drop INDEX {nometabela2}.Idx_2{i+1}"))
    DBI::dbExecute(con, glue::glue("CREATE INDEX Idx_1{i+1} ON {nometabela1}({toString(unique(c(colunas_exploratorias[i], colunas_valor1)))});"))
    DBI::dbExecute(con, glue::glue("CREATE INDEX Idx_2{i+1} ON {nometabela2}({toString(unique(c(colunas_exploratorias[i], colunas_valor2)))});"))
  }
}


criar_tabelas_id <- function(nometabela1, nometabela2, col, con) {

  nome_tabela_id = glue::glue("{col}Id")
  query_delete = glue::glue_sql(
    "
    IF OBJECT_ID('dbo.{`nome_tabela_id`}') IS NOT NULL
    DROP TABLE dbo.{`nome_tabela_id`};
    ",
    .con = con
  )

  query_create = glue::glue_sql(
    "
    select
      {`col`} into dbo.{`nome_tabela_id`}
    from (
      select {`col`} from {`nometabela1`}
      union
      select {`col`} from {`nometabela2`}
    ) t1
    ",
    .con = con
  )
  DBI::dbExecute(con, query_delete, immediate = TRUE)
  DBI::dbExecute(con, query_create, immediate = TRUE)
}
