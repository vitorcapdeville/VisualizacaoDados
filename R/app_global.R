app_global <- function() {
  con <<- get_golem_config("con")

  # Essas variaveis vao definir o comportamento do app. Serao definidas em golem-config.yml
  con <<- get_golem_config("con")
  colunasFiltro <<- get_golem_config("colunasFiltro")
  colunasFiltroTipo <<- get_golem_config("colunasFiltroTipo")
  colunasFiltroNome <<- get_golem_config("colunasFiltroNome")
  colunasTabela <<- get_golem_config("colunasTabela")
  colunasTabelaNome <<- get_golem_config("colunasTabelaNome")
  tabela1 <<- get_golem_config("tabela1")
  tabela2 <<- get_golem_config("tabela2")
  colunasValorTabela1 <<- get_golem_config("colunasValorTabela1")
  colunasValorNomeTabela1 <<- get_golem_config("colunasValorNomeTabela1")
  formatosValorTabela1 <<- get_golem_config("formatosValorTabela1")
  colunasValorTabela2 <<- get_golem_config("colunasValorTabela2")
  colunasValorNomeTabela2 <<- get_golem_config("colunasValorNomeTabela2")
  formatosValorTabela2 <<- get_golem_config("formatosValorTabela2")
  defaultValues <<- get_default_values(con = con, colunas_filtro = colunasFiltro, colunas_filtro_tipo = colunasFiltroTipo)


  defaultValues <<- get_default_values(con = con, colunas_filtro = colunasFiltro, colunas_filtro_tipo = colunasFiltroTipo)
}
