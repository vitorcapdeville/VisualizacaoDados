app_global <- function() {
  configs <<- get_golem_config(file = golem::get_golem_options("config_file"))
  con <<- configs$con

  # Essas variaveis vao definir o comportamento do app. Serao definidas em golem-config.yml
  colunasFiltro <<- configs$colunasFiltro
  colunasFiltroTipo <<- configs$colunasFiltroTipo
  colunasFiltroNome <<- configs$colunasFiltroNome
  colunasTabela <<- configs$colunasTabela
  colunasTabelaNome <<- configs$colunasTabelaNome
  tabela1 <<- configs$tabela1
  tabela2 <<- configs$tabela2
  colunasValorTabela1 <<- configs$colunasValorTabela1
  colunasValorNomeTabela1 <<- configs$colunasValorNomeTabela1
  formatosValorTabela1 <<- configs$formatosValorTabela1
  colunasValorTabela2 <<- configs$colunasValorTabela2
  colunasValorNomeTabela2 <<- configs$colunasValorNomeTabela2
  formatosValorTabela2 <<- configs$formatosValorTabela2
  defaultValues <<- get_default_values(con = con, colunas_filtro = colunasFiltro, colunas_filtro_tipo = colunasFiltroTipo)


  defaultValues <<- get_default_values(con = con, colunas_filtro = colunasFiltro, colunas_filtro_tipo = colunasFiltroTipo)
}
