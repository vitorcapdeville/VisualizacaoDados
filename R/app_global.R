app_global <- function() {
  con <<- DBI::dbConnect(RSQLite::SQLite(), glue::glue("{app_sys()}/app/sinistralidade-sicoob.sqlite"))

  colunasExp <<- c("Idade", "NomeProduto", "Cobertura", "TipoProduto", "RamoCodigo")
  colunasExpNome <<- c("Idade", "Produto", "Cobertura", "Tipo de produto", "Ramo c\u00F3digo")
  colunasExpTipo <<- c("slider", "picker", "picker", "picker", "picker")
  colunasValorTabela1 <<- c("PremioEmitido", "PremioGanhoCorrigido", "ComissaoGanha")
  colunasValorTabela2 <<- c("SinistroComER")

  tabela1 <<- "premioVI"
  tabela2 <<- "sinistroVI"
  list(colunasExp, colunasExpNome, colunasExpTipo, colunasValorTabela1, colunasValorTabela2, tabela1, tabela2)
  defaultValues <<- list()
  for (i in colunasExp) {
    if (!DBI::dbExistsTable(con, glue::glue("{i}Id"))) stop("Tabela ", i, "Id nao encontrada.")
    defaultValues[[i]] <<- DBI::dbGetQuery(con, glue::glue("select {i} from {i}Id")) %>% dplyr::pull()
  }

  whichSlider <- which(colunasExpTipo == "slider")
  whichDateRange <- which(colunasExpTipo == "dateRange")

  for (i in c(whichSlider)) {
    defaultValues[[i]] <<- c(
      min(defaultValues[[i]], na.rm = T),
      max(defaultValues[[i]], na.rm = T)
    )
  }
  for (i in whichDateRange) {
    defaultValues[[i]] <<- c(
      min(defaultValues[[i]] %>% as.numeric() %>% as.Date("1970-01-01")),
      max(defaultValues[[i]] %>% as.numeric() %>% as.Date("1970-01-01"))
    )
  }
}
