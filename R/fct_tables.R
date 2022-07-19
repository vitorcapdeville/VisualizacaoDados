#' Funcoes auxiliares para montar a query.
#'#' Funcoe#' Funcoes auxiliares para montar a query.
#'
#' @param col nome da coluna a ser sumarizada (sum(col))
#' @param name nome da coluna final (as name)
#' @param group nome da coluna de agrupamento.
#'
#' @return string com a construcao para auxiliar na query.
#' @noRd
sum_as <- function(col, name, con) {
  glue::glue_sql_collapse(glue::glue_sql("sum({`col`}) as {`name`}", .con = con), sep = ", ")
}
join_on <- function(group, con) {
  glue::glue_sql_collapse(glue::glue_sql("t1.{`group`} = t2.{`group`}", .con = con), sep = " and ")
}

#' Busca os dados em uma conexao e realiza a sumarizacao adequada.
#'
#' Essa funcao é a base para as tabelas padrão. Realiza uma query que filtra, sumariza e junta
#' os dados da tabela1 e tabela2. Idealmente, a conexao deve possuir indices criados via "create index"
#' para agilizar a query.
#'
#' @param con uma conexao criada com `DBI::dbConnect`.
#' @param group um vetor com uma ou mais strings, definindo o nome das colunas que serao usada na
#' clausula de `group by`.
#' @param value1,value2 vetores com uma ou mais strings, definindo quais colunas serão sumarizadas.
#' value1 se refere as colunas que pertencem a tabela1 e value2 se refere as colunas que pertencem a
#' tabela2. A sumarização realizada é sempre a soma.
#' @param name1,name2 vetores com uma ou mais strings, definindo o nome a ser usado para as colunas criadas
#' com a sumarizacao de value1 e value2. Deve ter o mesmo tanho que value1 e value2.
#' @param table1,table2 nome das tabelas.
#' @param filtro uma string contendo a clausula de where completa (incluindo o termo where). Essa string
#' usualmente é criada com a funcao `criacao_filtro`.
#'
#' @return um data.frame com as colunas definidas em group, value1 e value2.
#' @noRd
query_padrao <- function(con, group, value1, value2, name1, name2, table1, table2, filtro) {
  stopifnot(length(filtro) == 1)
  stopifnot(length(value1) == length(name1))
  stopifnot(length(value2) == length(name2))

  sumarizacao1 <- sum_as(value1, name1, con)
  sumarizacao2 <- sum_as(value2, name2, con)

  chaves_join <- join_on(group, con)

  group1 <- purrr::map(group, ~ DBI::Id(table = table1, column = .x))
  group2 <- purrr::map(group, ~ DBI::Id(table = table2, column = .x))
  if (length(group1) == 1) group1 <- group1[[1]]
  if (length(group2) == 1) group2 <- group2[[1]]

  subquery1 <- glue::glue_sql(
    "select {`group1`*},", sumarizacao1, "\n",
    "from {`table1`}\n",
    filtro, "\n",
    "group by {`group1`*}",
    .con = con
  )
  subquery2 <- glue::glue_sql(
    "select {`group2`*},", sumarizacao2, "\n",
    "from {`table2`}\n",
    filtro, "\n",
    "group by {`group2`*}",
    .con = con
  )

  cols <- c(
    purrr::map(group, ~ DBI::Id(table = "t1", column = .x)),
    purrr::map(name1, ~ DBI::Id(table = "t1", column = .x)),
    purrr::map(name2, ~ DBI::Id(table = "t2", column = .x))
  )
  data <- DBI::dbGetQuery(
    con,
    glue::glue_sql(
      "select {`cols`*}
      from ({subquery1}) t1
      join ({subquery2}) t2 on {chaves_join}
      ",
      .con = con
    )
  )

  return(data)
}

#' Cria objeto datatable
#'
#'
#' @param data data.frame com os resultados da query
#' @param fixed quantidade de colunas a esquerda que serao fixadas. So deve ser maior que
#' 1 se tiverem mais de uma coluna de agrupamento. Nao pode ser menor que 1.
#' @param cols nome das colunas de valor que constituirao o datatable
#' @param formats formato das colunas de valor que irao compor o datatable
#' @param widths widths das primeiras colunas
#' @param align posicao para alinhar o datatable
#' @param footer T ou F
#'
#' @noRd
createDT <- function(data, fixed = 1, cols, formats, widths = c("400px", "200px", "200px"),
                     align = "left", footer = T, pageLength = 10) {
  . <- NULL # Avoid R CMD Check notes
  stopifnot(fixed >= 1)
  data <- as.data.frame(data)

  tipo <- names(data)
  tipo[which(names(data) %in% cols[formats == "number"])] <- "number"
  tipo[which(names(data) %in% cols[formats == "perc"])] <- "perc"
  tipo[which(!names(data) %in% cols)] <- "-"
  tipo[1] <- "Total"
  DT::datatable(data,
    container = sketch(data, tipo, align = "left"),
    rownames = F,
    extensions = c("FixedColumns"),
    height = 600,
    options = list(
      "autoWidth" = TRUE,
      "pageLength" = pageLength,
      # "scrollY" = "400px",
      # "scrollCollapse" = F,
      "scrollX" = TRUE,
      "searching" = FALSE,
      "dom" = "tp",
      "fixedColumns" = list(leftColumns = fixed),
      "columnDefs" = list(
        list(width = widths[3], visible = T, targets = seq(2, ncol(data) - 1)),
        list(width = widths[2], visible = T, targets = c(1)),
        list(width = widths[1], visible = T, targets = c(0)),
        list(className = "dt-center", targets = "_all"),
        list(targets = "_all", visible = FALSE)
      ),
      "drawCallback" = DT::JS(glue::glue("function() {
          var api = this.api();
          var rowCount = api.rows({page: 'current'}).count();

          for (var i = 0; i < api.page.len() - (rowCount === 0? 1 : rowCount); i++) {
            $('#' + api.table().node().id + ' tbody').append($('<tr ><td>&nbsp;</td>[paste0(rep('<td></td>', ncol(data) - 1), collapse = '')]</tr>'));
          }
        }", .open = "[", .close = "]"))
    )
  ) %>%
    {
      if (length(formats[formats == "number"]) > 0) DT::formatCurrency(., cols[formats == "number"], "", mark = ".", dec.mark = ",") else .
    } %>%
    {
      if (length(formats[formats == "perc"]) > 0) DT::formatPercentage(., cols[formats == "perc"], digits = 2, mark = ".", dec.mark = ",") else .
    }
}

formatReal <- function(x) {
  paste("", format(x, big.mark = ".", decimal.mark = ",", nsmall = 2, scientific = F), sep = "")
}

#' Funcao para calcular o footer.
#'
#' @param x um vetor
#' @param tipo se number, retorna a soma de x. se perc, retorna -. Caso contrario retorna tipo.
#'
#' @noRd
calcula_footer <- function(x, tipo) {
  if (tipo == "number") {
    ret <- formatReal(sum(x))
  } else if (tipo == "perc") {
    ret <- "-"
  } else {
    ret <- tipo
  }
  return(ret)
}

sketch <- function(dados, tipos, align = "left") {
  htmltools::withTags(div(align = align, table(
    DT::tableHeader(dados),
    DT::tableFooter(purrr::map2(dados, tipos, ~ calcula_footer(x = .x, tipo = .y)))
  )))
}
