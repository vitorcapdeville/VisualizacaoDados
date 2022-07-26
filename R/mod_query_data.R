#' query_data Server Functions
#'
#' @noRd
mod_query_data_server <- function(id, con, group, value1, name1, table1, value2, name2, table2, filtro, colunas_transformadas, colunas_transformadas_nome, cuts = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Tras os dados do SQL, com as colunas iniciais
    preTable <- reactive({
      # Isso evita rodar esse reactive para as tabs q nao estao abertas no momento
      tabela = query_padrao(
        con = con,
        group = group,
        value1 = value1,
        name1 = name1,
        table1 = table1,
        value2 = value2,
        name2 = name2,
        table2 = table2,
        filtro = filtro()$filtro,
        colunas_transformadas = colunas_transformadas,
        colunas_transformadas_nome = colunas_transformadas_nome,
        cuts = if(!is.null(cuts)) cuts()$cuts else cuts
      )
      list(tabela = tabela)
    })
    return(preTable)
  })
}

## To be copied in the UI
# mod_query_data_ui("query_data_1")

## To be copied in the server
# mod_query_data_server("query_data_1")
