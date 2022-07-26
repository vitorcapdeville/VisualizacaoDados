#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  observeEvent(input$sidebarMenu,{
    #Ocultar icone da sidebar da direita
    shinyjs::toggleElement(selector = "body > div.wrapper > header > nav > div:nth-child(4) > ul", condition = !input$sidebarMenu %in% c("principal"))
  })

  #------------------------------Estrutura para salvar selecao--------------------------------------
  # Valor inicial do savedChoices
  savedChoices <- reactiveValues()


  for (i in colunasFiltro) {
    savedChoices[[i]] <- defaultValues[[i]]
  }
  mod_saved_choices_server("filtros", savedChoices, colunasFiltro, defaultValues, colunasFiltroTipo)

  #--------------------------------------------------Calculos da aba -------------------------------
  # Criação dos filtros necessário. Se não for necessário nenhum filtro, não filtrar.
  dados_filt <- reactive({
    filtro <- unlist(purrr::pmap(list(colunasFiltro, colunasFiltroTipo, rvtl(savedChoices)[colunasFiltro], defaultValues[colunasFiltro]), criacao_filtro, con))

    if (!is.null(filtro)) {
      filtro <- glue::glue_sql("where ", glue::glue_sql_collapse(filtro, sep = " and "), .con = con)
    } else {
      filtro <- glue::glue_sql("", .con = con)
    }
    list(filtro = filtro)
  })

  # Plot da primeira pagina
  mod_barplot_server(
    id = "plot_home_1", x = plot_home_1_colunas_x, y = plot_home_1_colunas_y, name = plot_home_1_colunas_name, tickx = plot_home_1_tickx, ticky = plot_home_1_ticky,
    titlex = plot_home_1_titlex, titley = plot_home_1_titley, barmode = plot_home_1_barmode, value1 = colunasValorTabela1, value2 = colunasValorTabela2,
    name1 = colunasValorNomeTabela1, name2 = colunasValorNomeTabela2,table1 = tabela1, table2 = tabela2,colunas_transformadas = colunasTransformadas,
    colunas_transformadas_nome = colunasTransformadasNome,filtro = reactive({list(filtro = "")})
  )
  # Fim plot da primeira pagina.

  # Tabelas
  purrr::pmap(
    list(colunasTabela, colunasTabela, dynamic_groups),
    ~mod_create_table_server(
      id = ..1, group = ..2, value1 = colunasValorTabela1, value2 = colunasValorTabela2,
      name1 = colunasValorNomeTabela1, name2 = colunasValorNomeTabela2,
      formats1 = formatosValorTabela1, formats2 = formatosValorTabela2,
      table1 = tabela1, table2 = tabela2,colunas_transformadas = colunasTransformadas,
      colunas_transformadas_nome = colunasTransformadasNome,formato_colunas_transformadas = formatosTransformadas,
      fixed = 1, widths = c("200px", "120px", "120px"), filtro = dados_filt, downloadable = T, dynamic_groups = ..3
    )
  )
  # Graficos
  purrr::pmap(
    list(glue::glue("{colunas_x}x{colunas_y}"), colunas_x, colunas_y, colunas_name, tickx, ticky, titlex, titley, barmode),
    ~mod_barplot_server(
      id = ..1, x = ..2, y = ..3, name = ..4, tickx = ..5, ticky = ..6,
      titlex = ..7, titley = ..8, barmode = ..9, value1 = colunasValorTabela1, value2 = colunasValorTabela2,
      name1 = colunasValorNomeTabela1, name2 = colunasValorNomeTabela2,table1 = tabela1, table2 = tabela2,colunas_transformadas = colunasTransformadas,
      colunas_transformadas_nome = colunasTransformadasNome,filtro = dados_filt
    )
  )

  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  } else {
    onStop(function() {
      DBI::dbDisconnect(con)
      stopApp()
    })
  }
}
