#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  header <- shinydashboard::dashboardHeader(title = configs$title_header)

  sidebar <- shinydashboard::dashboardSidebar(
    collapsed = TRUE,
    shinydashboard::sidebarMenu(
      id = "sidebarMenu",
      shinydashboard::menuItem(
        "Resumo",
        tabName = "principal",
        icon = icon("home")
      ),
      shinydashboard::menuItem(
        "Tabelas",
        tabName = "tabelas_detalhe",
        icon = icon("users")
      ),
      shinydashboard::menuItem(
        "Gráficos",
        tabName = "graficos_detalhe",
        icon = icon("chart-line")
      )
    )
  )

  body <- shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "principal",
        mod_saved_choices_ui("filtros", colunasFiltro, colunasFiltroNome, 3, estruturaPadrao, defaultValues, colunasFiltroTipo, 1),
      ),
      shinydashboard::tabItem(
        tabName = "tabelas_detalhe",
        do.call(
          shinydashboard::tabBox,
          c(
            list(title = "", id = "tabset1", width = 12, height = "600px"),
            purrr::map2(
              colunasTabela, colunasTabelaNome,
              ~mod_create_table_ui(id = .x, nome = .y)
            )
          )
        )
      ),
      shinydashboard::tabItem(
        tabName = "graficos_detalhe",
        do.call(
          shinydashboard::tabBox,
          c(
            list(title = "", id = "tabset1_graph", width = 12, height = "600px"),
            purrr::pmap(
              list(glue::glue("{colunas_x}x{colunas_y}"), nome_grafico),
              ~mod_barplot_ui(id = ..1, nome = ..2)
            )
          )
        )
      )
    )
  )


  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinydashboard::dashboardPage(
      header = header,
      sidebar = sidebar,
      body = body
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = configs$nome_pagina
    ),
    shinyjs::useShinyjs(),
    fresh::use_theme(configs$fresh_theme)
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
