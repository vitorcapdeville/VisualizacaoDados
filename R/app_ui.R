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
        "Principal",
        tabName = "principal",
        icon = icon("users")
      )
    )
  )

  body <- shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "principal",
        shinydashboard::box(
          status = "primary",
          solidHeader = T,
          title = strong("Filtros"),
          id = "filtros",
          collapsible = TRUE,
          collapsed = F,
          closable = F,
          width = 12,
          mod_saved_choices_ui("filtros", colunasFiltro, colunasFiltroNome, 3, estruturaPadrao, defaultValues, colunasFiltroTipo, 1)
        ),
        do.call(
          shinydashboard::tabBox,
          c(
            list(title = "", id = "tabset1", width = 12, height = "600px"),
            purrr::map2(
              colunasTabela, colunasTabelaNome,
              ~mod_create_table_ui(id = .x, nome = .y, value1 = colunasValorTabela1, value2 = colunasValorTabela2, name1 = colunasValorNomeTabela1, name2 = colunasValorNomeTabela2)
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
