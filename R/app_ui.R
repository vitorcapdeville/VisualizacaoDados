#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  header <- shinydashboard::dashboardHeader(title = "app")

  sidebar <- shinydashboard::dashboardSidebar(
    minified = F,
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
        do.call(shinydashboard::tabBox, c(list(title = "Tabelas", id = "tabset1", width = 12),purrr::map2(colunasTabela, colunasTabelaNome, mod_create_table_ui)))
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
      body = body,
      title = "Visualiza\u00E7ao Dados"
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
      app_title = "VisualizacaoDados"
    ),
    shinyjs::useShinyjs()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
