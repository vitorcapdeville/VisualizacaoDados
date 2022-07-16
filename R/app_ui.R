#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinydashboardPlus::dashboardPage(
      preloader = list(
        html = '<div align = "center">
                            <div class="overlay" align = "center">
                              <h3><b>Por favor, aguarde enquanto o aplicativo \u00E9 inicializado</b></h3>
                              <div class="boxxy">
                                <div class="spinner spinner--1"></div>
                              </div>
                            </div>
                          </div>',
        color = "#3c8dbc"
      ),
      header = shinydashboardPlus::dashboardHeader(fixed = T),
      sidebar = shinydashboardPlus::dashboardSidebar(
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
      ),
      body = shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem(
            tabName = "principal",
            shinydashboardPlus::box(
              status = "primary",
              solidHeader = T,
              title = strong("Filtros"),
              id = "filtros",
              collapsible = TRUE,
              collapsed = F,
              closable = F,
              width = 12,
              mod_saved_choices_ui("filtros", colunasExp, colunasExpNome, 3, estruturaPadrao, defaultValues, colunasExpTipo, 1)
            ),
            mod_create_table_ui("NomeProduto", "Produto")
          )
        )
      ),
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
