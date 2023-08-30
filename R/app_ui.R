#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  header <- shinydashboardPlus::dashboardHeader(
    title = configs$title_header,
    controlbarIcon = tagList(tag("font", list(strong("Filtros"), "size" = "+1")), shiny::icon("filter"))
  )

  sidebar <- shinydashboardPlus::dashboardSidebar(
    collapsed = TRUE,
    minified = FALSE,
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
        icon = icon("table")
      ),
      shinydashboard::menuItem(
        "Gr\u00E1ficos",
        tabName = "graficos_detalhe",
        icon = icon("chart-line")
      ),
      shinydashboard::menuItem(
        "Personalizado",
        tabName = "custom",
        icon = icon("plus")
      )
    )
  )

  body <- shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "principal",
        shinydashboardPlus::box(
          width = 12,
          title = plot_home_1_nome_grafico,
          closable = F,
          status = NULL,
          solidHeader = FALSE,
          collapsible = F,
          sidebar = NULL,
          mod_barplot_ui(id = "plot_home_1", nome = plot_home_1_nome_grafico)
        )
      ),
      shinydashboard::tabItem(
        tabName = "tabelas_detalhe",
        purrr::map(
          colunasTabela,
          ~mod_create_table_ui(id = .x)
        )
      ),
      shinydashboard::tabItem(
        tabName = "graficos_detalhe",
        do.call(
          shinydashboard::tabBox,
          c(
            list(title = "", id = "tabset1_graph", width = 12),
            purrr::pmap(
              list(glue::glue("{colunas_x}x{colunas_y}"), nome_grafico),
              ~mod_barplot_ui(id = ..1, nome = ..2)
            )
          )
        )
      ),
      shinydashboard::tabItem(
        tabName = "custom",
        shinyWidgets::pickerInput("group_by", "Agrupamento", choices = colunas_all_group, selected = colunas_all_group[1], multiple = T),
        mod_create_table_ui(id = "custom_table")
      )

    )
  )

  controlbar <- shinydashboardPlus::dashboardControlbar(
    id = "controlbar",
    width = 330,
    shinydashboardPlus::controlbarMenu(
      id = "controlbarmenu",
      shinydashboardPlus::controlbarItem(
        "Filtros",
        mod_saved_choices_ui(id = "filtros", id_inputs = colunasFiltro, label = colunasFiltroNome, ncols = 1, choices = defaultValues, tipo = colunasFiltroTipo, step = 1, input_list = NULL)
      )
    )
  )


  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinydashboardPlus::dashboardPage(
      header = header,
      sidebar = sidebar,
      body = body,
      controlbar  = controlbar
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
    path_www
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = path_www,
      app_title = configs$nome_pagina
    ),
    shinyjs::useShinyjs(),
    fresh::use_theme(configs$fresh_theme)
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
