#' Run the Shiny Application
#'
#' @param config_file location of the config file to be used.
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  # onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  config_file = app_sys("golem-config.yml"),
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = app_global,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...,config_file = config_file)
  )
}
