app_global <- function() {
  configs <<- get_golem_config(file = golem::get_golem_options("config_file"))
  purrr::walk2(configs, names(configs), ~assign(x = .y, value = .x, envir = globalenv()))
  defaultValues <<- get_default_values(con = con, colunas_filtro = colunasFiltro, colunas_filtro_tipo = colunasFiltroTipo)
}
