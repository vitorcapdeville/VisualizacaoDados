#' Estrutura padrao
#'
#' @description Essa funcao define a estrutura padrao do filtro. Atualmente sao suportados
#' filtros do tipo picker, checkBox, slider e dateRange. Os filtros sao criados dentro de colunas,
#' que serao encaixadas dentro de linhas
#'
#' @param id identificador do input
#' @param colunas_filtro_nome nome das colunas, a serem exibidas acima do filtro.
#' @param colunas_filtro_tipo tipo de filtro a ser criado.
#' @param default_values uma lista com todos os valores possiveis daquele filtro.
#' @param default_values uma lista com todos os valores possiveis daquele filtro.
#' @param width largura da coluna. See `?shiny::column` for more details.
#' @param step step do slider input. See `?shiny::sliderInput`
#' @param ncols numero de colunas a ser utilizado em cada linha. Deve ser um numero que divide 12. (1, 2, 3, 4, 6 ou 12).
#' @param estruturaPadrao funcao que define a estrutura padrao. Será incluido no fluidRow, logo, deve ser uma
#' funcao que retorna um objeto html com a classe col, como `shiny::column()`
#'
#' @noRd
estruturaPadrao <- function(id, colunas_filtro_nome, colunas_filtro_tipo, default_values, width, step = 1) {
  column(width, genericInput(id, colunas_filtro_nome, colunas_filtro_tipo, default_values, step))
}

filtrosEstrutura <- function(id, colunas_filtro_nome, ncols, estruturaPadrao, default_values, colunas_filtro_tipo, step = 1) {
  if (12 %% ncols != 0) stop("O resto da divis\u00E3o entre 12 e ncols deve ser zero.")
  width <- 12 / ncols
  # cria todos os filtros, um em cada posicao da lista. Cada filtro eh uma coluna.
  colunas <- purrr::pmap(
    list(id, colunas_filtro_nome, colunas_filtro_tipo),
    ~estruturaPadrao(id = ..1, colunas_filtro_nome = ..2, colunas_filtro_tipo = ..3, default_values = default_values, width = width, step = 1)
  )
  # Separa a lista com as colunas em listas cada uma com tamanho ncols. Essas serao passadas
  # como argumento para o fluidRow e criarao as linhas
  cols_splited <- split(colunas, rep(seq(from = 1, to = ceiling(length(colunas)/ncols)), each = ncols)[seq(length(colunas))])
  linhas = purrr::map(cols_splited, ~do.call(what = fluidRow, args = .x))
  # Junta as linhas, uma embaixo da outra.
  estrutura = tagList(linhas)
  return(estrutura)
}

genericInput <- function(id, colunas_filtro_nome, colunas_filtro_tipo, default_values, step) {
  stopifnot(colunas_filtro_tipo %in% c("picker", "slider", "checkBox", "dateRange"))
  id2 <- gsub(x = id,pattern =  ".*-", replacement = "")
  choices <- default_values[[id2]]

  if (colunas_filtro_tipo == "picker") {
    res <- tags$div(
      align = "center",
      shinyWidgets::pickerInput(
        inputId = id,
        label = colunas_filtro_nome,
        choices = choices,
        selected = choices,
        inline = F,
        options = list(
          `selected-text-format` = "count > 0",
          `live-search` = TRUE,
          `actions-box` = TRUE,
          dropupAuto = F,
          size = min(8, length(choices))
        ),
        multiple = TRUE,
        width = "90%"
      )
    )
  } else if (colunas_filtro_tipo == "slider") {
    choices <- c(min(choices), max(choices))
    res <- tags$div(
      align = "center",
      sliderInput(
        inputId = id,
        label = colunas_filtro_nome,
        min = choices[1], max = choices[2],
        value = choices, step = step,
        width = "90%"
      )
    )
  } else if (colunas_filtro_tipo == "checkBox") {
    res <- tags$div(
      align = "center",
      shinyWidgets::checkboxGroupButtons(
        inputId = id,
        label = colunas_filtro_nome,
        choices = choices,
        selected = choices,
        individual = TRUE,
        checkIcon = list(
          yes = icon("check-square", lib = "font-awesome"),
          no = icon("square", lib = "font-awesome")
        ),
        size = "xs",
        justified = F
      )
    )
  } else if (colunas_filtro_tipo == "dateRange") {
    choices <- c(
      min(choices %>% as.numeric() %>% as.Date("1970-01-01")),
      max(choices %>% as.numeric() %>% as.Date("1970-01-01"))
    )
    res <- tags$div(
      align = "center",
      dateRangeMonthsInput(
        inputId = id,
        label = colunas_filtro_nome,
        format = "MM/yy",
        min = choices[1],
        max = choices[2],
        start = choices[1],
        end = choices[2],
        language = "pt-BR",
        separator = " at\u00E9 ",
        startview = "year"
      )
    )
  }
}
