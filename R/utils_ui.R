#' Estrutura padrao
#'
#' @description Essa funcao cria uma lista com os inputs.
#'
#' @param id identificador do input
#' @param label label a ser exibido no input.
#' @param tipo tipo de input.
#' @param choices valores possiveis do input.
#' @param step step do slider input. See `?shiny::sliderInput`
#'
#' @return lista contendo os inputs
#'
#' @noRd
cria_filtros <- function(id, label, tipo, choices, step = 1) {
  purrr::pmap(
    list(id, label, tipo, choices, step),
    ~genericInput(id = ..1, label = ..2, tipo = ..3, choices = ..4, step = ..5)
  )
}

#' filtrosEstrutura
#'
#' Funcao que monta a estrutura de colunas a partir dos filtros criados.
#'
#' @param input_list lista de inputs a serem posicionados, usualmente criado com cria_filtros.
#' @param ncols numero de colunas a ser utilizado em cada linha. Deve ser um numero que divide 12. (1, 2, 3, 4, 6 ou 12).
#'
filtrosEstrutura <- function(ncols, input_list) {
  if (12 %% ncols != 0) stop("O resto da divis\u00E3o entre 12 e ncols deve ser zero.")
  width <- 12 / ncols
  # cria todos os filtros, um em cada posicao da lista. Cada filtro eh uma coluna.
  colunas <- purrr::map(input_list, ~column(width = width, .x))
  # Separa a lista com as colunas em listas cada uma com tamanho ncols. Essas serao passadas
  # como argumento para o fluidRow e criarao as linhas
  cols_splited <- split(colunas, rep(seq(from = 1, to = ceiling(length(colunas)/ncols)), each = ncols)[seq(length(colunas))])
  linhas = purrr::map(cols_splited, ~do.call(what = fluidRow, args = .x))
  # Junta as linhas, uma embaixo da outra.
  estrutura = tagList(linhas)
  return(estrutura)
}

genericInput <- function(id, label, tipo, choices, step = NULL) {
  stopifnot(tipo %in% c("picker", "slider", "checkBox", "dateRange"))

  if (tipo == "picker") {
    res <- tags$div(
      align = "center",
      shinyWidgets::pickerInput(
        inputId = id,
        label = label,
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
  } else if (tipo == "slider") {
    choices <- c(min(choices), max(choices))
    res <- tags$div(
      align = "center",
      sliderInput(
        inputId = id,
        label = label,
        min = choices[1], max = choices[2],
        value = choices, step = step,
        width = "90%"
      )
    )
  } else if (tipo == "checkBox") {
    res <- tags$div(
      align = "center",
      shinyWidgets::checkboxGroupButtons(
        inputId = id,
        label = label,
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
  } else if (tipo == "dateRange") {
    choices <- c(
      min(choices %>% as.numeric() %>% as.Date("1970-01-01")),
      max(choices %>% as.numeric() %>% as.Date("1970-01-01"))
    )
    res <- tags$div(
      align = "center",
      dateRangeMonthsInput(
        inputId = id,
        label = label,
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
