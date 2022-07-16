#' ui
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
estruturaPadrao <- function(id, nome, tipo, defaultValues, colWid, step = 1) {
  column(colWid, genericInput(id, nome, tipo, defaultValues, step))
}

filtrosEstrutura <- function(id, nome, ncols, estruturaPadrao, defaultValues, tipo, step = 1) {
  if (12 %% ncols != 0) stop("O resto da divisão entre 12 e ncols deve ser zero.")
  c <- 1
  estrutura <- tagList()
  linha <- fluidRow()
  colWid <- 12 / ncols
  colunas <- pmap(list(id, nome, tipo), estruturaPadrao, defaultValues, colWid, step = 1)
  for (i in 1:length(colunas)) {
    # Varre o total de colunas, e se tiver dentro do limite por linhas, adiciona na linha
    if (c <= ncols) {
      linha <- tagAppendChild(linha, colunas[[i]])
      c <- c + 1
      # Se estourar o limite, passa pra uma nova linha e recomeça a contagem
    } else {
      estrutura <- tagAppendChild(estrutura, linha)
      # Se eu nao adicionar a coluna aqui, eu perco a iteração i
      linha <- fluidRow(colunas[[i]])
      c <- 1
    }
  }
  # Se terminar dentr do limite, então a última linha não foi adicionada
  if (c <= ncols) estrutura <- tagAppendChild(estrutura, linha)
  return(estrutura)
}

genericInput <- function(id, label, tipo, defaultValues, step) {
  stopifnot(tipo %in% c("picker", "slider", "checkBox", "dateRange"))
  id2 <- substr(id, str_locate(id, "-")[1, 1] + 1, str_length(id))
  choices <- defaultValues[[id2]]

  if (tipo == "picker") {
    res <- tags$div(
      align = "center",
      pickerInput(
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
        width = "80%"
      )
    )
  } else if (tipo == "checkBox") {
    res <- tags$div(
      align = "center",
      checkboxGroupButtons(
        inputId = id,
        label = label,
        choices = choices,
        selected = choices,
        individual = TRUE,
        checkIcon = list(
          yes = icon("check-square", lib = "font-awesome"),
          no = icon("square-o", lib = "font-awesome")
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
        separator = " até ",
        startview = "year"
      )
    )
  }
}
