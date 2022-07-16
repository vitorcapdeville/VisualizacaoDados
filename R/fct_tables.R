formatReal <- function(x) {
  paste("", format(sum(x), big.mark = ".", decimal.mark = ",", nsmall = 2, scientific = F), sep = "")
}

# Se der tempo, tentar incluir o total da página. ou
# colocar todos os estipulantes na página e incluir barra de rolagem.
sketch <- function(dados, cols, align = "left") {
  htmltools::withTags(div(align = align, table(
    DT::tableHeader(dados),
    DT::tableFooter(sapply(dados, function(x) {
      ifelse((is.numeric(x)),
        formatReal(sum(x, na.rm = T)),
        "Total"
      )
    }))
  )))
}

getSQLTable <- function(group, value1, value2, table1, table2, filtro) {
  aux11 <- aux12 <- aux2 <- aux3 <- c()
  for (i in value1) {
    aux11 <- c(aux11, glue::glue("sum({i}) as {i}"))
  }
  for (i in value2) {
    aux12 <- c(aux12, glue::glue("sum({i}) as {i}"))
  }
  for (i in group) {
    aux2 <- c(aux2, glue::glue("t1.{i} = t2.{i}"))
    aux3 <- c(aux3, glue::glue("t1.{i}"))
  }
  aux2 <- paste0(aux2, collapse = " and ")
  data <- dbGetQuery(
    con,
    glue::glue(
      "select {toString(aux3)}, {toString(value1)}, {toString(value2)}
      from (
        select {toString(group)}, {toString(aux11)}
        from {table1}
        {filtro}
        group by {toString(group)}
      ) t1
      join (
        select {toString(group)}, {toString(aux12)}
        from {table2}
        {filtro}
        group by {toString(group)}
      ) t2 on {aux2}
      "
    )
  ) %>% as.data.table()

  for (i in group) {
    data <- data %>%
      left_join(dbGetQuery(con, glue::glue("select * from {i}Id")), by = c(setNames("Id", i))) %>%
      select(!glue::glue("{i}")) %>%
      rename(!!i := glue::glue("{i}.y")) %>%
      select(all_of(group), all_of(c(value1, value2)))
  }

  return(data)
}

createDT <- function(data, colsFixed, fixed = 1,
                     widths = c("400px", "200px", "200px"),
                     align = "left",
                     pageLength = 10, scrollY = T, footer = T) {
  data <- as.data.frame(data)

  if (nrow(data) > pageLength) {
    dom <- "tp"
  } else {
    dom <- "t"
  }
  soma1 <- which(names(data) %in% (data %>% select(where(is.numeric) & !ends_with("Percentual")) %>% names())) - 1
  # soma2 = which(startsWith(names(data),"Quantidade")) - 1
  # traco = which(names(data) %in% (data %>% select((!where(is.numeric) & !all_of(colsFixed)) | ends_with("Percentual")) %>% names())) - 1
  total <- which(names(data) %in% colsFixed) - 1

  if (footer) {
    javascript <- DT::JS(
      js_op_aux("start"),
      js_op(total, operation = "custom", txt = "Total"),
      js_op(soma1, operation = "sum", txt = ""),
      # js_op(traco, operation = "custom", txt = "-"),
      # js_op(soma2, operation = "sum", txt = ""),
      js_op_aux("end")
    )
  } else {
    javascript <- NA
  }


  DT::datatable(data,
    container = js_op_aux("sketch", data, align = align),
    rownames = F,
    extensions = c("FixedColumns", "ColReorder"),
    options = list(
      "autoWidth" = TRUE,
      "pageLength" = pageLength,
      "scrollY" = scrollY,
      "scrollX" = TRUE,
      "searching" = FALSE,
      "dom" = dom,
      "fixedColumns" = list(leftColumns = fixed),
      "columnDefs" = list(
        list(width = widths[3], visible = T, targets = seq(2, ncol(data) - 1)),
        list(width = widths[2], visible = T, targets = c(1)),
        list(width = widths[1], visible = T, targets = c(0)),
        list(className = "dt-center", targets = "_all"),
        list(targets = "_all", visible = FALSE)
      ),
      footerCallback = javascript
    )
  )
}