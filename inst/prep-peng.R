# #Dummy data
# require(palmerpenguins)
# require(RSQLite)
# require(dplyr)
# require(purrr)
# require(lubridate)
# require(data.table)
#
# # Conexão com SQL (aqui no caso, com a memória do PC por que não tenho SQL)
# con <- dbConnect(RSQLite::SQLite(), "inst/penguins.sqlite")
# # Nome das tabelas (premio e sinistro, por ex)
# tabela1 = "bill"
# tabela2 = "rest"
#
# # Colunas exploratorias
# colunasExp = c("species", "island", "sex", "year","idade", "Flag")
# colunasExpNome = c("Espécies", "Ilha", "Sexo", "Ano", "Idade", "Flag")
# colunasExpTipo = c("picker", "picker", "picker", "dateRange", "slider", "checkBox")
# # Colunas de valor
# colunasValor1 = c("bill_length_mm", "bill_depth_mm")
# colunasValor2 = c("flipper_length_mm", "body_mass_g")
#
# #-------------------------------------------------------
# # Essa parte não existiria se os dados estivessem em SQL
# penguins1 <- penguins %>% mutate(idade = rpois(nrow(penguins), 10)) %>%
#   mutate(Flag = as.character(as.numeric(rbernoulli(nrow(penguins))))) %>%
#   mutate(year = glue::glue("{year}-01-01")) %>%
#   mutate(sex = as.character(sex)) %>%
#   mutate(across(where(is.factor), as.character)) %>%
#   mutate(across(where(is.numeric), ~ifelse(is.na(.x), 0, .x))) %>%
#   mutate(across(!where(is.numeric), ~ifelse(is.na(.x), "", .x)))
#
# # Separar em dois conjuntos (premio e sinistro, por exemplo)
# bill = penguins1 %>%
#   select(species, island, sex, year, idade, Flag, bill_length_mm, bill_depth_mm) %>%
#   as.data.table()
#
# rest = penguins1 %>%
#   select(species, island, sex, year, idade, Flag, flipper_length_mm, body_mass_g) %>%
#   as.data.table()
#
#
# if (anyNA(rest, recursive = T)) stop("NAs sao proibidos")
# if (anyNA(bill, recursive = T)) stop("NAs sao proibidos")
#
# formata_dados(con, bill, rest, "bill", "rest", colunasExp, colunasValor1, colunasValor2, overwrite = T)
#
# dbDisconnect(con)
#
