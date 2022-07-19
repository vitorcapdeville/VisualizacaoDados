
<!-- README.md is generated from README.Rmd. Please edit that file -->

# VisualizacaoDados

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/vitorcapdeville/VisualizacaoDados/branch/master/graph/badge.svg)](https://app.codecov.io/gh/vitorcapdeville/VisualizacaoDados?branch=master)
<!-- badges: end -->

Ferramenta para visualização de dados no formato de tabelas, com
estrutura de filtros.

## Installation

You can install the development version of VisualizacaoDados like so:

``` r
devtools::install_github("vitorcapdeville/VisualizacaoDados")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(VisualizacaoDados)
## basic example code
```

## Updates futuros

### Modulos

-   Estrutura de faixas (faixa etária, faixa de capital etc)

    -   Incluir quantidade dinâmica de faixas

-   Estrutura de agravo

### Correções

-   Ajustar download pra ja vir formatado com xlsx e pra exibir os
    filtros ativos tb.

-   Testar se o filtro baseado em uma das tabelas da certo no full join
    quando tem entrada em uma das tabelas e nao tem na outra. Exemplo:
    t1 full join t2. Tem valor na t2, mas nao tem na t1. Eu coloco o
    filtro em t1. O que acontece?

-   Verificar se é mais eficiente juntar as duas tabelas em uma tabela
    só, ou deixar duas tabelas separadas. A ideia de duas tabelas
    separadas é que uma delas geralmente é muito menor, entao em tese
    juntar as duas ocuparia mais espaco, ja que teria q replicar NULL em
    varias linhas da segunda tabela.

-   Incluir a documentação e reescrever as funções de forma mais
    intuitiva.

-   Verificar a possibilidade de sobrescrever as opções definidas em
    “golem-config.yml” usando os parametros do run_app().

-   Verificar como usar uma imagem fora de www no shiny.

-   Verificar global assignment em app_global ou uma melhor forma de
    compartilhar valores em app_server e app_ui.

## Code of Conduct

Please note that the VisualizacaoDados project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
