
<!-- README.md is generated from README.Rmd. Please edit that file -->

# VisualizacaoDados

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
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

-   Estrutura de saved choices

-   Estrutura de agravo

-   ~~Criação das tabelas a partir do filtro escolhido (server e output
    apenas)~~

### Correções

-   Total das tabelas: achar forma de botar o total por coluna sem ter q
    exibir todas as linhas

-   Ajustar download pra ja vir formatado com xlsx e pra exibir os
    filtros ativos tb.

-   Testar se o filtro baseado em uma das tabelas da certo no full join
    quando tem entrada em uma das tabelas e nao tem na outra. Exemplo:
    t1 full join t2. Tem valor na t2, mas nao tem na t1. Eu coloco o
    filtro em t1. O que acontece?

-   Testar se é mais rapido fazer 2 queries, uma em cada base, e juntar
    os resultados ou fazer uma query só.

## Code of Conduct

Please note that the VisualizacaoDados project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.