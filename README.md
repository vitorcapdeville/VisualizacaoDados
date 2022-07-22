
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

-   incluir outputs q dependem apenas de uma das tabelas

-   Coloquei barplots, mas preciso adaptar ainda para o caso que o
    barplot depende das duas tabelas (atualmente so da pra fazer com
    uma. Preciso q seja possivel fazer com dados q dependam só de uma ou
    das duas. Talvez seja mais facil botar sempre as duas independente
    de depender de uma ou das duas)

### Correções

-   Estou usando o proxy, isso significa q quando o filtro muda, ele nao
    mostra o loading na tabela, e mantem tudo q eu ja filtrei e etc. Me
    parece mais clean, mas eu preciso de um aviso pra bloquear a
    interação com o APP enquanto o filtro estiver rodando, talvez um
    sweet alert.

    -   Tirei o proxy por que ele nao atualiza o footer. Teria q volta
        pra callback, mas o callback nao posso usar as paginas
        rsrsrsrsrsrsrsrs

-   Colocar CARDs com valores totais.

-   Verificar se faz mais sentido usar o dplyr pra montar a query ao
    inves de montar a query explicitamente. quero ver um jeito de
    compartilhar os dados entre as tabelas e os graficos, pro caso em
    que o grafico e a tabela usam o mesmo dado, se possivel.

-   Adicionar uma forma de exibir a query que gerou a tabela atual,
    junto com o download.

-   Verificar se vale mudar para a tabela gerada com excelR. Precisaria
    achar uma forma de incluir os filtros

-   Ajustar download pra ja vir formatado.

-   Dar um jeito de proibir o download enquanto a tabela nao esta
    disponivel. Na verdade, dar um jeito de bloquear todos os botoes
    enquanto a tabela nao esta disponivel.

-   Verificar a possibilidade de sobrescrever as opções definidas em
    “golem-config.yml” usando os parametros do run_app().

-   Verificar como usar uma imagem fora de www no shiny.

-   Verificar global assignment em app_global ou uma melhor forma de
    compartilhar valores em app_server e app_ui.

-   Verificar se é mais eficiente juntar as duas tabelas em uma tabela
    só, ou deixar duas tabelas separadas. A ideia de duas tabelas
    separadas é que uma delas geralmente é muito menor, entao em tese
    juntar as duas ocuparia mais espaco, ja que teria q replicar NULL em
    varias linhas da segunda tabela.

## Code of Conduct

Please note that the VisualizacaoDados project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
