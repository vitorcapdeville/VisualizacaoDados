
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
run_app()
```

## Updates futuros

### Modulos

-   Estrutura de agravo

-   incluir outputs q dependem apenas de uma das tabelas

-   Queria colocar um “drill-down”, onde a gente clica em uma tabela e
    aparecem mais detalhes sobre a linha em algum lugar, o mesmo com os
    graficos.

-   Nomear as faixas melhor. Pensar se quero deixar o step dinamico tb.

-   Verificar no reactlog pq a tabela é reconstruida duas vezes qnd muda
    as faixas, e talvez colocar um timer para ter um minimo de tempo
    demorando pra ativar o cssloader

-   Verificar se quero grafico custom tb. Tabela custom preciso usar o
    modulo de saved choices, e mexer no modulo da tabela para permitir
    selecionar quais colunas serao exibidas na tabela final (devo
    permitir diferenciar as colunas q serao trazidas do sql e calculadas
    pos sql das que serao exibidas.)

### Correções

-   Verificar alternativa ao DT.

-   Verificar manipulateWidget

-   Fazer as faixas funcionarem. O noUiSlider por algum motivo nao ta
    renderizando. O slider normal renderiza.

-   Estou usando o proxy, isso significa q quando o filtro muda, ele nao
    mostra o loading na tabela, e mantem tudo q eu ja filtrei e etc. Me
    parece mais clean, mas eu preciso de um aviso pra bloquear a
    interação com o APP enquanto o filtro estiver rodando, talvez um
    sweet alert.

    -   Tirei o proxy por que ele nao atualiza o footer. Teria q volta
        pra callback, mas o callback nao posso usar as paginas
        rsrsrsrsrsrsrsrs

-   Criar uma pagina de recepção com algumas informacoes relevantes.
    Maiores e menores valores por alguma variavel interessante, evolucao
    temporal de valores para os dados completos e etc.

-   Colocar CARDs com valores totais.

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
