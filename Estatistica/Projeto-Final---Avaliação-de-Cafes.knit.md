---
title: "Avaliação de Cafés"
author: "Flávio Barbosa Shirahige, Hélio Pereira de Oliveira e Michel Maurice Conjaud Neto"
date: "2023-11-24"
output: html_document
---


```r
knitr::opts_chunk$set(echo = TRUE)
library(MASS)
library(tidyverse)
library(keras)
library(rsample)
library(keras)
library(yardstick)
library(skimr)
library(dplyr)
library(GGally)
library(ggrepel)
library(factoextra)
library(countrycode)
library(dplyr)
library(tidymodels)
library(tidyverse)
library(ISLR)
library(vip)
library(doParallel)
library(skimr)
library(corrplot)
```

## Introdução

Inicialmente, o objetivo desse projeto é prever o pontuação/score da avaliação de cafés a partir das demais preditoras do dataset (análise supervisionada) e agrupar os tipos de café a partir da qualidade do café (análise não supervisionada).

No entanto, a pontuação/score final de um café é a simples somatória das notas de cada uma das qualidades avaliadas. Assim, a nota final é simplesmente uma modelo linear das variáveis de qualidades de café, tornando-se um modelo com erro quase zero.

Assim, optou-se por construir um modelo de classificação que tenta prever a qualidade do café para quatro diferentes níveis: ruim, bom,muito bom e excelente. Para tal, não serão utilizadas nesse processo de modelagem as variáveis que compõe a nota final dos café. Logo, visa-se prever a qualidade do café a partir de outras características do mesmo, como local, tipo de café, grão etc.

Nesse projeto será utilizada a base de dados com informações de qualidade de café coletadas das avaliações do Coffee Quality Institute  de Janeiro de 2018. A avaliação dos café é feita a partir de diversas características do café, como acidez, doçura, equilíbrio etc. e é pontuada numa escala de 0 a 100.
Para os modelos supervisionados, foi criada uma variável resposta a partir dessa pontuação, onde:

- Pontuação menor do que 70: Ruim (será considerado como 0)
- Pontuação maior do que 70 e menor do que 80:  Bom (será considerado como 1)
- Pontuação maior do que 85 e menor do que 85: Muito Bom (será considerado como 2)
- Pontuação maior do que 85: Excelente (será considerado comoá 3)

Essa critério segue o padrão dos principais institutos que avaliam cafés.

Para os modelos supervisionados, será aplicado o PCA e o K-means para clusterização e serão consideradas somente as variáveis de qualidade, conforme abaixo. O objetivo é criar grupos de cafés a partir das características avaliadas.

- Aroma grade
- Flavor grade
- Aftertaste grade
- Acidity grade
- Body grade
- Balance grade
- Uniformity grade
- Clean cup grade
- Sweetness grade
- Cupper Points



## Tratamento e Análise Exploratória dos Dados

Inicialmente, serão importados os dados:










































































