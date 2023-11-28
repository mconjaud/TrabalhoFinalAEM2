# TrabalhoFinalEstatistica
# 
# Dados: Coffee

#### Bibliotecas Necessárias para aplicação dos modelos

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


#### TRAZENDO A BASE PARA PREDIÇÃO 

# Upload do Data Frame

df <- read.csv("https://raw.githubusercontent.com/mconjaud/TrabalhoFinalAEM2/main/Estatistica/coffee_ratings.csv")


#### ANALIZANDO A BASE ############

View(df) #Vizualizando a base
str(df)  # Tipo da variavel 
skim(df) # % de missings 


## troquei o gráfico por um mais 'bonito'

# Gráfico com o % de variáveis NA
colunas_na <- colSums(is.na(df))
info_na <- colunas_na[colunas_na > 0] # 4 colunas com NA
porcent_NA <- info_na / nrow(df) * 100

barplot(porcent_NA, main = "Porcentagem de NA por Coluna", 
        ylab = "Porcentagem de NA", xlab = "Colunas",
        col = "blue", las = 2)  # las = 2 para rotacionar os rótulos do eixo x



#### TRATANDO A BASE ANTES DE APLICAR MODELOS ############

# Removendo as linhas com missing values da coluna country_of_origin, pois essa linha tem pouca informação

df <- df[complete.cases(df$country_of_origin), ]

# Removendo as variáveis que não serão utilizadas para as duas análises (supervisionada e não supervisionada)
coffee <- subset (df, 
                  select = -c(owner
                              ,farm_name
                              ,lot_number
                              ,mill
                              ,ico_number
                              ,company
                              ,altitude
                              ,region
                              ,producer
                              ,number_of_bags
                              ,bag_weight
                              ,in_country_partner
                              ,harvest_year
                              ,grading_date
                              ,owner_1
                              #,variety
                              ,expiration
                              ,certification_body
                              ,certification_address
                              ,certification_contact
                              ,altitude_low_meters
                              ,altitude_high_meters))


# Trocar missing por "Washed / Wet"
unique(coffee$processing_method)

coffee <- coffee %>%
  mutate(processing_method = 
           ifelse(is.na(processing_method), "Washed / Wet", processing_method))

# Trocar missing por "Green"
unique(coffee$color)

coffee <- coffee %>%
  mutate(color = ifelse(is.na(color), "Green", color))

#---------------------------------------------------------------------
#Deixei comentado os códigos abaixo, pois acho que teremos que converter mesmo de pés para metros

# Padronizar "unit_of_measurement" em metros
#unique(coffee$unit_of_measurement)
#view(coffee$unit_of_measurement)

#coffee <- coffee %>%
#  mutate(unit_of_measurement = 
#           ifelse(unit_of_measurement == "ft", "m", unit_of_measurement))
#---------------------------------------------------------------------

#Converte a coluna altitude_mean_meters de pés para metros
coffee<- coffee %>%
  mutate(altitude_mean_meters = ifelse(unit_of_measurement == "ft", altitude_mean_meters * 0.3048, altitude_mean_meters))

# há problema de qualidade dos dados, pois há medidas acima dos 10000 (mais alto que o Everest).
#Assim, vamos dividir esses valores por 100 (valores acima dos 100.000) e por 10 (acima dos 5000), nesse sequência.
coffee<- coffee %>%
  mutate(altitude_mean_meters = ifelse(altitude_mean_meters > 100000, altitude_mean_meters/100, altitude_mean_meters))

coffee<- coffee %>%
  mutate(altitude_mean_meters = ifelse(altitude_mean_meters > 5000, altitude_mean_meters/10, altitude_mean_meters))

# Preenchimento dos missings da coluna altitude_mean_meters pela média por país
coffee <- coffee %>%
  group_by(country_of_origin) %>%
  mutate(altitude_mean_meters = ifelse(is.na(altitude_mean_meters),
                                       mean(altitude_mean_meters, na.rm = TRUE),
                                       altitude_mean_meters))
#---------------------------------------------------------------------
#Deixei comentado os códigos abaixo, pois substitui os missing values da coluna variety pela moda por país
# substituindo missing values da 
# Trocando "NA" por "other" em variety
#coffee$variety_index <- match(df$variety, unique(df$variety)) # criando indice
#coffee$variety <- ifelse(is.na(coffee$variety), "Other", coffee$variety)

#quantidade_other <- table(coffee$variety)["Other"] #336 vezes

#---------------------------------------------------------------------

# Substitui os valores ausentes em quakes pela moda
coffee$quakers <- ifelse(is.na(coffee$quakers),
                         names(sort(-table(coffee$quakers)))[1], #calcula a moda
                         coffee$quakers)

#Tratamento dos missing values da coluna variety': vamos substituir pelo valor mais frequente por país
# A Costa do Marfim, Equador, Mauricios e Papua Nova Guine só possuem um café avaliado e não possui valores de variety.
#Assim, vamos usar a moda da coluna para esses países

coffee$variety <- ifelse(coffee$country_of_origin == "Cote d?Ivoire" |
                           coffee$country_of_origin == "Ecuador" |
                           coffee$country_of_origin == "Mauritius"|
                           coffee$country_of_origin == "Papua New Guinea",
                         names(sort(-table(coffee$variety)))[1], #calcula a moda
                         coffee$variety)

# Cria uma tabela com a moda de variety por country_of_origin
moda_por_regiao <- coffee %>%
  group_by(country_of_origin) %>%
  summarise(moda_variety = names(sort(-table(variety)))[1])

moda_por_regiao <- na.omit(moda_por_regiao)

# Substitui os valores ausentes pelo valor mais frequente na região
coffee <- coffee %>%
  group_by(country_of_origin) %>%
  mutate(variety = ifelse(is.na(variety), moda_por_regiao$moda_variety[match(country_of_origin, moda_por_regiao$country_of_origin)], variety))


sum(is.na(coffee$variety))

#---------------------------------------------------------------------

## não achei a forma de trocar pela mediada ... ainda buscando :(
## Comentário Flávio: resolvi acima
### S2



# Criando a coluna continentes de acordo com o nome do country
## Comentário Flávio: precisamos da uma coluna chamada continente? Fiquei na dúvida
### na ultima versão que eu fiz eu havia retirado, realmente não agrega, vou comentar 

#coffee$continent <-
#  countrycode(sourcevar = coffee$country_of_origin, 
#              origin = "country.name", destination = "continent")

#---------------------------------------------------------------------

view(coffee)

#Removendo a coluna unit_of_measurement (não precisaremos dela)
coffee <- subset(coffee, select = -c(unit_of_measurement))

#---------------------------------------------------------------------
# não entendo a inclusão de "idade"
## Comentário Flávio: acho que não faz sentido também. Aí deixei comentado.

# Adding a new column called "Age"
#df <- mutate(df, Age = c(25, 30, 22, 28, 35))
#---------------------------------------------------------------------


## Criar a coluna chamada Grade com a classificação dos cafés
## Critério:
## < 70 - Ruim (será 0)
## 70>= e < 80 - Bom (será 1)
## 80>= e < 85 - Muito Bom (será 2)
## 85 >= - Excelente (será 3)

coffee$grade <- cut(coffee$total_cup_points,
                                    breaks = c(-Inf, 70, 80, 85, Inf),
                                    labels = c(0, 1, 2, 3),
                                    right = FALSE)


# Proporção de faixas + Colinearidade ----------------------------------------------------

####### Propoção

# Proporção de café por faixa de score
contagem_faixas <- table(coffee$grade)

barplot(prop.table(contagem_faixas) * 100, main = "Porcentagem de Faixa de score",
        xlab = "Faixa de score", ylab = "Porcentagem", col = "green")

####### Verificando NA

# % de variáveis NA após tratativa da base
colunas_na <- colSums(is.na(coffee))
info_na <- colunas_na[colunas_na > 0] # 4 colunas com NA
porcent_NA <- info_na / nrow(coffee) * 100

####### Analise de Colinearidade 

# Calculando a matriz de correlação
dados <- coffee
colunas_nao_numericas <- sapply(dados, class) != "numeric"
dados_numericos <- dados[, !colunas_nao_numericas]
matrix_de_correlacao <- cor(dados_numericos)

options(repr.plot.width = 32, repr.plot.height = 32) 
corrplot(matrix_de_correlacao, method = "color", type = "upper", tl.col = "black", tl.srt = 50)



#-----------------------------------------------------------------------------

# Vamos criar agora dois datasets:
# um para análise supervisionada (SEMas colunas de qualidade) e 
# outro para análise não supervisionada (COM as colunas de qualidade)
# O objetivo do primeiro é criar modelos para estimar a classificação do café SEM os dados de qualidade
# Para o segundo é clusterizar os cafés a partir da qualidade dos mesmos

coffee_grade <- subset(coffee, select = -c(total_cup_points,aroma,flavor,
                                          aftertaste,acidity,body,balance,
                                          uniformity,clean_cup,sweetness,
                                          cupper_points))

view(coffee_grade)

# Coffee_grade podemos retirar a variavel "total_cup_points", certo?

coffee_cluster <- subset(coffee, select = c(total_cup_points,aroma,flavor,
                                            aftertaste,acidity,body,balance,
                                            uniformity,clean_cup,sweetness,
                                            cupper_points))
view(coffee_grade)


# Dados -------------------------------------------------------------------

#separando em training e test (coffee)
set.seed(123)
split <- initial_split(coffee, prop = 0.7)
training <- training(split)
test <- testing(split)
View(training)
View(test)

#separando em training e test (coffee_quality)

split_quality <- initial_split(coffee_quality, prop = 0.7)
training_quality <- training(split_quality)
test_quality <- testing(split_quality)
View(training_quality)
View(test_quality)


#Receita-------------------------------------------------------------------

#receita coffee
receita <- recipe(total_cup_points ~ ., data = training) %>% 
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes()) 

#receita coffee_quality
receita_quality <- recipe(total_cup_points ~ ., data = training_quality) %>% 
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes()) 

#Prepara receita

(receita_prep <- prep(receita))

training_proc <- bake(receita_prep, new_data = NULL)
test_proc <- bake(receita_prep, new_data = test)

(receita_prep_quality <- prep(receita_quality))

training_proc_quality <- bake(receita_prep_quality, new_data = NULL)
test_proc_quality <- bake(receita_prep_quality, new_data = test_quality,)

# Tidymodels: Regressão Linear

lm <- linear_reg() %>% set_engine("lm") 
lm_fit <- linear_reg() %>% 
  set_engine("lm") %>%
  fit(total_cup_points ~ ., training_proc)

fitted<- lm_fit %>% 
  predict(new_data = test_proc) %>% 
  mutate(observado = test_proc$total_cup_points, 
         modelo = "Regressao Linear")

head(fitted)

lm_quality <- linear_reg() %>% set_engine("lm") 
lm_fit_quality <- linear_reg() %>% 
  set_engine("lm") %>%
  fit(total_cup_points ~ ., training_proc_quality)

fitted_quality<- lm_fit_quality %>% 
  predict(new_data = test_proc_quality) %>% 
  mutate(observado = test_proc_quality$total_cup_points, 
         modelo = "Regressao Linear Quality")

fitted <- fitted %>% 
  bind_rows(fitted_quality)

head(fitted_quality)


# Tidymodels: XGBoost

boost <- boost_tree(trees = tune(), min_n = tune(), 
                    tree_depth = tune(), learn_rate = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

set.seed(123)
cv_split <- vfold_cv(training, v = 5)
registerDoParallel()

boost_grid <- tune_grid(boost, 
                        receita, 
                        resamples = cv_split, 
                        grid = 30, 
                        metrics = metric_set(rmse, mae))

best <- boost_grid %>% 
  select_best("rmse")

boost_fit <- finalize_model(boost, parameters = best) %>% 
  fit(total_cup_points ~ ., training_proc)

fitted_bst <- boost_fit %>% 
  predict(new_data = test_proc) %>% 
  mutate(observado = test_proc$total_cup_points, 
         modelo = "XGBoost")

fitted <- fitted %>% 
  bind_rows(fitted_bst)

fitted %>% 
  group_by(modelo) %>% 
  metrics(truth = observado, estimate = .pred) 

mean(coffee$total_cup_points)

lm_fit_quality

min(coffee$total_cup_points)
