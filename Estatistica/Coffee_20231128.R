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
library(doParallel)

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
coffee <- coffee %>%
  mutate(processing_method = 
           ifelse(is.na(processing_method), "Washed / Wet", processing_method))

# Trocar missing por "Green"
coffee <- coffee %>%
  mutate(color = ifelse(is.na(color), "Green", color))

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

view(coffee)

# -----------------------------------------------------------------------------

# Verifiquei que alguns paises estavam com "other" na "variety", vou colocar 
# a moda da variavel e ver se muda algo na acuracia 

coffee$variety <- ifelse(coffee$variety == "Other" ,
                         names(sort(-table(coffee$variety)))[1], #calcula a moda
                         coffee$variety)

# Verificando se ainda eiste Other
teste_other <- subset(coffee, variety == "other")
view(teste_other)

#---------------------------------------------------------------------

## não achei a forma de trocar pela mediada ... ainda buscando :(
## Comentário Flávio: resolvi acima
### S2

#---------------------------------------------------------------------

view(coffee)

#Removendo a coluna unit_of_measurement (não precisaremos dela)
coffee <- subset(coffee, select = -c(unit_of_measurement))


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
# um para análise supervisionada (SEM as colunas de qualidade) e 
# outro para análise não supervisionada (COM as colunas de qualidade)
# O objetivo do primeiro é criar modelos para estimar a classificação do café SEM os dados de qualidade
# Para o segundo é clusterizar os cafés a partir da qualidade dos mesmos

coffee_grade <- subset(coffee, select = -c(total_cup_points,aroma,flavor,
                                          aftertaste,acidity,body,balance,
                                          uniformity,clean_cup,sweetness,
                                          cupper_points))

view(coffee_grade)

# Coffee_grade podemos retirar a variavel "total_cup_points", certo?

coffee_cluster <- subset(coffee, select = c(aroma,flavor,
                                            aftertaste,acidity,body,balance,
                                            uniformity,clean_cup,sweetness,
                                            cupper_points))
view(coffee_cluster)



# Análise Supervisionada -------------------------------------------------------------------

# Tidymodel: Regressão Multinominal e XGBoost

#separando em training e test (coffee_grade)
set.seed(123)
split <- initial_split(coffee_grade, prop = 0.7)
training <- training(split)
test <- testing(split)
View(training)
View(test)

#Receita-------------------------------------------------------------------

#receita coffee_grade
receita <- recipe(grade ~ ., data = training) %>% 
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes()) #transforma as variáveis categóricas em variáveis dummy


#Prepara receita

(receita_prep <- prep(receita))

training_proc <- bake(receita_prep, new_data = NULL)
test_proc <- bake(receita_prep, new_data = test)

# Tidymodels: Regressão Multinomial
# Tipode penalty = 1 lasso \ 2 Ridge


fit_mr <- multinom_reg(penalty = 1, mixture = NULL) %>% # define um modelo de regressao Multinomial e define parâmetros nulos para ter regularização
  set_engine("nnet") %>% # define a engine do modelo
  set_mode("classification") %>% # define que é problema de classificacao
  fit(grade ~ ., training_proc)

fitted <- fit_mr %>% 
  predict(new_data = test_proc) %>% # realiza predicao para os dados de teste - não colocar type = "prob" caso não queira retornar as probabilidades de cada classificação (ao invés da classificação em si)
  mutate(observado = test_proc$grade, # cria uma coluna com o valor observado de default
         modelo = "Regressao Multinomial") # cria uma coluna para indicar qual o modelo ajustado


head(fitted)


# Tidymodels: XGBoost

boost <- boost_tree(trees = tune(), min_n = tune(), 
                    tree_depth = tune(), learn_rate = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

set.seed(123)
cv_split <- vfold_cv(training, v = 5)
registerDoParallel()

boost_grid <- tune_grid(boost, 
                        receita, 
                        resamples = cv_split, 
                        grid = 30, 
                        metrics = metric_set(roc_auc, accuracy))

best <- boost_grid %>% 
  select_best("accuracy") #seleciona o melhor ROC_AUC

boost_fit <- finalize_model(boost, parameters = best) %>% 
  fit(grade ~ ., training_proc)

fitted_bst <- boost_fit %>% 
  predict(new_data = test_proc) %>% 
  mutate(observado = test_proc$grade, 
         modelo = "XGBoost")

fitted <- fitted %>% 
  bind_rows(fitted_bst)

# Redes Neurais----------------------------------
# Preparando os dados para a rede neural 

#Transformando as variáveis preditoras de treinamento e teste em matriz
x_train <- training_proc %>% 
  select(-grade) %>% 
  as.matrix()

View(x_train)

x_test <- test_proc %>% 
  select(-grade) %>% 
  as.matrix()

x_train <- scale(x_train)

x_test <- scale(x_test,
               center = attr(x_train, "scaled:center"),
               scale = attr(x_train, "scaled:scale"))

# A variável resposta é só um vetor inteiro com valores variando de 0 a 9.
#Para preparar esses dados para treinamento, codificamos os vetores
#em matrizes de classe bin?ria usando a fun??o Keras to_categorical():
y_train <- to_categorical(training_proc$grade)
y_test <- to_categorical(test_proc$grade)


net <- keras_model_sequential() %>% 
  layer_dense(units = 64, 
              activation = "relu", 
              input_shape = ncol(x_train)) %>% 
  #layer_dropout(rate = 0.1) %>% # regularizacao para evitar overfitting
  layer_dense(units = 32, activation = "relu") %>% 
  #layer_dropout(rate = 0.1) %>%  # regularizacao para evitar overfitting
  layer_dense(units = 16, activation = "relu") %>% 
  #layer_dropout(rate = 0.1) %>%  # regularizacao para evitar overfitting
  layer_dense(units = 8, activation = "relu") %>% 
  layer_dense(units = 4, activation = 'softmax')

summary(net)

# Compilando o modelo -----------------------------------------------------
# to configure the learning process

net <- compile(net,
               loss = 'categorical_crossentropy', 
               optimizer = "adam", #ver comentario abaixo
               metrics = c("accuracy") #pode ser customizada
)


# Treinando o modelo ------------------------------------------------------

history <- fit(net, x_train, y_train,
               batch_size = 24, epochs = 30, # defaults: 32, 10
               validation_split = 0.2)

# batch_size: define o n?mero de amostras para trabalhar antes de atualizar o
# par?metros internos do modelo.
# epoch: define o n?mero de vezes que o algoritmo de aprendizado funcionar?
# todo o conjunto de dados de treinamento (Uma ?poca significa que cada amostra no treinamento
# conjunto de dados teve a oportunidade de atualizar os par?metros do modelo interno. ).

# N?o h? regras m?gicas de como configurar esses par?metros. Voc? deve tentar diferente
# valores e veja o que funciona melhor para o seu problema.

history
plot(history)


# Avaliando o modelo ------------------------------------------------------

# Predicao

pred_net <- net %>% predict(x_test) %>% k_argmax()%>% k_get_value()

#data.frame(y = test_proc$grade, y_hat = pred1)

#mean(abs(test_proc$grade == as.numeric(pred1)))

#Calcula a acurácia do modelo de rede neurais
Metrics::accuracy(test_proc$grade, pred_net)

# Cria dataframe com os valores observados e estimados do modelo de redes neurais
fitted_net <- data.frame(.pred_class = factor(pred_net), observado = test_proc$grade, modelo = "Redes Neurais")

fitted <- fitted %>% 
  bind_rows(fitted_net)


head(fitted)

#compara as métricas dos 3 modelos
fitted %>% 
  group_by(modelo) %>% 
  metrics(truth = observado, estimate = .pred_class)


#O valor de Kappa varia de -1 a 1, sendo interpretado da seguinte forma:
  
#Kappa próximo de 1: Indica uma concordância perfeita entre observadores ou métodos.
#Kappa próximo de 0: Indica uma concordância igual àquela esperada ao acaso.
#Kappa próximo de -1: Indica uma concordância inversa, o que geralmente não é considerado válido em muitos contextos.

#A interpretação do valor de Kappa pode ser resumida da seguinte maneira:
  
#0.81-1.00: Concordância quase perfeita.
#0.61-0.80: Concordância substancial.
#0.41-0.60: Concordância moderada.
#0.21-0.40: Concordância fraca.
#0.00-0.20: Concordância mínima ou inexistente.