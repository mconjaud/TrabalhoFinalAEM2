# Trabalho Final Estatistica
# 
# Dados: Coffee

####### Bibliotecas Necessárias para aplicação dos modelos

library(keras)
library(GGally)
library(ggrepel)
library(factoextra)
library(countrycode)
library(dplyr)
library(car)
library(tidyr)
library(rpart)
library(rpart.plot)
library(partykit)
library(skimr)
library(glmnet)
library(plotmo)
library(naniar)
library(rsample)
library(yardstick)
library(modeldata)
library(ranger)
library(patchwork)
library(vip)
library(tidymodels)
library(tidyverse)
library(ISLR)
library(doParallel)
library(gbm)
library(xgboost)
library(conflicted)
library(corrplot)

####### TRAZENDO A BASE PARA PREDIÇÃO 

# Upload do Data Frame

df <- read.csv("https://raw.githubusercontent.com/mconjaud/TrabalhoFinalAEM2/main/Estatistica/coffee_ratings.csv")


################## ANALISANDO A BASE ##########################

#Visualizando a base
View(df) 

# Tipo da variavel
str(df)   


# Gráfico com o % de variáveis NA
colunas_na <- colSums(is.na(df))
info_na <- colunas_na[colunas_na > 0] # 4 colunas com NA
porcent_NA <- info_na / nrow(df) * 100

barplot(porcent_NA, main = "Porcentagem de NA por Coluna", 
        ylab = "Porcentagem de NA", xlab = "Colunas",
        col = "blue", las = 2)  # las = 2 para rotacionar os rótulos do eixo x



################## TRATANDO A BASE ##########################

# Removendo Variaveís com elevada correlação com a coluna resposta  

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
                              ,expiration
                              ,certification_body
                              ,certification_address
                              ,certification_contact
                              ,unit_of_measurement
                              ,aroma	
                              ,flavor	
                              ,aftertaste	
                              ,acidity	
                              ,body	
                              ,balance	
                              ,uniformity	
                              ,clean_cup	
                              ,sweetness
                              ,cupper_points
                              ,altitude_low_meters	
                              ,altitude_high_meters
                              
                              
))


# Trocar NA por "Washed / Wet"
coffee <- coffee %>%
  mutate(processing_method = 
           ifelse(is.na(processing_method), "Washed / Wet", processing_method))

# Trocar NA por "Green"
coffee <- coffee %>%
  mutate(color = ifelse(is.na(color), "Green", color))


# Preenchimento dos NA com a média de acordo com o país
coffee <- coffee %>%
  group_by(country_of_origin) %>%
  mutate(altitude_mean_meters = ifelse(is.na(altitude_mean_meters),
                                       mean(altitude_mean_meters, na.rm = TRUE),
                                       altitude_mean_meters))

# Trocando "NA" por "0" 
coffee <- coffee %>%
  mutate(quakers = ifelse(is.na(quakers), 0, quakers))

# Criando faixas de corte (Faixas reconhecida internacionalmente) 
coffee$specialty_coffee <- 
  ifelse(coffee$total_cup_points < 80, 1,
  ifelse(coffee$total_cup_points >= 80 & coffee$total_cup_points < 85, 2,
  ifelse(coffee$total_cup_points >= 85 & coffee$total_cup_points < 90, 3, 
  4)))

# Retirando dados NA da variavel "country_of" pois tem a mesma qtd das demais
coffee_01 <- coffee[complete.cases(coffee$country_of_origin), ]

view(coffee_01)





####### Gráficos

# Proporção de café por faixa de score
contagem_faixas <- table(coffee_01$specialty_coffee)

barplot(prop.table(contagem_faixas) * 100, main = "Porcentagem de Faixa de score",
        xlab = "Faixa de score", ylab = "Porcentagem", col = "green")

# Gráfico com o % de variáveis NA
colunas_na <- colSums(is.na(coffee_01))
info_na <- colunas_na[colunas_na > 0] # 4 colunas com NA
porcent_NA <- info_na / nrow(coffee_01) * 100

barplot(porcent_NA, main = "Porcentagem de NA por Coluna", 
        ylab = "Porcentagem de NA", xlab = "Colunas",
        col = "blue", las = 2)  # las = 2 para rotacionar os rótulos do eixo x


####### Analise de Colinearidade 

# Calculando a matriz de correlação
dados <- coffee_01
colunas_nao_numericas <- sapply(dados, class) != "numeric"
dados_numericos <- dados[, !colunas_nao_numericas]
matrix_de_correlacao <- cor(dados_numericos)
corrplot(matrix_de_correlacao, method = "color", type = "upper", tl.col = "black", tl.srt = 45)


####### APLICANDO O PCA 
# PCA seleciona a qtd de variaveis que soma 80% da variancia


# Data frame sem a variavel resposta 
coffee_pca <- subset (coffee_01, 
                  select = -c(country_of_origin, total_cup_points))

# Ajustando data frame
pca <- coffee_pca %>%
  select(is.numeric) %>% # variaveis numericas
  prcomp(scale = TRUE, center = TRUE) # aplica PCA


pca$rotation <- -pca$rotation # troca o sinal das cargas
pca$x <- -pca$x # troca o sinal dos scores

Phi <- pca$rotation # matriz de cargas
head(Phi)

Z <- pca$x # matriz de scores
head(Z)

# o grafico abaixo mostra o percentual explicado da variancia de cada componente
fviz_eig(pca, addlabels = TRUE, ncp = 17) + 
  labs(x = "Componente Principal",
       y = "Percentual explicado da variância")


# Selecionar os primeiros N componentes principais
N <- 6
dados_pca <- as.data.frame(pca$x[, 1:N])

# Juntar os componentes do PCA com a variável categórica e a variável de resposta
dados_modelo <- cbind(
  dados_pca, paises = coffee_01$country_of_origin, 
  total_cup_points = coffee_01$total_cup_points)

# Aplicar one-hot encoding na variável categórica
# ajuda no Kmeans e Rede neural
dados_one_hot <- model.matrix(~ . - 1, data = dados_modelo)
dados_final <- as.data.frame(dados_one_hot)

# Limpeza dos nomes das colunas
colnames(dados_final) <- gsub("paises", "", colnames(dados_final))
colnames(dados_final) <- gsub(" ", "", colnames(dados_final))
colnames(dados_final) <- gsub("\\.", "", colnames(dados_final))

# Verificando a contagem de linhas
nrow(dados_modelo)
nrow(dados_final)
view(dados_final)


#### treinamento x teste 

set.seed(123)

split <- initial_split(coffee_01, prop = .80)

treino <- training(split)
teste  <- testing(split)


################## APLICANDO MODELOS ##########################

# O primeiro passo é definir a receita, com a variavel resposta e os dados 

receita <- recipe(total_cup_points ~ ., data = treino) %>% 
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes()) 


# Preparar a receita definida acima:

(receita_prep <- prep(receita))

# Por fim, obtemos os dados de treinamento e teste processados: 

treino_proc <- bake(receita_prep, new_data = NULL)
teste_proc <- bake(receita_prep, new_data = teste)


#### Regressão Linear .....................................................

# Primeiramente será estimado o modelo de regressão linear:

lm <- linear_reg() %>% set_engine("lm") 
lm_fit <- linear_reg() %>% 
  set_engine("lm") %>%
  fit(total_cup_points ~ ., treino_proc) 


# Aplicando o modelo de previsão nos dados de teste e estimando os dados de previsão, armazenando numa tabela chamada fitted_lm:

fitted<- lm_fit %>% 
  predict(new_data = teste_proc) %>% 
  mutate(observado = teste_proc$total_cup_points, 
         modelo = "Regressao Linear") %>%
  mutate(fitted = pmax(pmin(.pred, 100), 0))

view(fitted)



#### XGboost ..........................................................


# O último modelo a ser estimado é o XGBoost. Inicialmente é definir o modelo 
# e quais hiperparâmetros serão otimizados (trees = número de árvores,
# min_n = valor mínimo por nó, tree_depth = profundidade da árvore e 
# learn_rate = taxa de aprendizagem do modelo):

boost <- boost_tree(trees = tune(), min_n = tune(), 
                    tree_depth = tune(), learn_rate = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")


# Para esse processo de escolha dos hiperparâmetros, também será utilizada a 
# validação cruzada em 10 lotes. A seguir, serão escolhidos os valores de 
# hiperparâmetros para os melhores erro quadrático médio (rmse) a partir de um 
# grid de 30:

set.seed(123)
cv_split <- vfold_cv(treino, v = 5)

boost_grid <- tune_grid(boost, 
                        receita, 
                        resamples = cv_split, 
                        grid = 30, 
                        metrics = metric_set(rmse, mae))

best <- boost_grid %>% 
  select_best("rmse")


# Por fim será aplicado o modelo de previsão nos dados de teste e estimando os 
# dados de previsão, armazenando numa tabela chamada fitted_rf:

boost_fit <- finalize_model(boost, parameters = best) %>% 
  fit(total_cup_points ~ ., treino_proc)

fitted_bst <- boost_fit %>% 
  predict(new_data = teste_proc) %>% 
  mutate(observado = teste_proc$total_cup_points, 
         modelo = "XGBoost")%>%
  mutate(fitted = pmax(pmin(.pred, 100), 0))

fitted <- fitted %>% 
  bind_rows(fitted_bst)


# Resultados finais dos modelos acima estimados com tidymodels estão abaixo:


fitted %>% 
  group_by(modelo) %>% 
  metrics(truth = observado, estimate = .pred) 


# RMSE = EQM desvio padrão dos valores residuais (erros de previsão) 
# RSQ R quadrado 
# MAE média de erro absoluto  

