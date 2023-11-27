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


#### TRAZENDO A BASE PARA PREDIÇÃO 

# Upload do Data Frame

df <- read.csv("https://raw.githubusercontent.com/mconjaud/TrabalhoFinalAEM2/main/coffee_ratings.csv")


#### ANALIZANDO A BASE ############

View(df) #Vizualizando a base
str(df)  # Tipo da variavel 
skim(df) # % de missings 


# Calculando a proporção de valores missing por variável
prop_missing <- df %>%
  summarise_all(~ mean(is.na(.))) %>%
  gather() %>%
  arrange(desc(value))


grafico <- ggplot(prop_missing, aes(x = reorder(key, -value), y = value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # Rotaciona as barras para torná-las horizontais
  labs(x = "Variável", y = "% de Valores missing", 
       title = "% de Valores missing por Variável")

print(grafico)


#### TRATANDO A BASE ANTES DE APLICAR MODELOS ############


# Removendo as variáveis que não serão utilizadas
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


# Padronizar "unit_of_measurement" em metros
unique(coffee$unit_of_measurement)
view(coffee$unit_of_measurement)

coffee <- coffee %>%
  mutate(unit_of_measurement = 
           ifelse(unit_of_measurement == "ft", "m", unit_of_measurement))


# Preenchimento dos missings com a média de acordo com o país
unique(coffee$country_of_origin)

coffee <- coffee %>%
  group_by(country_of_origin) %>%
  mutate(altitude_mean_meters = ifelse(is.na(altitude_mean_meters),
                                       mean(altitude_mean_meters, na.rm = TRUE),
                                       altitude_mean_meters))

# Trocando "NA" por "other" 
coffee$variety_index <- match(df$variety, unique(df$variety)) # criando indice
coffee$variety <- ifelse(is.na(coffee$variety), "Other", coffee$variety)

quantidade_other <- table(coffee$variety)["Other"] #336 vezes


## não achei a forma de trocar pela mediada ... ainda buscando :(



# Criando a coluna continentes de acordo com o nome do country
coffee$continent <-
  countrycode(sourcevar = coffee$country_of_origin, 
              origin = "country.name", destination = "continent")


view(coffee)

#Removendo a coluna unit_of_measurement (não precisaremos dela)
coffee <- subset(coffee, select = -c(unit_of_measurement))


# não entendo a inclusão de "idade"

# Adding a new column called "Age"
df <- mutate(df, Age = c(25, 30, 22, 28, 35))

# criando um dataset somente com os dados de qualidade (para comparação) 
# assim teremos o dataset coffee (com as colunas selecionadas) e coffee_quality (como somente as colunas de qualidade)

coffee_quality <- subset(coffee, select = c(total_cup_points,aroma,flavor,
                                          aftertaste,acidity,body,balance,
                                          uniformity,clean_cup,sweetness,
                                          cupper_points))



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
