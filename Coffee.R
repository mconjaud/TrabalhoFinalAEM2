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


# não entendo a inclusão de "idade"

# Adding a new column called "Age"
df <- mutate(df, Age = c(25, 30, 22, 28, 35))



# Dados -------------------------------------------------------------------

set.seed(123)
split <- initial_split(coffee, prop = 0.7)
training <- training(split)
test <- testing(split)
View(training)
View(test)