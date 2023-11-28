# TrabalhoFinalEstatistica
# 
# Dados: Coffee

# Bibliotecas -------------------------------------------------------------

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


#Base de dados

dados <- read.csv("https://raw.githubusercontent.com/mconjaud/TrabalhoFinalAEM2/main/coffee_ratings.csv")

summary(dados)

view(dados)
# Tratamento de dados removendo as variáveis que não serão utilizadas:

coffee <- subset (dados, 
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
                  ,variety
                  ,expiration
                  ,certification_body
                  ,certification_address
                  ,certification_contact
                  ,altitude_low_meters
                  ,altitude_high_meters))

view(coffee)
#Separando em 4 regions os countries
valores_unicos <- unique(coffee$country_of_origin)
countries <- c("Ethiopia", "Guatemala", "Brazil", "Peru", "United States", 
  "United States (Hawaii)", "Indonesia", "China", "Costa Rica", "Mexico", 
  "Uganda", "Honduras", "Taiwan", "Nicaragua", "Tanzania, United Republic Of", 
  "Kenya", "Thailand", "Colombia", "Panama", "Papua New Guinea", "El Salvador", 
  "Japan", "Ecuador", "United States (Puerto Rico)", "Haiti", "Burundi", 
  "Vietnam", "Philippines", "Rwanda", "Malawi", "Laos", "Zambia", "Myanmar", 
  "Mauritius", "Cote d'Ivoire", NA, "India")
continentes <- countrycode(countries, "country.name", "continent")

# Adding a new column called "Age"
df <- mutate(df, Age = c(25, 30, 22, 28, 35))

#Processing Method
coffee <- coffee %>%
  mutate(processing_method = ifelse(is.na(processing_method), "Washed / Wet", processing_method))
#color
coffee <- coffee %>%
  mutate(color = ifelse(is.na(color), "Green", color))
#converter a altidude de ft para meters

# Realizar uma regressão Linear

                 
# Dados -------------------------------------------------------------------

set.seed(123)

split <- initial_split(coffee, prop = 0.7)

training <- training(split)
test <- testing(split)

View(training)
View(test)
