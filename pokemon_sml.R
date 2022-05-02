# libraries and data ------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(corrplot)

pokemon <- read_csv("data sets/Pokemon.csv")

# eda ---------------------------------------------------------------------
pokemon %>% 
  glimpse()

pokemon %>% 
  count(Legendary)

pokemon %>% 
  count(Generation)

pokemon %>% 
  summary()

pokemon %>% 
  ggplot(aes(x = Attack, y = Defense, color = Legendary)) + 
  geom_point() + 
  theme_minimal()

pokemon_cor <- pokemon %>% 
  select(HP:Speed) %>% 
  cor()

pokemon_cor %>% 
  corrplot(., method = "circle", type = "upper", order = "hclust")
