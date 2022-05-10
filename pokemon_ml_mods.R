# library & data ----------------------------------------------------------
library(tidyverse)
library(GGally)
library(tidymodels)
library(kknn)
library(ranger)
library(corrplot)

pokemon <- read_csv("data sets/Pokemon.csv") %>% 
  janitor::clean_names() %>% 
  mutate(legendary = factor(legendary, levels = c(TRUE, FALSE)))

# predict legendary pokemon using different classification models

# eda ---------------------------------------------------------------------

# count and percentage of legendary pokemon
pokemon_legendary <- pokemon %>% 
  count(legendary) %>% 
  mutate(percent = n/sum(n)) 

pokemon_legendary %>% 
  ggplot(aes(x=legendary, y=percent)) + 
  geom_col() + 
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() + 
  labs(x = "Legendary",
       y = "Percent") + 
  coord_flip()

# legendary by generation
pokemon_legendary_gen <- pokemon %>% 
  count(legendary, generation) %>% 
  mutate(percent = n/sum(n)) %>% 
  filter(legendary == TRUE) 

pokemon_legendary_gen %>% 
  ggplot(aes(x=generation, y=percent)) + 
  geom_col() + 
  scale_y_continuous("Percent" ,labels = scales::percent) +
  scale_x_continuous("Generation", breaks = c(1:6)) +
  theme_minimal()

# correlation for all pokemon stats
pokemon_cor <- pokemon %>% 
  select(total:speed) %>% 
  cor()

pokemon_cor %>% 
  corrplot()

# correlation, histogram, and scatter plot of stats  
ggpairs(pokemon,
        columns = 5:11,
        aes(color = legendary,
            alpha = 0.2)) + 
  theme_minimal()


