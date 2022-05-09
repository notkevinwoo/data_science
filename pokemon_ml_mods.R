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
pokemon %>% 
  count(legendary) %>% 
  mutate(percent = n/sum(n)) %>% 
  ggplot(aes(x=legendary, y=percent)) + 
  geom_col() + 
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() + 
  labs(x = "Legendary",
       y = "Percent") + 
  coord_flip()

pokemon %>% 
  count(legendary, generation) %>% 
  mutate(percent = n/sum(n)) %>% 
  filter(legendary == TRUE) %>% 
  ggplot(aes(x=generation, y=percent)) + 
  geom_col() + 
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() + 
  labs(x = "Legendary",
       y = "Percent")

pokemon_cor <- pokemon %>% 
  select(total:speed) %>% 
  cor()

pokemon_cor %>% 
  corrplot()
  
ggpairs(pokemon,
        columns = 5:11,
        aes(color = legendary,
            alpha = 0.8)) + 
  theme_minimal()


