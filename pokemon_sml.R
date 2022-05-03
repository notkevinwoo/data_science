# libraries and data ------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(corrplot)
library(janitor)

pokemon <- read_csv("data sets/Pokemon.csv") %>% 
  clean_names() %>% 
  mutate(legendary = factor(legendary, levels = c(TRUE, FALSE)))

# eda ---------------------------------------------------------------------
pokemon %>% 
  glimpse()

pokemon %>% 
  count(legendary) %>% 
  mutate(percent = n/sum(n))

pokemon %>% 
  count(generation)

pokemon %>% 
  summary()

pokemon %>% 
  ggplot(aes(x = attack, y = defense, color = legendary)) + 
  geom_point() + 
  theme_minimal()

pokemon_cor <- pokemon %>% 
  select(hp:speed) %>% 
  cor()

pokemon_cor %>% 
  corrplot(., method = "circle", type = "upper", order = "hclust")

# data split --------------------------------------------------------------
set.seed(13)

pokemon_split <- initial_split(pokemon, strata = legendary)

pokemon_train <- pokemon_split %>% 
  training()

pokemon_test <- pokemon_split %>% 
  testing()

# initial test ------------------------------------------------------------
pokemon_logistic_reg <- logistic_reg() %>% 
  fit(legendary ~ total + hp + attack + defense + sp_atk + sp_def + speed,
      pokemon_train)

pokemon_train_pred_prob <- predict(pokemon_logistic_reg, 
                                   pokemon_train, 
                                   type = "prob")

pokemon_train_pred_class <- predict(pokemon_logistic_reg, 
                                    pokemon_train, 
                                    type = "class")

pokemon_train_results <- pokemon_train %>% 
  select(number, name, legendary) %>% 
  bind_cols(pokemon_train_pred_class, 
            pokemon_train_pred_prob)

conf_mat(pokemon_train_results,
         truth = legendary,
         estimate = .pred_class)

pokemon_metrics <- metric_set(accuracy, sens, spec)

pokemon_metrics(pokemon_train_results,
                truth = legendary,
                estimate = .pred_class)

pokemon_roc <- pokemon_train_results %>% 
  roc_curve(legendary, .pred_TRUE) %>% 
  autoplot()

roc_auc(pokemon_train_results,
        truth = legendary,
        estimate = .pred_TRUE)

# feature engineering -----------------------------------------------------
pokemon_rec <- recipe(legendary ~ .,
                      data = pokemon_train) %>% 
  update_role(number, name, generation, new_role = "ID") %>% 
  step_mutate(type_2 = if_else(is.na(type_2), type_1, type_2)) %>% 
  step_corr(all_numeric_predictors(), threshold = .8) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors())
