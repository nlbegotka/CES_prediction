# Split data into train and test 
# Load CES_prediction.Rproj
# Last updated: 5/8/2024 by NB 

#-------------------------------------------------------------------------------
# 0. Load packages and poll data
#-------------------------------------------------------------------------------
# Libraries 
rm(list=ls())
library(dplyr)
library(tidymodels)

# Data
load("temp/poll_clean.rda")
load("temp/eda_results.rda")

#-------------------------------------------------------------------------------
# 1. Split data into train and test sets
#    Training set will be used for hyperparameter tuning and final training 
#    Testing set will be used for final model evaluation 
#-------------------------------------------------------------------------------
# Split data into 80-20
set.seed(42)
split <- poll_clean %>% 
  initial_split(prop = 0.8, strata = "climate_change_worry")
poll_train <- training(split)
poll_test <- testing(split)

# # Save
# save(poll_train, file="temp/poll_train.rda")
# save(poll_test, file="temp/poll_test.rda")


#-------------------------------------------------------------------------------
# 2. Apply train/test split pre-processing from EDA
#-------------------------------------------------------------------------------
# Remove weak features
poll_train <- poll_train %>% 
  dplyr::select(-all_of(eda_results$weaker_features))

poll_test <- poll_test %>% 
  dplyr::select(-all_of(eda_results$weaker_features))

# Combine CDC features with PCA
pca_recipe <- poll_train %>% 
  recipe("climate_change_worry ~ .") %>% 
  step_pca(all_of(eda_results$pca_transform), num_comp = 1)

poll_train <- pca_recipe %>% 
  prep() %>% juice() %>% 
  data.frame() %>% 
  rename(cdc_svi_comp = "PC1")
  
poll_test <- pca_recipe %>% 
  prep() %>% bake(poll_test) %>% 
  data.frame() %>% 
  rename(cdc_svi_comp = "PC1")

# Log transform NOAA feature
poll_train$noaa_cost_pc <- log(poll_train$noaa_cost_pc + 0.1)
poll_test$noaa_cost_pc <- log(poll_test$noaa_cost_pc + 0.1)


# Save processed splits
save(poll_train, file="temp/poll_train.rda")
save(poll_test, file="temp/poll_test.rda")


