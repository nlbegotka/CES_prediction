# Test individual model performance and ensemble model performance
# Last updated: 5/11/2024 by NB 

#-------------------------------------------------------------------------------
# 0. Load objects
#-------------------------------------------------------------------------------
# Libraries
rm(list=ls())
library(lme4)
library(tidymodels)
library(tidyverse)
library(xgboost)
library(glmnet)
library(ranger)
library(pROC) 
library(yardstick)
library(Metrics)

# Data
load("temp/poll_train.rda")
load("temp/poll_test.rda")

# Functions
source("scripts/0_functions/00_create_directory.R")

#-------------------------------------------------------------------------------
# 1. Create recipes to pre-process data for models 
#    Many of the recipes are the same but are separated in case we want to make changes later
#-------------------------------------------------------------------------------
# For all models 
dv <- "climate_change_worry" 
recipe_formula <- as.formula(paste0(dv, " ~ ."))

## 1.1. XGBoost recipe ---------------------------------------------------------

xgb_recipe_train <- poll_train %>%
  recipe(recipe_formula) %>% 
  step_dummy(all_nominal_predictors()) %>% # dummy encode 
  step_select(-evangelical_Other) %>% # remove zero-variance
  step_normalize(all_numeric_predictors()) # normalize 

xgb_recipe_test <- poll_test %>%
  recipe(recipe_formula) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_select(-evangelical_Other) %>% # remove zero-variance
  step_normalize(all_numeric_predictors()) 


## 1.2. Random forest recipe ---------------------------------------------------

rf_recipe_train <- poll_train %>%
  recipe(recipe_formula) %>% 
  step_normalize(all_numeric_predictors())  

rf_recipe_test <- poll_test %>%
  recipe(recipe_formula) %>% 
  step_normalize(all_numeric_predictors()) 


## 1.3. Elastic net recipe -----------------------------------------------------
enet_recipe_train <- poll_train %>%
  recipe(recipe_formula) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_select(-evangelical_Other) %>% # remove zero-variance
  step_normalize(all_numeric())  


enet_recipe_test <- poll_test %>%
  recipe(recipe_formula) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_select(-evangelical_Other) %>% # remove zero-variance
  step_normalize(all_numeric())  


## 1.4. PCA recipe -------------------------------------------------------------
# num_comp = the number of optimal components found from tuning 
pca_recipe_train <- poll_train %>%
  recipe(recipe_formula) %>% 
  step_normalize(all_numeric_predictors()) %>%  
  step_pca(all_numeric_predictors(), num_comp = 4) # convert to pca components 

pca_recipe_test <- poll_test %>%
  recipe(recipe_formula) %>% 
  step_normalize(all_numeric_predictors()) %>%
  step_pca(all_numeric_predictors(), num_comp = 4) # convert to pca components 


## 1.5. Best subsets recipe ------------------------------------------------

bestsub_recipe_train <- poll_train %>%
  recipe(recipe_formula) %>% 
  step_normalize(all_numeric_predictors()) 

bestsub_recipe_test <- poll_test %>%
  recipe(recipe_formula) %>% 
  step_normalize(all_numeric_predictors()) 

#-------------------------------------------------------------------------------
# 2. Pre-process data for models 
#-------------------------------------------------------------------------------
## 2.1. XGBoost ----------------------------------------------------------------

xgb_train <- xgb_recipe_train %>%
  prep() %>%
  bake(poll_train)

xgb_test <- xgb_recipe_test %>%
  prep() %>%
  bake(poll_test)

## 2.2. Random forest ----------------------------------------------------------

rf_train <- rf_recipe_train %>%
  prep() %>%
  bake(poll_train)

rf_test <- rf_recipe_test %>%
  prep() %>%
  bake(poll_test)

## 2.3. Elastic net ------------------------------------------------------------

enet_train <- enet_recipe_train %>%
  prep() %>%
  bake(poll_train)

enet_test <- enet_recipe_test %>%
  prep() %>%
  bake(poll_test)

## 2.4. PCA --------------------------------------------------------------------

pca_train <- pca_recipe_train %>%
  prep() %>%
  bake(poll_train)

pca_test <- pca_recipe_test %>%
  prep() %>%
  bake(poll_test)

## 2.5. Best subsets -----------------------------------------------------------

bestsub_train <- bestsub_recipe_train %>%
  prep() %>%
  bake(poll_train)

bestsub_test <- bestsub_recipe_test %>%
  prep() %>%
  bake(poll_test)


#-------------------------------------------------------------------------------
# 3. Train tuned model, run on test data, track predictions and performance
#-------------------------------------------------------------------------------
# 3.0. Create lists to store model performance on test set ---------------------
aucs <- list()
log_losses <- list()

# 3.1. XGBoost ----------------------------------------------------------------
# Load optimal hyperparameters from tuning output
load("temp/tuning/xgb_tuning.rda")

# Define model with optimal hyperparameters
xgb_model <- boost_tree(
  trees = xgb_tune_best$trees, 
  min_n = xgb_tune_best$min_n,
  tree_depth = xgb_tune_best$tree_depth, 
  learn_rate = xgb_tune_best$learn_rate, 
  sample_size = xgb_tune_best$sample_size, 
  stop_iter = xgb_tune_best$stop_iter) %>% 
  set_mode("classification") %>% 
  set_engine("xgboost")

# Define model formula 
dv <- "climate_change_worry" 
xgb_formula <- as.formula(paste0(dv, " ~ ."))

# Fit model to training data
xgb_fit <- xgb_model %>% 
  fit(xgb_formula, data=xgb_train)

# Create train assessment object -- for EBMA only  
xgb_train_pred_class <- predict(xgb_fit, new_data=xgb_train)
xgb_train_pred_prob <- predict(xgb_fit, new_data=xgb_train, type="prob") %>% dplyr::select(.pred_1)
xgb_train_pred_all <- cbind(xgb_train_pred_class, xgb_train_pred_prob, xgb_train$climate_change_worry) 
names(xgb_train_pred_all) <- c("pred_class", "pred_prob", "truth")

# Create test assessment object 
xgb_test_pred_class <- predict(xgb_fit, new_data=xgb_test)
xgb_test_pred_prob <- predict(xgb_fit, new_data=xgb_test, type="prob") %>% dplyr::select(.pred_1)
xgb_test_pred_all <- cbind(xgb_test_pred_class, xgb_test_pred_prob, xgb_test$climate_change_worry) 
names(xgb_test_pred_all) <- c("pred_class", "pred_prob", "truth")

# Assess model performance on test set 
(xgb_auc <- roc(xgb_test_pred_all$truth, xgb_test_pred_all$pred_prob) %>% auc())
(xgb_logloss <- logLoss(as.numeric(xgb_test_pred_all$truth), xgb_test_pred_all$pred_prob))


# Add results to list 
aucs$xgboost <- xgb_auc
log_losses$xgboost <- xgb_logloss

# 3.2. Random forest -----------------------------------------------------------
# Follow same steps as in XGBoost
load("temp/tuning/rf_tuning.rda")

rf_model <- rand_forest(
  trees = rf_tune_best$trees, 
  min_n = rf_tune_best$min_n,
  mtry = rf_tune_best$mtry) %>% 
  set_mode("classification")%>% 
  set_engine("ranger")

dv <- "climate_change_worry" 
rf_formula <- as.formula(paste0(dv, " ~ ."))

rf_fit <- rf_model %>% 
  fit(rf_formula, data=rf_train)

# Create train assessment object -- for EBMA only 
rf_train_pred_class <- predict(rf_fit, new_data=rf_train)
rf_train_pred_prob <- predict(rf_fit, new_data=rf_train, type="prob") %>% dplyr::select(.pred_1)
rf_train_pred_all <- cbind(rf_train_pred_class, rf_train_pred_prob, rf_train$climate_change_worry) 
names(rf_train_pred_all) <- c("pred_class", "pred_prob", "truth")

# Create test assessment object 
rf_test_pred_class <- predict(rf_fit, new_data=rf_test)
rf_test_pred_prob <- predict(rf_fit, new_data=rf_test, type="prob") %>% dplyr::select(.pred_1)
rf_test_pred_all <- cbind(rf_test_pred_class, rf_test_pred_prob, rf_test$climate_change_worry) 
names(rf_test_pred_all) <- c("pred_class", "pred_prob", "truth")

# Assess model performance on test set 
(rf_auc <- roc(rf_test_pred_all$truth, rf_test_pred_all$pred_prob) %>% auc())
(rf_logloss <- logLoss(as.numeric(rf_test_pred_all$truth), rf_test_pred_all$pred_prob))

# Add results to list 
aucs$randforest <- rf_auc
log_losses$randforest <- rf_logloss

# 3.3. Elastic net -------------------------------------------------------------
load("temp/tuning/enet_tuning.rda")

enet_model <- logistic_reg(
  penalty = enet_tune_best$penalty, 
  mixture = enet_tune_best$mixture) %>% 
  set_mode("classification")%>% 
  set_engine("glmnet")

dv <- "climate_change_worry" 
enet_formula <- as.formula(paste0(dv, " ~ ."))

enet_fit <- enet_model %>% 
  fit(enet_formula, data=enet_train)

# Create train assessment object -- for EBMA only  
enet_train_pred_class <- predict(enet_fit, new_data=enet_train)
enet_train_pred_prob <- predict(enet_fit, new_data=enet_train, type="prob") %>% dplyr::select(.pred_1)
enet_train_pred_all <- cbind(enet_train_pred_class, enet_train_pred_prob, enet_train$climate_change_worry) 
names(enet_train_pred_all) <- c("pred_class", "pred_prob", "truth")

# Create test assessment object 
enet_test_pred_class <- predict(enet_fit, new_data=enet_test)
enet_test_pred_prob <- predict(enet_fit, new_data=enet_test, type="prob") %>% dplyr::select(.pred_1)
enet_test_pred_all <- cbind(enet_test_pred_class, enet_test_pred_prob, enet_test$climate_change_worry) 
names(enet_test_pred_all) <- c("pred_class", "pred_prob", "truth")

# Assess model peenetormance on test set 
(enet_auc <- roc(enet_test_pred_all$truth, enet_test_pred_all$pred_prob) %>% auc())
(enet_logloss <- logLoss(as.numeric(enet_test_pred_all$truth), enet_test_pred_all$pred_prob))

# Add results to list 
aucs$elasticnet <- enet_auc
log_losses$elasticnet <- enet_logloss

# 3.4. PCA ---------------------------------------------------------------------
# PCA model with four components performs better than one component which was 
# found to be optimal in tuning script -- investigate later 

load("temp/tuning/pca_tuning.rda")

# Define model formula -- uses mixed model framework unlike XGB, rf, and enet
dv <- "climate_change_worry"
best_components <- c("PC1", "PC2", "PC3", "PC4")
model_formula <- as.formula(paste0(dv, " ~ (1|state_fips) + ",
                                   paste(best_components, collapse="+")))

pca_fit <- glmer(model_formula, data = pca_train, family = binomial(link = "logit"))


# Create train assessment object -- for EBMA only  
pca_train_pred_prob <- predict(pca_fit, pca_train, type="response") %>% as.numeric()
pca_train_pred_class <- ifelse(pca_train_pred_prob > 0.5, 1, 0) %>% as.factor()
pca_train_pred_all <- data.frame(pca_train_pred_class, pca_train_pred_prob, pca_train$climate_change_worry) 
names(pca_train_pred_all) <- c("pred_class", "pred_prob", "truth")

# Create test assessment object 
pca_test_pred_prob <- predict(pca_fit, pca_test, type="response") %>% as.numeric()
pca_test_pred_class <- ifelse(pca_test_pred_prob > 0.5, 1, 0) %>% as.factor()
pca_test_pred_all <- data.frame(pca_test_pred_class, pca_test_pred_prob, pca_test$climate_change_worry) 
names(pca_test_pred_all) <- c("pred_class", "pred_prob", "truth")

# Assess model performance on test set 
(pca_auc <- roc(pca_test_pred_all$truth, pca_test_pred_all$pred_prob) %>% auc())
(pca_logloss <- logLoss(as.numeric(pca_test_pred_all$truth), pca_test_pred_all$pred_prob))

# Add results to list 
aucs$PCA <- pca_auc
log_losses$PCA <- pca_logloss


# 3.5. Best subsets ------------------------------------------------------------
# Load best predictors and associated AUC value
load("temp/tuning/bestsub_tuning.rda")

# Define model formula -- best_predictors is loaded above 
dv <- "climate_change_worry"
model_formula <- as.formula(paste0(dv, " ~ (1|state_fips) + ",
                                   paste(best_features, collapse="+")))

bestsub_fit <- glmer(model_formula, data = bestsub_train, family = binomial(link = "logit"))


# Create train assessment object -- for EBMA only  
bestsub_train_pred_prob <- predict(bestsub_fit, bestsub_train, type="response") %>% as.numeric()
bestsub_train_pred_class <- ifelse(bestsub_train_pred_prob > 0.5, 1, 0) %>% as.factor()
bestsub_train_pred_all <- data.frame(bestsub_train_pred_class, bestsub_train_pred_prob, bestsub_train$climate_change_worry) 
names(bestsub_train_pred_all) <- c("pred_class", "pred_prob", "truth")

# Create test assessment object 
bestsub_test_pred_prob <- predict(bestsub_fit, bestsub_test, type="response") %>% as.numeric()
bestsub_test_pred_class <- ifelse(bestsub_test_pred_prob > 0.5, 1, 0) %>% as.factor()
bestsub_test_pred_all <- data.frame(bestsub_test_pred_class, bestsub_test_pred_prob, bestsub_test$climate_change_worry) 
names(bestsub_test_pred_all) <- c("pred_class", "pred_prob", "truth")

# Assess model performance on test set 
(bestsub_auc <- roc(bestsub_test_pred_all$truth, bestsub_test_pred_all$pred_prob) %>% auc())
(bestsub_logloss <- logLoss(as.numeric(bestsub_test_pred_all$truth), bestsub_test_pred_all$pred_prob))

# Add results to list 
aucs$bestsub <- bestsub_auc
log_losses$bestsub <- bestsub_logloss

#-------------------------------------------------------------------------------
# 4. Manually ensemble 
#-------------------------------------------------------------------------------
# 4.1. Averaging all probabilities 

avg_test_pred_prob <- (xgb_test_pred_all$pred_prob + rf_test_pred_all$pred_prob +
                         enet_test_pred_all$pred_prob + pca_test_pred_all$pred_prob + 
                         bestsub_test_pred_all$pred_prob) / 5
avg_test_pred_class <- ifelse(avg_test_pred_prob > 0.5, 1, 0) %>% as.factor()
avg_test_pred_all <- data.frame(avg_test_pred_class, avg_test_pred_prob, bestsub_test$climate_change_worry) 
names(avg_test_pred_all) <- c("pred_class", "pred_prob", "truth")

# Assess avg model performance 
(avg_auc <- roc(avg_test_pred_all$truth, avg_test_pred_all$pred_prob) %>% auc())
(avg_logloss <- logLoss(as.numeric(avg_test_pred_all$truth), avg_test_pred_all$pred_prob))

# Add results to list -- AUC is slightly worse than just XGBoost model 
aucs$average <- avg_auc
log_losses$average <- avg_logloss


# 4.2. Constructing weighted averaging probabilities based on auc performance
auc_scores_vec  <- c(aucs$xgboost, aucs$randforest, aucs$elasticnet, aucs$PCA, aucs$bestsub)
ranked_indices <- order(auc_scores_vec, decreasing = TRUE)
weights <- 1 / (ranked_indices + 1)
weights <- weights / sum(weights)

# Apply weights 
avg_test_pred_prob_weighted <- 
  xgb_test_pred_all$pred_prob * weights[1] + 
  rf_test_pred_all$pred_prob * weights[2] + 
  enet_test_pred_all$pred_prob * weights[3] + 
  pca_test_pred_all$pred_prob * weights[4] + 
  bestsub_test_pred_all$pred_prob * weights[5]

avg_test_pred_class_weighted <- ifelse(avg_test_pred_prob_weighted > 0.5, 1, 0) %>% as.factor()
avg_test_pred_weighted <- data.frame(avg_test_pred_class_weighted, avg_test_pred_prob_weighted, bestsub_test$climate_change_worry) 
names(avg_test_pred_weighted) <- c("pred_class", "pred_prob", "truth")


# Assess avg model performance 
(avg_auc_weighted <- roc(avg_test_pred_weighted$truth, avg_test_pred_weighted$pred_prob) %>% auc())

# Add results to list -- AUC is better than all other models 
aucs$average_weighted <- avg_auc_weighted



# 4.3. Averaging weighted top 3 probabilities based on auc performance
auc_scores_vec  <- c(aucs$xgboost, aucs$randforest, aucs$elasticnet)
ranked_indices <- order(auc_scores_vec, decreasing = TRUE)

weights <- 1 / (ranked_indices + 1)
weights <- weights / sum(weights)

# Apply weights 
avg_test_pred_prob_weighted_top3 <- 
  xgb_test_pred_all$pred_prob * weights[1] + 
  rf_test_pred_all$pred_prob * weights[2] + 
  enet_test_pred_all$pred_prob * weights[3]

avg_test_pred_class_weighted_top3 <- ifelse(avg_test_pred_prob_weighted_top3 > 0.5, 1, 0) %>% as.factor()
avg_test_pred_weighted_top3 <- data.frame(avg_test_pred_class_weighted_top3, avg_test_pred_prob_weighted_top3, bestsub_test$climate_change_worry) 
names(avg_test_pred_weighted_top3) <- c("pred_class", "pred_prob", "truth")


# Assess avg model performance 
(avg_auc_weighted_top3 <- roc(avg_test_pred_weighted_top3$truth, avg_test_pred_weighted_top3$pred_prob) %>% auc())

# Add results to list -- AUC is the same 
aucs$average_weighted_top3 <- avg_auc_weighted_top3


#-------------------------------------------------------------------------------
# 5. Save AUCs to evaluate later
#-------------------------------------------------------------------------------
outpath <- "output/3_aucs"
create_directory(outpath)
save(aucs, file=file.path(outpath, "auc_assessment.rda"))



