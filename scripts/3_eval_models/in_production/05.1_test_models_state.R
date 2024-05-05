# Ensemble various models to improve performance with averaging and EBMA 
# Load ypccc_hazards.Rproj!
# Last updated: 4/10/2024 by NLB 

#-------------------------------------------------------------------------------
# 0.0 Load packages 
#-------------------------------------------------------------------------------
rm(list=ls())
library(lme4)
library(tidymodels)
library(tidyverse)
library(xgboost)
library(glmnet)
library(ranger)
library(pROC) 
library(yardstick)

#-------------------------------------------------------------------------------
# 0.1 Load test data and output from component models  
#-------------------------------------------------------------------------------
load("downscale/heat/temp/poll_state_heat_train.rda")
load("downscale/heat/temp/poll_state_heat_test.rda")

#-------------------------------------------------------------------------------
# 0.2 Clean poll by removing state name -- we are only modeling in this script
#-------------------------------------------------------------------------------

poll_state_train <- poll_state_train %>% 
  dplyr::select(-state)
poll_state_test <- poll_state_test %>% 
  dplyr::select(-state)

#-------------------------------------------------------------------------------
# 1. Create recipes to pre-process data for models 
#-------------------------------------------------------------------------------
## Note: because there are different counties present in the train and test dfs 
## we add an extra step where we find which counties overlap 
## (after removing dummy-encoded low-variance counties)

## 1.0. Specify formula for all recipes  ---------------------------------------

dv <- "dv_heat" 
recipe_formula <- as.formula(paste0(dv, " ~ ."))

## 1.1. XGBoost recipe ---------------------------------------------------------
## poll_state_train is train set and poll_emba_state is test set 

# Define variable selection recipes for train and test sets 
xgb_recipe_train_temp <- poll_state_train %>%
  recipe(recipe_formula) %>% 
  step_dummy(all_nominal_predictors()) %>% # dummy encode
  step_nzv(all_predictors(), freq_cut = 99/1) %>%  # var select 
  step_lincomb(all_numeric()) %>% # var select 
  step_corr(threshold = 0.9) 

xgb_recipe_test_temp <- poll_state_test %>%
  recipe(recipe_formula) %>% 
  step_dummy(all_nominal_predictors()) %>% # dummy encode
  step_nzv(all_predictors(), freq_cut = 99/1) %>%  # var select 
  #step_lincomb(all_numeric()) %>% # var select 
  step_corr(threshold = 0.9) 

# Apply the recipes to pre-process the train and test sets 
xgb_train_temp <- xgb_recipe_train_temp %>%
  prep() %>%
  bake(poll_state_train)

xgb_test_temp <- xgb_recipe_test_temp %>%
  prep() %>%
  bake(poll_state_test)

# Find which states are present in the train and test sets after pre-processing 
overlapping_vars <- intersect(names(xgb_train_temp), names(xgb_test_temp))
overlapping_states <- overlapping_vars[grepl("geoid_", overlapping_vars)]

# Create final recipes to remove all states except for the overlapping ones 
xgb_recipe_train <- poll_county_train %>%
  recipe(recipe_formula) %>% 
  step_nzv(all_predictors(), freq_cut = 99/1) %>% 
  step_lincomb(all_numeric()) %>% # var select 
  step_corr(threshold = 0.9) %>%  # var select 
  step_dummy(all_nominal_predictors()) %>% # dummy encode 
  step_select(-starts_with("geoid_") | starts_with("state_geoid_") | all_of(overlapping_states)) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% # transform
  step_normalize(all_numeric())  # scale 


xgb_recipe_test <- poll_county_test %>%
  recipe(recipe_formula) %>% 
  step_nzv(all_predictors(), freq_cut = 99/1) %>%  # var select 
  step_lincomb(all_numeric()) %>% # var select 
  step_corr(threshold = 0.9) %>%  # var select 
  step_dummy(all_nominal_predictors()) %>% # dummy encode 
  step_select(-starts_with("geoid_") | starts_with("state_geoid_") | all_of(overlapping_states)) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% # transform
  step_normalize(all_numeric())  # scale 

rm(xgb_train_temp)
rm(xgb_test_temp)
rm(xgb_recipe_train_temp)
rm(xgb_recipe_test_temp)

## 1.2. Random forest recipe ---------------------------------------------------
## Follow the same steps as in XGBoost 
## Because we now know which counties are overlapping, we can skip that step 

rf_recipe_train <- poll_state_train %>%
  recipe(recipe_formula) %>% 
  step_nzv(all_predictors(), freq_cut = 99/1) %>%  # remove low-variance predictors
  step_lincomb(all_numeric_predictors()) %>% # remove linear combinations
  step_corr(threshold = 0.9) %>% 
  step_dummy(geoid) %>% 
  step_select(-contains("geoid_") | all_of(overlapping_states)) %>%
  step_YeoJohnson(all_numeric_predictors()) %>% # transforms data to be more normal
  step_normalize(all_numeric_predictors())  # normalize predictors

rf_recipe_test <- poll_state_test %>%
  recipe(recipe_formula) %>% 
  step_nzv(all_predictors(), freq_cut = 99/1) %>%  # remove low-variance predictors
  step_lincomb(all_numeric_predictors()) %>% # remove linear combinations
  step_corr(threshold = 0.9) %>% 
  step_dummy(geoid) %>% 
  step_select(-contains("geoid_") | all_of(overlapping_states)) %>%
  step_YeoJohnson(all_numeric_predictors()) %>% # transforms data to be more normal
  step_normalize(all_numeric_predictors())  # normalize predictors


## 1.3. Elastic net recipe -----------------------------------------------------
## Note: recipe is identical to XGBoost recipe
enet_recipe_train <- poll_state_train %>%
  recipe(recipe_formula) %>% 
  step_nzv(all_predictors(), freq_cut = 99/1) %>% 
  step_lincomb(all_numeric()) %>% # var select 
  step_corr(threshold = 0.9) %>%  # var select 
  step_dummy(all_nominal_predictors()) %>% # dummy encode 
  step_select(-starts_with("geoid_") | starts_with("state_geoid_") | all_of(overlapping_states)) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% # transform
  step_normalize(all_numeric())  # scale 


enet_recipe_test <- poll_state_test %>%
  recipe(recipe_formula) %>% 
  step_nzv(all_predictors(), freq_cut = 99/1) %>%  # var select 
  step_lincomb(all_numeric()) %>% # var select 
  step_corr(threshold = 0.9) %>%  # var select 
  step_dummy(all_nominal_predictors()) %>% # dummy encode 
  step_select(-starts_with("geoid_") | starts_with("state_geoid_") | all_of(overlapping_states)) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% # transform
  step_normalize(all_numeric())  # scale 


## 1.4. PCA recipe -------------------------------------------------------------

pca_recipe_train <- poll_state_train %>%
  recipe(recipe_formula) %>% 
  step_nzv(all_numeric_predictors(), freq_cut = 99/1) %>%  # remove low variance
  step_lincomb(all_numeric_predictors()) %>% # remove linear combinations
  step_corr(threshold = 0.9) %>% # remove highly correlated 
  step_dummy(geoid) %>%
  step_select(-contains("geoid_") | all_of(overlapping_states)) %>%
  step_YeoJohnson(all_numeric_predictors()) %>% # transform to be more normal
  step_normalize(all_numeric_predictors()) %>% # normalize 
  step_pca(all_numeric_predictors(), num_comp = 15) # convert to pca components 

pca_recipe_test <- poll_state_test %>%
  recipe(recipe_formula) %>% 
  step_nzv(all_numeric_predictors(), freq_cut = 99/1) %>%  # remove low variance
  step_lincomb(all_numeric_predictors()) %>% # remove linear combinations
  step_corr(threshold = 0.9) %>% # remove highly correlated 
  step_dummy(geoid) %>%
  step_select(-contains("geoid_") | all_of(overlapping_states)) %>%
  step_YeoJohnson(all_numeric_predictors()) %>% # transform to be more normal
  step_normalize(all_numeric_predictors()) %>% # normalize 
  step_pca(all_numeric_predictors(), num_comp = 15) # convert to pca components 


## 1.5. Best subsets recipe ------------------------------------------------

bestsub_recipe_train <- poll_state_train %>%
  recipe(recipe_formula) %>% 
  step_nzv(all_numeric_predictors(), freq_cut = 99/1) %>%  # remove low variance
  step_lincomb(all_numeric_predictors()) %>% # remove linear combinations
  step_corr(threshold = 0.9) %>%  # remove highly correlated 
  step_dummy(geoid) %>% 
  step_select(-contains("geoid_") | all_of(overlapping_states)) %>%
  step_YeoJohnson(all_numeric_predictors()) %>% # transform to be more normal
  step_normalize(all_numeric_predictors()) # normalize 

bestsub_recipe_test <- poll_state_test %>%
  recipe(recipe_formula) %>% 
  step_nzv(all_numeric_predictors(), freq_cut = 99/1) %>%  # remove low variance
  step_lincomb(all_numeric_predictors()) %>% # remove linear combinations
  step_corr(threshold = 0.9) %>%  # remove highly correlated 
  step_dummy(geoid) %>% 
  step_select(-contains("geoid_") | all_of(overlapping_states)) %>%
  step_YeoJohnson(all_numeric_predictors()) %>% # transform to be more normal
  step_normalize(all_numeric_predictors()) # normalize 



#-------------------------------------------------------------------------------
# 2. Pre-process data for models 
#-------------------------------------------------------------------------------
## 2.1. XGBoost ----------------------------------------------------------------

xgb_train <- xgb_recipe_train %>%
  prep() %>%
  bake(poll_state_train)

xgb_test <- xgb_recipe_test %>%
  prep() %>%
  bake(poll_state_test)

## 2.2. Random forest ----------------------------------------------------------

rf_train <- rf_recipe_train %>%
  prep() %>%
  bake(poll_state_train)

rf_test <- rf_recipe_test %>%
  prep() %>%
  bake(poll_state_test)

## 2.3. Elastic net ------------------------------------------------------------

enet_train <- enet_recipe_train %>%
  prep() %>%
  bake(poll_state_train)

enet_test <- enet_recipe_test %>%
  prep() %>%
  bake(poll_state_test)

## 2.4. PCA --------------------------------------------------------------------

pca_train <- pca_recipe_train %>%
  prep() %>%
  bake(poll_state_train)

pca_test <- pca_recipe_test %>%
  prep() %>%
  bake(poll_state_test)

## 2.5. Best subsets -----------------------------------------------------------

bestsub_train <- bestsub_recipe_train %>%
  prep() %>%
  bake(poll_state_train)

bestsub_test <- bestsub_recipe_test %>%
  prep() %>%
  bake(poll_state_test)


#-------------------------------------------------------------------------------
# 3. Train tuned model, run on test data, track predictions and performance
#-------------------------------------------------------------------------------

# 3.0. Create lists to store model performance on test set ---------------------
aucs <- list()
f1_scores <- list()

# 3.1. XGBoost ----------------------------------------------------------------
# Load optimal hyperparameters from tuning output
load("downscale/heat/temp/tuning/xgb_heat_state_tuning.rda")

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
dv <- "dv_heat" 
xgb_formula <- as.formula(paste0(dv, " ~ ."))

# Fit model to training data
xgb_fit <- xgb_model %>% 
  fit(xgb_formula, data=xgb_train)

# Create train assessment object -- for EBMA only  
xgb_train_pred_class <- predict(xgb_fit, new_data=xgb_train)
xgb_train_pred_prob <- predict(xgb_fit, new_data=xgb_train, type="prob") %>% dplyr::select(.pred_1)
xgb_train_pred_all <- cbind(xgb_train_pred_class, xgb_train_pred_prob, xgb_train$dv_heat) 
names(xgb_train_pred_all) <- c("pred_class", "pred_prob", "truth")

# Create test assessment object 
xgb_test_pred_class <- predict(xgb_fit, new_data=xgb_test)
xgb_test_pred_prob <- predict(xgb_fit, new_data=xgb_test, type="prob") %>% dplyr::select(.pred_1)
xgb_test_pred_all <- cbind(xgb_test_pred_class, xgb_test_pred_prob, xgb_test$dv_heat) 
names(xgb_test_pred_all) <- c("pred_class", "pred_prob", "truth")

# Assess model performance on test set 
(xgb_auc <- roc(xgb_test_pred_all$truth, xgb_test_pred_all$pred_prob) %>% auc())
(xgb_f1 <- xgb_test_pred_all %>% f_meas(truth, pred_class))

# Add results to list 
aucs$xgboost <- xgb_auc
f1_scores$xgboost <- xgb_f1


# 3.2. Random forest -----------------------------------------------------------
# Follow same steps as in XGBoost
load("downscale/heat/temp/tuning/rf_heat_state_tuning.rda")

rf_model <- rand_forest(
  trees = rf_tune_best$trees, 
  min_n = rf_tune_best$min_n,
  mtry = rf_tune_best$mtry) %>% 
  set_mode("classification")%>% 
  set_engine("ranger")

dv <- "dv_heat" 
rf_formula <- as.formula(paste0(dv, " ~ ."))

rf_fit <- rf_model %>% 
  fit(rf_formula, data=rf_train)

# Create train assessment object -- for EBMA only 
rf_train_pred_class <- predict(rf_fit, new_data=rf_train)
rf_train_pred_prob <- predict(rf_fit, new_data=rf_train, type="prob") %>% dplyr::select(.pred_1)
rf_train_pred_all <- cbind(rf_train_pred_class, rf_train_pred_prob, rf_train$dv_heat) 
names(rf_train_pred_all) <- c("pred_class", "pred_prob", "truth")

# Create test assessment object 
rf_test_pred_class <- predict(rf_fit, new_data=rf_test)
rf_test_pred_prob <- predict(rf_fit, new_data=rf_test, type="prob") %>% dplyr::select(.pred_1)
rf_test_pred_all <- cbind(rf_test_pred_class, rf_test_pred_prob, rf_test$dv_heat) 
names(rf_test_pred_all) <- c("pred_class", "pred_prob", "truth")

# Assess model performance on test set 
(rf_auc <- roc(rf_test_pred_all$truth, rf_test_pred_all$pred_prob) %>% auc())
(rf_f1 <- rf_test_pred_all %>% f_meas(truth, pred_class))

# Add results to list 
aucs$randforest <- rf_auc
f1_scores$randforest <- rf_f1


# 3.3. Elastic net -------------------------------------------------------------
load("downscale/heat/temp/tuning/enet_heat_state_tuning.rda")

enet_model <- logistic_reg(
  penalty = enet_tune_best$penalty, 
  mixture = enet_tune_best$mixture) %>% 
  set_mode("classification")%>% 
  set_engine("glmnet")

dv <- "dv_heat" 
enet_formula <- as.formula(paste0(dv, " ~ ."))

enet_fit <- enet_model %>% 
  fit(enet_formula, data=enet_train)

# Create train assessment object -- for EBMA only  
enet_train_pred_class <- predict(enet_fit, new_data=enet_train)
enet_train_pred_prob <- predict(enet_fit, new_data=enet_train, type="prob") %>% dplyr::select(.pred_1)
enet_train_pred_all <- cbind(enet_train_pred_class, enet_train_pred_prob, enet_train$dv_heat) 
names(enet_train_pred_all) <- c("pred_class", "pred_prob", "truth")

# Create test assessment object 
enet_test_pred_class <- predict(enet_fit, new_data=enet_test)
enet_test_pred_prob <- predict(enet_fit, new_data=enet_test, type="prob") %>% dplyr::select(.pred_1)
enet_test_pred_all <- cbind(enet_test_pred_class, enet_test_pred_prob, enet_test$dv_heat) 
names(enet_test_pred_all) <- c("pred_class", "pred_prob", "truth")

# Assess model peenetormance on test set 
(enet_auc <- roc(enet_test_pred_all$truth, enet_test_pred_all$pred_prob) %>% auc())
(enet_f1 <- enet_test_pred_all %>% f_meas(truth, pred_class))

# Add results to list 
aucs$elasticnet <- enet_auc
f1_scores$elasticnet <- enet_f1


# 3.4. PCA ---------------------------------------------------------------------
# PCA model with four components performs better than one component which was 
# found to be optimal in tuning script -- investigate later 

load("downscale/heat/temp/tuning/pca_heat_state_tuning.rda")

# Define model formula -- uses mixed model framework unlike XGB, rf, and enet
dv <- "dv_heat"
best_components <- c("PC01", "PC02", "PC03", "PC04")
model_formula <- as.formula(paste0(dv, " ~ (1|state_geoid) + (1|reg9) + 
                                     (1|sex) + (1|age_group) + (1|race) + 
                                     (1|sex:age_group:race) + ",
                                   paste(best_components, collapse="+")))

pca_fit <- glmer(model_formula, data = pca_train, family = binomial(link = "logit"))


# Create train assessment object -- for EBMA only  
pca_train_pred_prob <- predict(pca_fit, pca_train, type="response") %>% as.numeric()
pca_train_pred_class <- ifelse(pca_train_pred_prob > 0.5, 1, 0) %>% as.factor()
pca_train_pred_all <- data.frame(pca_train_pred_class, pca_train_pred_prob, pca_train$dv_heat) 
names(pca_train_pred_all) <- c("pred_class", "pred_prob", "truth")

# Create test assessment object 
pca_test_pred_prob <- predict(pca_fit, pca_test, type="response") %>% as.numeric()
pca_test_pred_class <- ifelse(pca_test_pred_prob > 0.5, 1, 0) %>% as.factor()
pca_test_pred_all <- data.frame(pca_test_pred_class, pca_test_pred_prob, pca_test$dv_heat) 
names(pca_test_pred_all) <- c("pred_class", "pred_prob", "truth")

# Assess model performance on test set 
(pca_auc <- roc(pca_test_pred_all$truth, pca_test_pred_all$pred_prob) %>% auc())
(pca_f1 <- pca_test_pred_all %>% f_meas(truth, pred_class))

# Add results to list 
aucs$PCA <- pca_auc
f1_scores$PCA <- pca_f1



# 3.5. Best subsets ------------------------------------------------------------
# Load best predictors and associated AUC value
load("downscale/heat/temp/tuning/bestsub_heat_state_tuning.rda")

# Define model formula -- best_predictors is loaded above 
dv <- "dv_heat"
model_formula <- as.formula(paste0(dv, " ~ (1|state_geoid) + (1|reg9) + 
                                     (1|sex) + (1|age_group) + (1|race) + 
                                     (1|sex:age_group:race) + ",
                                   paste(best_predictors, collapse="+")))

bestsub_fit <- glmer(model_formula, data = bestsub_train, family = binomial(link = "logit"))


# Create train assessment object -- for EBMA only  
bestsub_train_pred_prob <- predict(bestsub_fit, bestsub_train, type="response") %>% as.numeric()
bestsub_train_pred_class <- ifelse(bestsub_train_pred_prob > 0.5, 1, 0) %>% as.factor()
bestsub_train_pred_all <- data.frame(bestsub_train_pred_class, bestsub_train_pred_prob, bestsub_train$dv_heat) 
names(bestsub_train_pred_all) <- c("pred_class", "pred_prob", "truth")

# Create test assessment object 
bestsub_test_pred_prob <- predict(bestsub_fit, bestsub_test, type="response") %>% as.numeric()
bestsub_test_pred_class <- ifelse(bestsub_test_pred_prob > 0.5, 1, 0) %>% as.factor()
bestsub_test_pred_all <- data.frame(bestsub_test_pred_class, bestsub_test_pred_prob, bestsub_test$dv_heat) 
names(bestsub_test_pred_all) <- c("pred_class", "pred_prob", "truth")

# Assess model performance on test set 
(bestsub_auc <- roc(bestsub_test_pred_all$truth, bestsub_test_pred_all$pred_prob) %>% auc())
(bestsub_f1 <- bestsub_test_pred_all %>% f_meas(truth, pred_class))

# Add results to list 
aucs$bestsub <- bestsub_auc
f1_scores$bestsub <- bestsub_f1

#-------------------------------------------------------------------------------
# 4. Manually ensemble 
#-------------------------------------------------------------------------------
# 4.1. Averaging all probabilities 

avg_test_pred_prob <- (xgb_test_pred_all$pred_prob + rf_test_pred_all$pred_prob +
                         enet_test_pred_all$pred_prob + pca_test_pred_all$pred_prob + 
                         bestsub_test_pred_all$pred_prob) / 5
avg_test_pred_class <- ifelse(avg_test_pred_prob > 0.5, 1, 0) %>% as.factor()
avg_test_pred_all <- data.frame(avg_test_pred_class, avg_test_pred_prob, bestsub_test$dv_heat) 
names(avg_test_pred_all) <- c("pred_class", "pred_prob", "truth")

# Assess avg model performance 
(avg_auc <- roc(avg_test_pred_all$truth, avg_test_pred_all$pred_prob) %>% auc())
(avg_f1 <- avg_test_pred_all %>% f_meas(truth, pred_class))

# Add results to list -- AUC is slightly worse than just XGBoost model 
aucs$average <- avg_auc
f1_scores$average <- avg_f1



# 4.2. Constructing weighted averaging probabilities 
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
avg_test_pred_weighted <- data.frame(avg_test_pred_class_weighted, avg_test_pred_prob_weighted, bestsub_test$dv_heat) 
names(avg_test_pred_weighted) <- c("pred_class", "pred_prob", "truth")


# Assess avg model performance 
(avg_auc_weighted <- roc(avg_test_pred_weighted$truth, avg_test_pred_weighted$pred_prob) %>% auc())
(avg_f1_weighted <- avg_test_pred_weighted %>% f_meas(truth, pred_class))

# Add results to list -- AUC is slightly worse than just XGBoost model 
aucs$average_weighted <- avg_auc_weighted
f1_scores$average_weighted <- avg_f1_weighted



# 4.3. Averaging weighted top 3 probabilities 
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
avg_test_pred_weighted_top3 <- data.frame(avg_test_pred_class_weighted_top3, avg_test_pred_prob_weighted_top3, bestsub_test$dv_heat) 
names(avg_test_pred_weighted_top3) <- c("pred_class", "pred_prob", "truth")


# Assess avg model performance 
(avg_auc_weighted_top3 <- roc(avg_test_pred_weighted_top3$truth, avg_test_pred_weighted_top3$pred_prob) %>% auc())
(avg_f1_weighted_top3 <- avg_test_pred_weighted_top3 %>% f_meas(truth, pred_class))

# Add results to list -- AUC is slightly worse than just XGBoost model 
aucs$average_weighted_top3 <- avg_auc_weighted_top3
f1_scores$average_weighted_top3 <- avg_f1_weighted_top3


# 4.4. Class voting -- only applies to F1 score --------------------------------
class_voting_df <- data.frame(xgb_test_pred_all$pred_class, rf_test_pred_all$pred_class, 
                              enet_test_pred_all$pred_class, pca_test_pred_all$pred_class, 
                              bestsub_test_pred_all$pred_class)
names(class_voting_df) <- c("xgboost", "randforest", "elasticnet", "PCA", "bestsub")

class_voting_df$majority <- apply(class_voting_df, 1, function(row) {
  table_row <- table(row)
  mode_value <- as.numeric(names(table_row)[which.max(table_row)])
  return(mode_value)
})

class_voting_df <- class_voting_df %>% 
  mutate(majority = as.factor(majority), 
         truth = as.factor(bestsub_test$dv_heat))

(voting_f1 <- class_voting_df %>% f_meas(truth, majority))
f1_scores$voting <- voting_f1


#-------------------------------------------------------------------------------
# 5. Save AUCs to evaluate later
#-------------------------------------------------------------------------------
save(aucs, file= "downscale/heat/temp/aucs/final_auc_assessment_state.rda")





