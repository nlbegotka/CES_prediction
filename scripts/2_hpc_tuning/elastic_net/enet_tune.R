# Tune Elastic Net model for on Yale cluster
# Last updated: 5/11/2024 by NB 

#-------------------------------------------------------------------------------
# 0. Load objects
#-------------------------------------------------------------------------------
# Libraries
library(tidymodels)
library(tidyverse)
library(glmnet)
library(ggplot2)
library(doParallel) # to run code in parallel
library(foreach)
library(vip)

# Define path to read in data 
up_path <- paste(rep("..", 3), collapse = "/") # dynamic
input_path <- file.path(up_path, "temp")

# Read in data 
load(file.path(input_path, "poll_train.rda")) # data


#==============================================================================#
# 1. Run elastic net model 
#==============================================================================#
#-------------------------------------------------------------------------------
# 2.1. Split data into train and test (for tuning only)
#-------------------------------------------------------------------------------
set.seed(42)
split_county <- initial_split(poll_train, prop = 0.8, strata = "climate_change_worry")
poll_train_tune <- training(split_county)
poll_test_tune  <- testing(split_county)


#-------------------------------------------------------------------------------
# 2.2. Preprocess data 
#-------------------------------------------------------------------------------
dv <- "climate_change_worry" # define dv to be used throughout the rest of the code 
model_formula <- as.formula(paste0(dv, " ~ ."))

# Recipe
enet_recipe <- poll_train_tune %>%
  recipe(model_formula) %>% 
  step_dummy(all_nominal_predictors()) %>% # dummy encode
  step_normalize(all_numeric_predictors()) 


#-------------------------------------------------------------------------------
# 2.3. Specify model
#-------------------------------------------------------------------------------
# Tuning hyperparameters pulled from example in Tidy Modeling with R by Max Kuhn

enet_spec <- logistic_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_mode("classification") %>% 
  set_engine("glmnet")


#-------------------------------------------------------------------------------
# 2.4. Create workflow for modeling
#-------------------------------------------------------------------------------
enet_wflow <- workflow() %>%
  add_recipe(enet_recipe) %>% 
  add_model(enet_spec) 

#-------------------------------------------------------------------------------
# 2.5. Specify a resampling strategy for model tuning -- cross-validation
#-------------------------------------------------------------------------------
set.seed(42)
cv_object <- vfold_cv(poll_train_tune, v = 10, strata = "climate_change_worry", repeats=10)

#-------------------------------------------------------------------------------
# 2.6. Create tuning grid
#-------------------------------------------------------------------------------
# # Check standard ranges for hparams
# enet_param <- extract_parameter_set_dials(enet_spec)
# enet_param$object

# Create grid with narrower parameter range 
set.seed(42)
enet_grid_rand <- grid_latin_hypercube(
  penalty(c(-3, 0)),
  mixture(c(0, 1)),
  size = 500
)


#-------------------------------------------------------------------------------
# 2.7. Tune model 
#-------------------------------------------------------------------------------
# Run in parallel 
ncores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK")) 
cl <- makeCluster(ncores, type="FORK")
registerDoParallel(cl)

# Measure run time
start_time <- proc.time()

enet_tune <- tune_grid(
  enet_wflow,
  resamples = cv_object,
  grid = enet_grid_rand,
  metrics = metric_set(roc_auc), # log_loss is another option 
  control = control_grid(allow_par = TRUE, save_pred = TRUE, parallel_over = 'resamples')
)

end_time <- proc.time()
runtime <- end_time - start_time
runtimehrs <- round(runtime[3]/3600, 1)
paste("Run time county:", runtimehrs, "hours on", ncores, "cores")

#-------------------------------------------------------------------------------
# 2.8. Print results 
#-------------------------------------------------------------------------------
# Check results
tuning_df <- collect_metrics(enet_tune) %>% data.frame()

# Select best parameters
enet_tune_best <- select_best(enet_tune)

# Propagate the best set of parameters into the final workflow
enet_wflow_best <- finalize_workflow(enet_wflow, enet_tune_best)

# Finally fit the final model using the best parameter set on the split created before
enet_wflow_final <- last_fit(enet_wflow_best, split = split_county)

# Find model aucs
train_auc <- show_best(enet_tune, n = 1)$mean
test_auc <- enet_wflow_final$.metrics[[1]]$.estimate[[2]]

# Calc predictor importance
fitted_model <- extract_fit_parsnip(enet_wflow_final)
predictor_importance <- vip::vi(fitted_model)

## Calc a bunch of other metrics
predictions <- collect_predictions(enet_wflow_final)
multi_metric <- yardstick::metric_set(accuracy, yardstick::precision, 
                                      yardstick::recall, f_meas)
metrics <- multi_metric(data=predictions, truth=!!sym(dv), estimate=.pred_class)

# Create confusion matrix
conf_mat <- predictions %>%
  conf_mat(truth = !!sym(dv), estimate = .pred_class)

# Print all metrics
print(train_auc)
print(test_auc)
print(metrics)
print(conf_mat)

# Save output
up_path <- paste(rep("..", 3), collapse = "/") # dynamic
outpath <- "temp/tuning/"
output_path <- file.path(up_path, outpath)

save(tuning_df, enet_tune_best, train_auc, test_auc, predictor_importance, metrics, 
     conf_mat, file=paste0(output_path, "enet_tuning.rda"))


