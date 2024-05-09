# Tune XGBoost model for on Yale cluster
# Last updated: 5/11/2024 by NB 

#-------------------------------------------------------------------------------
# 0. Load objects
#-------------------------------------------------------------------------------
# Libraries
library(tidymodels)
library(tidyverse)
library(xgboost)
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
# 2. Run XGBoost model 
#==============================================================================#
#-------------------------------------------------------------------------------
# 2.1. Split data into train and test (for tuning only)
#-------------------------------------------------------------------------------
set.seed(42)
split_county <- initial_split(poll_county_train, prop = 0.8, strata = "state_fips")
poll_train_tune <- training(split_county)
poll_test_tune  <- testing(split_county)


#-------------------------------------------------------------------------------
# 2.2. Preprocess data (recipe)
#-------------------------------------------------------------------------------
dv <- "climate_change_worry" # define dv to be used throughout the rest of the code 
model_formula <- as.formula(paste0(dv, " ~ ."))

# Recipe
xgb_recipe <- poll_train_tune %>%
  recipe(model_formula) %>% 
  step_dummy(all_nominal_predictors()) %>% # dummy encode
  step_normalize(all_numeric_predictors()) 

#-------------------------------------------------------------------------------
# 2.3. Create XGBoost model specification (engine) 
#-------------------------------------------------------------------------------
# Tuning hyperparameters pulled from example in Tidy Modeling with R by Max Kuhn
xgb_spec <- boost_tree(
  trees = tune(),
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(), 
  # loss_reduction = tune(), 
  sample_size = tune(), 
  stop_iter = tune()
) %>%
  set_mode("classification") %>% 
  set_engine("xgboost")


#-------------------------------------------------------------------------------
# 2.4. Create workflow for modeling
#-------------------------------------------------------------------------------
xgb_wflow <- workflow() %>%
  add_recipe(xgb_recipe) %>% 
  add_model(xgb_spec) 

#-------------------------------------------------------------------------------
# 2.5. Specify a resampling strategy for model tuning -- cross-validation
#-------------------------------------------------------------------------------
cv_obj <- vfold_cv(poll_train_tune, v = 10, strata = "state_fips", repeats=10)

#-------------------------------------------------------------------------------
# 2.6. Create tuning grid
#-------------------------------------------------------------------------------
# # Check standard ranges for hparams
# xgb_param <- extract_parameter_set_dials(xgb_spec)
# xgb_param$object

# Create grid with narrower parameter range 
set.seed(42)
xgb_grid_rand <- grid_latin_hypercube(
  trees(c(1000, 2000)),
  tree_depth(c(3, 10)),
  min_n(c(8, 18)),
  learn_rate(c(-3, -1)),
  # loss_reduction(c(-10, 1.5)),
  sample_size = sample_prop(c(0.2, 0.9)), # sample_prop allows us to set range 
  stop_iter(c(5, 25)),
  size = 500
)


#-------------------------------------------------------------------------------
# 2.7. Tune model 
#-------------------------------------------------------------------------------
# Run in parallel 
ncores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))
cl <- makeCluster(ncores, type="FORK")
registerDoParallel(cores=ncores)

# Measure run time
start_time <- proc.time()

xgb_tune <- tune_grid(
  xgb_wflow,
  resamples = cv_obj,
  grid = xgb_grid_rand,
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
tuning_df <- collect_metrics(xgb_tune) %>% data.frame()

# Select best parameters
xgb_tune_best <- select_best(xgb_tune)

# Propagate the best set of parameters into the final workflow
xgb_wflow_best <- finalize_workflow(xgb_wflow, xgb_tune_best)

# Finally fit the final model using the best parameter set on the split created before
xgb_wflow_final <- last_fit(xgb_wflow_best, split = split_county)

# Find model aucs
train_auc <- show_best(xgb_tune, n = 1)$mean
test_auc <- xgb_wflow_final$.metrics[[1]]$.estimate[[2]]

# Calc predictor importance
fitted_model <- extract_fit_parsnip(xgb_wflow_final)
predictor_importance <- vip::vi(fitted_model)

## Calc a bunch of other metrics
predictions <- collect_predictions(xgb_wflow_final)
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

# Construct the output path to go up and then down directory tree
up_path <- paste(rep("..", 3), collapse = "/")
output_path <- "temp/tuning/"
output_path <- file.path(up_path, output_path)

# Save output
save(tuning_df, xgb_tune_best, train_auc, test_auc, predictor_importance, metrics, 
     conf_mat, file=paste0(output_path, "xgb_tuning.rda"))


