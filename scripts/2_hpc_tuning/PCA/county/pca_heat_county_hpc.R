# Run glmer model with components from PCA for heat on Grace cluster
# Last updated: 4/11/2024 by NLB

#-------------------------------------------------------------------------------
# 0. Load libraries and train sets 
#-------------------------------------------------------------------------------
rm(list=ls())
library(lme4)
library(tidymodels)
library(tidyverse)
library(pROC) 
library(foreach)
library(doParallel)

# Define path to read in data 
up_path <- paste(rep("..", 4), collapse = "/") # dynamic
poll_path <- "temp/" # dynamic
input_path <- file.path(up_path, poll_path)
# Read in data 
load(paste0(input_path, "poll_county_heat_train.rda")) # data


#==============================================================================#
# 2. Create df with random effects and PCA components for fixed effects 
#==============================================================================#
#-------------------------------------------------------------------------------
# 2.1. Create recipe and process df right before applying PCA 
#-------------------------------------------------------------------------------

dv <- "dv_heat"
model_formula_pca <- as.formula(paste0(dv, " ~ ."))

recipe_base <- poll_county_train %>%
  recipe(model_formula_pca) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% # transform to be more normal
  step_dummy(geoid) %>% 
  step_nzv(all_numeric_predictors(), freq_cut = 99/1) %>%  # remove low variance
  step_normalize(all_numeric_predictors()) %>% # normalize 
  step_lincomb(all_numeric_predictors()) %>% # remove linear combinations
  step_corr(threshold = 0.9)  # remove highly correlated 

poll_county_base <- recipe_base %>% 
  prep() %>%
  bake(poll_county_train)



#-------------------------------------------------------------------------------
# 2.1. Define cross-validation function -- calculates AUC 
#-------------------------------------------------------------------------------
run_cv <- function(df, dv, model_formula, n_folds, seed) {
  
  set.seed(seed)
  folds <- sample(rep(1:n_folds, length.out = nrow(df))) # stratify later!
  auc_model <- numeric(n_folds)
  
  
  for (i in 1:n_folds) {
    # Get indices for train and test data
    test_indices <- which(folds == i)
    train_indices <- which(folds != i)
    
    # Fit model on training data
    fitted_model <- glmer(
      model_formula,
      data = df[train_indices, ],
      family = binomial(link = "logit")
    )
    
    # Predict probabilities on test data
    predicted_probs <- predict(fitted_model, newdata = df[test_indices, ], type = "response")
    
    # Calculate AUC for the current fold
    roc_obj <- roc(df[[dv]][test_indices], predicted_probs)
    auc_model[i] <- auc(roc_obj)
  }
  
  # Calculate mean AUC across folds for this model
  auc_mean <- mean(auc_model, na.rm = TRUE)
  
  return(auc_mean)
}


#-------------------------------------------------------------------------------
# 2.2. Loop through creating different numbers of components 
#-------------------------------------------------------------------------------
# Define global objects 
num_components <- 1:15
n_folds <- 10 
dv <- "dv_heat"

# Set up parallel backend
ncores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))
cl <- makeCluster(ncores, type="FORK")
registerDoParallel(cl)

# Track time
start_time <- proc.time()

# Run parallel loop using for each 
auc_values <- foreach(n_comp = num_components, .combine = "c") %dopar% {
  
  pca_recipe <- poll_county_base %>%
    recipe(model_formula_pca) %>% 
    step_pca(all_numeric_predictors(), num_comp = n_comp)
  
  poll_county_pca <- pca_recipe %>%
    prep() %>%
    bake(poll_county_base) %>% 
    data.frame()
  
  # Create model formula 
  pca_vars <- names(poll_county_pca)[grepl("PC", names(poll_county_pca))]
  model_formula <- as.formula(paste0(dv, " ~ (1|state_geoid) + (1|reg9) + 
                                     (1|sex) + (1|age_group) + (1|race) + 
                                     (1|sex:age_group:race) + ",
                                     paste(pca_vars, collapse="+")))
  
  # Run cross-validation for this n_comp returning auc value
  run_cv(df = poll_county_pca, dv = "dv_heat", 
         model_formula = model_formula, seed = 42, n_folds = n_folds)
  
}

# Print runtime
end_time <- proc.time()
duration <- end_time - start_time
runtimemin <- round(duration[3]/60, 1)
paste("Run time county:", runtimemin, "minutes on 10 folds")


# Print output
print(auc_values)

# Construct the output path to go up and then down directory tree
up_path <- paste(rep("..", 4), collapse = "/")
output_path <- "temp/tuning/"
output_path <- file.path(up_path, output_path)

# Save output
save(auc_values, file=paste0(output_path, "pca_heat_county_tuning.rda"))











