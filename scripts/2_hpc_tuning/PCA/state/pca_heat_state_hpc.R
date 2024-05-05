# Run glmer model with components from PCA for heat on Grace cluster
# Last updated: 2/13/2024 by NLB

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
load(paste0(input_path, "poll_county_heat.rda")) # data


#==============================================================================#
# 2. Create df with random effects and PCA components for fixed effects 
#==============================================================================#
#-------------------------------------------------------------------------------
# 2.1. Create df with PCA components 
#-------------------------------------------------------------------------------

dv <- "dv_heat"
model_formula_pls <- as.formula(paste0(dv, " ~ ."))

pca_recipe <- poll_county %>%
  recipe(model_formula_pls) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% # transform to be more normal
  step_dummy(geoid) %>% 
  step_normalize(all_numeric_predictors()) %>% # normalize 
  step_nzv(all_numeric_predictors(), freq_cut = 99/1) %>%  # remove low variance
  step_lincomb(all_numeric_predictors()) %>% # remove linear combinations
  step_corr(threshold = 0.9) %>% # remove highly correlated 
  step_pca(all_numeric_predictors(), num_comp = 15) # convert to pca components 


# processed data -- check to make sure the recipe worked as intended
poll_county_processed <- pca_recipe %>%
  prep() %>%
  bake(poll_county)

# Create new df with random effects and PCA variables 
poll_county_pca <- poll_county_processed %>% 
  mutate(sex = poll_county$sex, 
         age_group = poll_county$age_group, 
         race = poll_county$race, 
         state_geoid = poll_county$state_geoid, 
         reg9 = poll_county$reg9) %>% 
  data.frame()



# Remove 
rm(poll_county_processed)


#-------------------------------------------------------------------------------
# 2.3. Create list of model formulas to test 
#-------------------------------------------------------------------------------
# Define important objects
dv <- "dv_heat"
pca_vars <- names(poll_county_pca)[grepl("PC", names(poll_county_pca))]
glmer_model_formulas <- list()
pca_vars_model <- c()

# Initialize list with no additional components 
glmer_model_formulas[["00"]] <- as.formula(paste0(dv, " ~ (1|state_geoid) + (1|reg9) + 
                                     (1|sex) + (1|age_group) + (1|race) + 
                                     (1|sex:age_group:race)"))

# Create entire list of formulas  
for (var in pca_vars) {
  key <- str_remove(var, "PC")
  pca_vars_model <- c(pca_vars_model, var)
  model_formula <- as.formula(paste0(dv, " ~ (1|state_geoid) + (1|reg9) + 
                                     (1|sex) + (1|age_group) + (1|race) + 
                                     (1|sex:age_group:race) + ",
                                     paste(pca_vars, collapse="+")))
  glmer_model_formulas[[key]] <- model_formula
}


#-------------------------------------------------------------------------------
# 2.4. Test performance of models with different number of components 
#-------------------------------------------------------------------------------
# Run in parallel 
ncores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))
cl <- makeCluster(ncores, type="FORK")
registerDoParallel(cl)


# Manually run 10-fold cross-validation
# write function to run cv 
run_cv <- function(model_formula, seed, n_folds, df, dv) {
    
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

# keep track of time

auc_values <- foreach(key = names(glmer_model_formulas), .combine = "c") %dopar% {
  model_formula <- glmer_model_formulas[[key]]
  run_cv(model_formula=model_formula, seed=42, n_folds=10, df=poll_county_pca, dv="dv_heat")
}

end_time <- proc.time()
duration <- end_time - start_time
runtimemin <- round(duration[3]/60, 1)
paste("Run time county:", runtimemin, "minutes on 10 folds")

# print output
print(auc_values)
  
# Construct the output path to go up and then down directory tree
up_path <- paste(rep("..", 4), collapse = "/")
output_path <- "temp/tuning/"
output_path <- file.path(up_path, output_path)

# Save output
save(auc_values, file=paste0(output_path, "pca_heat_county_tuning.rda"))










