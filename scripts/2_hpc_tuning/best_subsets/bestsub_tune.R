# Tune glmer model with features from best subsets selection on Yale cluster
# Last updated: 5/11/2024 by NB 

#-------------------------------------------------------------------------------
# 0. Load objects
#-------------------------------------------------------------------------------
# Libraries
rm(list=ls())
library(lme4)
library(tidymodels)
library(tidyverse)
library(pROC) 
library(foreach)
library(doParallel)


# Define path to read in data 
up_path <- paste(rep("..", 3), collapse = "/") # dynamic
input_path <- file.path(up_path, "temp")

# Read in data 
load(file.path(input_path, "poll_train.rda")) # data


#-------------------------------------------------------------------------------
# 1. Preprocess data for modeling 
#-------------------------------------------------------------------------------
dv <- "climate_change_worry"
model_formula <- as.formula(paste0(dv, " ~ ."))

bs_recipe <- poll_train %>%
  recipe(model_formula) %>% 
  step_normalize(all_numeric_predictors())

poll_processed <- bs_recipe %>%
  prep() %>%
  bake(poll_train)


#-------------------------------------------------------------------------------
# 2. Define cross-validation function -- calculates AUC 
#-------------------------------------------------------------------------------

# Define function to run cross-validation 
run_cv <- function(df, dv, model_formula, n_folds, seed) {
  
  set.seed(seed)
  folds <- sample(rep(1:n_folds, length.out = nrow(df))) 
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
# 3. Run through all feature combinations and track cross-validated aucs 
#-------------------------------------------------------------------------------

# Create all combinations of predictors
rand_effect_vars <- "state_fips"
features <- names(poll_county_processed)[!names(poll_county_processed) %in% c(dv, rand_effect_vars)]

# manually select which features to test in best subsets 
bestsub_features <- c("age", "gender", "union_member", "cdc_svi_comp","college_degree", 
                      "white_non_hisp", "pol_identity", "citizen", "rural_resident", 
                      "evangelical", "military_service", "heterosexual")

n <- length(bestsub_features)
all_combinations <- lapply(1:n, function(x) combn(bestsub_features, x, simplify=FALSE))
all_combinations <- unlist(all_combinations, recursive = FALSE)

# Set up parallel backend
ncores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))
cl <- makeCluster(ncores, type="FORK")
registerDoParallel(cl)

# Track time
start_time <- proc.time()

# Run parallel loop using for each 
auc_values <- foreach(features = all_combinations, .combine = "c") %dopar% {
  
  model_formula <- as.formula(paste0(dv, " ~ (1|state_fips) + ",
                                     paste(features, collapse="+")))
  
  run_cv(df = poll_train, dv = "climate_change_worry", 
         model_formula = model_formula, seed = 42, n_folds = 10)
  
}


# Print runtime
end_time <- proc.time()
duration <- end_time - start_time
runtimemin <- round(duration[3]/60, 1)
paste("Run time county:", runtimemin, "minutes on 10 folds")

# Find best feautres and associatd AUC 
best_auc_idx <- which.min(auc_values)
best_features <- all_combinations[best_auc_idx] %>% unlist()
best_auc <- auc_values[best_auc_idx]

print(best_features)
print(best_auc)

#-------------------------------------------------------------------------------
# 4. Save output
#-------------------------------------------------------------------------------
up_path <- paste(rep("..", 3), collapse = "/") # dynamic
output_path <- file.path(up_path, "temp/tuning/")
save(best_features, best_auc, file=paste0(output_path, "bestsub_tuning.rda"))







