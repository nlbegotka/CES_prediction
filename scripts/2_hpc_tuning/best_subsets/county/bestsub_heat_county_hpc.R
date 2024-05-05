# Run glmer model with components from PCA for heat
# Only using tidymodels framework for preprocessing due to its systemic errors 
# Building models for Ensemble Baysian Model Averaging (EBMA)
# Working in ypccc_hazards.Rproj
# Last updated: 2/13/2024 by NLB 


#-------------------------------------------------------------------------------
# Thoughts 
#-------------------------------------------------------------------------------
# There is a lot of area for improvement!
# 1. Passing in PCA components into a xgb or random forest model (among others)
# 2. Passing in UMAP compnents (an alternative to PCA) 
# 3. This is something we should test 


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
# 1. Preprocess df for best subsets 
#==============================================================================#
#-------------------------------------------------------------------------------
# 1.1. Create recipe and preprocess
#-------------------------------------------------------------------------------

dv <- "dv_heat"
model_formula <- as.formula(paste0(dv, " ~ ."))

bs_recipe <- poll_county_train %>%
  recipe(model_formula) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% # transform to be more normal
  step_dummy(geoid) %>% 
  step_normalize(all_numeric_predictors()) %>% # normalize 
  step_nzv(all_numeric_predictors(), freq_cut = 99/1) %>%  # remove low variance
  step_lincomb(all_numeric_predictors()) %>% # remove linear combinations
  step_corr(threshold = 0.9)     # remove highly correlated 


# processed data -- check to make sure the recipe worked as intended
poll_county_processed <- bs_recipe %>%
  prep() %>%
  bake(poll_county_train)

#-------------------------------------------------------------------------------
# 2.3. Test performance of models with different variable combinations 
#-------------------------------------------------------------------------------

# Create important objects
rand_effect_vars <- c("state_geoid", "reg9", "race", "age_group", "sex")
predictors <- names(poll_county_processed)[!names(poll_county_processed) %in% c(dv, rand_effect_vars)]
# manually select which predictors to test in best subsets 
bestsub_predictors <- c("cc_heat_threshold_f", "cc_heat_wetbulb_threshold_f", 
                        "y_centroid_lat", "y_pct_presdem",
                        "c_prop_has_bachelors", 
                        "cdc_svi_demo", "cdc_svi_housing", "cdc_svi_socioecon", 
                        "fema_HWAV_HLRP", "fema_HWAV_EVNTS", "fema_HWAV_EXPT", "fema_HWAV_EALB")

n <- length(bestsub_predictors)
all_combinations <- lapply(1:n, function(x) combn(bestsub_predictors, x, simplify=FALSE))
all_combinations <- unlist(all_combinations, recursive = FALSE)

# Set up parallel backend
ncores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))
cl <- makeCluster(ncores, type="FORK")
registerDoParallel(cl)

# Write function to test different models 
fit_glmer <- function(predictors, df, dv) {
  formula <- as.formula(paste0(dv, " ~ (1|state_geoid) + (1|reg9) + 
                                     (1|sex) + (1|age_group) + (1|race) + 
                                     (1|sex:age_group:race) + ",
                               paste(predictors, collapse="+")))
  glm_model <- glmer(formula = formula,
                     family = binomial(link = "logit"),
                     data = df)
  return(BIC(glm_model))  # return model evaluation metric
  
}

# Run function in parallel using foreach 
bic_values <- foreach(predictors = all_combinations, .combine = "c") %dopar% {
  fit_glmer(predictors, df=poll_county_processed, dv="dv_heat")
}

best_bic_idx <- which.min(bic_values)
best_predictors <- all_combinations[best_bic_idx] %>% unlist()
best_bic <- bic_values[best_bic_idx]

print(best_predictors)
print(best_bic)

# Save output
up_path <- paste(rep("..", 4), collapse = "/") # dynamic
outpath <- "temp/tuning/"
output_path <- file.path(up_path, outpath)
save(best_predictors, best_bic, file=paste0(output_path, "bestsub_heat_county_tuning.rda"))







