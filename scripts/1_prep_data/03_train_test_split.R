# Split data into train test splits -- NOT using EBMA
# load ypccc_hazards.Rproj!
# Last updated: 4/17/2024 by NLB 

#-------------------------------------------------------------------------------
# 0. Load packages and poll data
#-------------------------------------------------------------------------------
# packages
rm(list=ls())
library(dplyr)
library(tidymodels)

# data
load("downscale/heat/temp/poll_state_heat.rda") # state heat poll 
load("downscale/heat/temp/poll_county_heat.rda") # county heat poll 


#-------------------------------------------------------------------------------
# 1. Split data into training and testing sets
#    Training set will be used for hyperparameter tuning and final training 
#    Testing set will be used for final model evaluation 
#-------------------------------------------------------------------------------
## 1.1. State ------------------------------------------------------------------
# Split data into 80-20
set.seed(42)
split_state <- poll_state %>% 
  initial_split(prop = 0.8, strata = "geoid")
poll_state_train <- training(split_state)
poll_state_test <- testing(split_state)

# Make sure that the levels are only for levels within each split 
poll_state_train$geoid <- droplevels(poll_state_train$geoid)
poll_state_test$geoid <- droplevels(poll_state_test$geoid)

# Save
save(poll_state_train, file="downscale/heat/temp/poll_state_heat_train.rda")
save(poll_state_test, file="downscale/heat/temp/poll_state_heat_test.rda")




## 1.2 County -----------------------------------------------------------------
# Split data into 80-20
set.seed(42)
split_county <- poll_county %>% 
  initial_split(prop = 0.8, strata = "state_geoid")
poll_county_train <- training(split_county)
poll_county_test <- testing(split_county)

# Make sure that the levels are only for levels within each split 
poll_county_train$geoid <- droplevels(poll_county_train$geoid)
poll_county_test$geoid <- droplevels(poll_county_test$geoid)
poll_county_train$state_geoid <- droplevels(poll_county_train$state_geoid)
poll_county_test$state_geoid <- droplevels(poll_county_test$state_geoid)

# Save
save(poll_county_train, file="downscale/heat/temp/poll_county_heat_train.rda")
save(poll_county_test, file="downscale/heat/temp/poll_county_heat_test.rda")



