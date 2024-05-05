# 01.3_clean_heat_poll_final.R
# This is another script where we do data cleaning on poll...
# One day we will combine all the poll cleaning scripts
# load ypccc_hazards.Rproj!
# Last updated: 2/4/2024 by NLB 


#==============================================================================#
# 0. Load libraries poll data and functions
#==============================================================================#
rm(list=ls())
library(dplyr)
library(ggplot2)
library(corrplot)
library(stringr)

load("downscale/all/output/poll_state_with_covars.rda") # state poll
load("downscale/all/output/poll_county_with_covars.rda") # county poll
load("downscale/all/output/df_state_with_covars.rda") # state df 
load("downscale/all/output/df_county_with_covars.rda") # county df 
load("downscale/heat/temp/heat_vars_state.rda") # heat vars state
load("downscale/heat/temp/heat_vars_county.rda") # heat vars county


#================================================================================#
# 1. Select for model vars + heat vars and save 
#================================================================================#
# Polls 
poll_state <- poll_state %>% 
  dplyr::select(geoid, state, reg4, reg9, race, age_group, 
                sex, all_of(heat_vars_state), dv_heat)

poll_county <- poll_county %>% 
  dplyr::select(geoid, county, state_geoid, reg4, reg9, race, age_group, 
                sex, all_of(heat_vars_county), dv_heat)

# Dfs 
df_state <- df_state %>% 
  dplyr::select(geoid, state, reg4, reg9, race, age_group, 
                sex, n, N, n_pct_geo, all_of(heat_vars_state))

df_county <- df_county %>% 
  dplyr::select(geoid, county, state_geoid, reg4, reg9, race, age_group, 
                sex, n, N, n_pct_geo, all_of(heat_vars_county))

# Save 
save(poll_state, file="downscale/heat/temp/poll_state_heat.rda")
save(poll_county, file="downscale/heat/temp/poll_county_heat.rda")
save(df_state, file="downscale/heat/temp/df_state_heat.rda")
save(df_county, file="downscale/heat/temp/df_county_heat.rda")
