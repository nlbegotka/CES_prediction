# Select predictors that are best for predicting heat worry 
# load ypccc_hazards.Rproj!
# Last updated: 4/5/2024 by NLB 

#==============================================================================#
# 0. Load libraries poll data and functions
#==============================================================================#
rm(list=ls())
library(dplyr)
library(tidymodels)
library(ggplot2)
library(corrplot)
library(stringr)

drop_basepath <- "~/Dropbox (YSE)/" # dynamic
load("downscale/all/output/poll_state_with_covars.rda")

#==============================================================================#
# 1. Identify predictors of that would intuitively be related to heat worry
#==============================================================================#
#-------------------------------------------------------------------------------
# 1.1 Identify by data source
#-------------------------------------------------------------------------------
# Heat-specific vars -- cc means climatecheck and y means yale 
cc_heat <- names(poll_state)[grepl("heat", names(poll_state)) & names(poll_state) != "dv_heat"]
y_heat <- c("y_centroid_lat", "y_mean_impervious", "y_aircon")

# Vars we want to include in all hazards models 
# Add elevation per county to y_all (across all models)
y_all <- c("y_pct_presdem", "y_prop_co2_pop", "c_prop_has_bachelors", "c_median_family_household_income")

# Look more closely at fema data again -- search through dict to find interesting vars
fema_dictionary <- 
  read.csv(paste0(drop_basepath, "ypcccdb/_data/external/usa/fema/natriskindex/NRIDataDictionary.csv")) %>% 
  dplyr::mutate(Field.Alias = str_replace_all(Field.Alias, " - ", " ")) %>% 
  dplyr::mutate(Field.Alias = str_remove(Field.Alias, "\\(.*")) 


fema_heat_all <- names(poll_state)[grepl("fema_HWAV", names(poll_state))]
fema_heat_spec <- NULL # possibly specify subset of predictors -- some overlap

# Social vulnerability vars
sovi_vars <- c("cdc_svi_socioecon", "cdc_svi_demo", "cdc_svi_race", "cdc_svi_housing")

# Put them all together
heat_vars_i <- c(cc_heat, y_heat, y_all, fema_heat_all, sovi_vars)

#-------------------------------------------------------------------------------
# 1.2 Check correlation plot for overlapping information
#-------------------------------------------------------------------------------
# Identify correlated variables 
cor_matrix <- cor(poll_state[, heat_vars_i])
threshold <- 0.90

# Check section 1.4 for description of the line below 
correlated_vars <- colnames(cor_matrix)[apply(cor_matrix, 1, function(x) sum(abs(x) > threshold) > 1)]

# Check correlation matrix of correlated vars to hand pick which ones to keep 
# Zoom the plot to see clearly 
cor_matrix_p <- cor(poll_state[, correlated_vars])
corrplot(
  cor_matrix_p,
  method = "circle",
  type = "upper",
  order = "hclust",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 0.6,       # Adjust the text label size for variable names
  number.cex = 0.8,   # Adjust the size of numeric labels
  addCoef.col = "black",
  bg = "white",
  pch.col = "black",
  pch.cex = 1.5,
  mar = c(0, 0, 1, 0)
)

# We see that multiple variables are completely correlated so we remove them
rem_vars <- c("cc_heat_wetbulb_magnitude_1990_f", "cc_heat_magnitude_1990_f", 
              "fema_HWAV_RISKS", "fema_HWAV_EXPB", "fema_HWAV_EXPP", "fema_HWAV_EXPPE", 
              "fema_HWAV_EALP", "fema_HWAV_EALPE")

heat_vars_state <- heat_vars_i[!heat_vars_i %in% rem_vars]


#-------------------------------------------------------------------------------
# 1.3 Save vars 
#-------------------------------------------------------------------------------
save(heat_vars_state, file="downscale/heat/temp/heat_vars_state.rda")


#-------------------------------------------------------------------------------
# 1.4 Code description 
#-------------------------------------------------------------------------------
# The  code iterates through each row of the correlation matrix and checks if the 
# absolute value of any correlation coefficient in the row is greater than a specified threshold. 
# Then, it sums up the number of coefficients above the threshold for each row. 
# Since every variable is perfectly correlated with itself, we must ensure that 
# the sum is greater than 1 to confirm correlation with at least one other variable. 
# Finally, the result is a boolean vector indicating whether each variable is 
# correlated with at least one other variable.


