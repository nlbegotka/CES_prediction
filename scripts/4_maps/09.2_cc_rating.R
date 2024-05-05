# Output risk maps using climatecheck data 
# Working in ypccc_hazards.Rproj
# Last updated: 3/27/2024 by NLB 

# Note -- loaded data from old pipeline, eventually update this after merging workflow

#-------------------------------------------------------------------------------
# 0. Load objects 
#-------------------------------------------------------------------------------
## 0.1. Packages ---------------------------------------------------------------

rm(list=ls())
library(dplyr)
library(tidymodels)
library(tigris)
library(sf)
library(ggplot2)
library(scales)


## 0.2. Functions --------------------------------------------------------------

git_basepath <- "downscale/heat" # data path 
git_basepath_old <- "downscale/old_pipeline"
source(file.path(git_basepath, "scripts/0_functions/00_create_directory.R"))
source(file.path(git_basepath, "scripts/0_functions/01_add_bins.R"))
source(file.path(git_basepath, "scripts/0_functions/02_merge_geo.R"))
source(file.path(git_basepath, "scripts/0_functions/03_sf_plot_save.R"))


## 0.3. Input data ---------------------------------------------------------------
load(file.path(git_basepath_old, "temp/hazard_covars_county_unscl.rda"))

#-------------------------------------------------------------------------------
# 1. Clean the climate check data to filter for the variable(s) of interest
#-------------------------------------------------------------------------------
heat_risk_county <- hazard_covars_county %>% 
  dplyr::select(geoid, cc_heat_rating)

rm(hazard_covars_county)

# Also fix problem with Oglala Lakota county manually -- does not look right 
heat_risk_county[heat_risk_county$geoid == 46102, ]$cc_heat_rating <- 
  heat_risk_county[heat_risk_county$geoid == 46121, ]$cc_heat_rating

# Save data for future use 
save(heat_risk_county, file=file.path(git_basepath, "temp/general", "cc_county_heat_risk.rda"))

#-------------------------------------------------------------------------------
# 2. Merge county geography into df with predictions 
#-------------------------------------------------------------------------------

heat_risk_county <- 
  merge_county_geo(df=heat_risk_county, year=2021, crs_code=5070, df_geoid_col="geoid")

#-------------------------------------------------------------------------------
# 2. Prepare data (sf object) to be mapped
#-------------------------------------------------------------------------------
# 10 color bins 
palette_10 <- c("#4575B4", "#77A6CE", "#ABD0E4", "#E0F3F8", "#F4FBD2", "#FEF4AF", "#FEE090",  "#FCA86B", "#EF6E48", "#D73027")

# Create sf with binned cols 
risk_sf_binned <- heat_risk_county %>% 
  add_bins("cc_heat_rating", 10)

#-------------------------------------------------------------------------------
# 3. Output map 
#-------------------------------------------------------------------------------
map_type <- "cc_risk"
outpath <- file.path(git_basepath, "output/4_maps", map_type)
create_directory(outpath)
title <- "County-level heat risk"
legendlabel <- "Risk"

sf_plot_save_county(risk_sf_binned, "cc_heat_rating_bins", palette_10, 
                    0.35, title, legendlabel, file.path(outpath, "cc_heat_rating.png"))


