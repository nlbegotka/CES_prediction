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
source(file.path(git_basepath, "scripts/0_functions/04_calc_gap.R"))


## 0.3. Input data ---------------------------------------------------------------
load(file.path(git_basepath_old, "temp/hazard_covars_county_unscl.rda"))
load(file.path(git_basepath, "output/2_poststrat/ensemble/heat_poststrat_output_all.rda"))


#-------------------------------------------------------------------------------
# 1. Merge worry with cc_rating and calculate gap
#-------------------------------------------------------------------------------
heat_gap_county <- left_join(average_poststrat_comp, hazard_covars_county, by="geoid") %>% 
  dplyr::select(geoid, heat_worry=weighted_average_top_3, cc_heat_rating) %>% 
  mutate(heat_gap = calc_gap(cc_heat_rating, heat_worry, c(-1, 1)))


#-------------------------------------------------------------------------------
# 2. Merge county geography  
#-------------------------------------------------------------------------------

heat_gap_county_geo <- 
  merge_county_geo(df=heat_gap_county, year=2021, crs_code=5070, df_geoid_col="geoid")


#-------------------------------------------------------------------------------
# 3. Prepare data (sf object) to be mapped
#-------------------------------------------------------------------------------
# 10 color bins 
palette_10 <- c("#4575B4", "#77A6CE", "#ABD0E4", "#E0F3F8", "#F4FBD2", "#FEF4AF", "#FEE090",  "#FCA86B", "#EF6E48", "#D73027")

# Create df with binned cols 
gap_sf_binned <- heat_gap_county_geo %>% 
  add_bins("heat_gap", 10, 2)

#-------------------------------------------------------------------------------
# 4. Output map 
#-------------------------------------------------------------------------------
map_type <- "cc_risk_gap"
outpath <- file.path(git_basepath, "output/4_maps", map_type)
create_directory(outpath)
title <- "County-level heat risk-worry gap"
legendlabel <- "Gap"

sf_plot_save_county(gap_sf_binned, "heat_gap_bins", palette_10, 
                    0.35, title, legendlabel, file.path(outpath, "cc_heat_gap.png"))



