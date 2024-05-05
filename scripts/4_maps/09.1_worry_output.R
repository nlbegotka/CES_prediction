# Output maps 
# Working in ypccc_hazards.Rproj
# Last updated: 3/27/2024 by NLB 

#-------------------------------------------------------------------------------
# 0. Load objects 
#-------------------------------------------------------------------------------
## 0.1. Packages ---------------------------------------------------------------

rm(list=ls())
library(dplyr)
library(tidymodels)
library(tigris)
library(sf)

## 0.2. Functions --------------------------------------------------------------

git_basepath <- "downscale/heat" # data path 
source(file.path(git_basepath, "scripts/0_functions/00_create_directory.R"))
source(file.path(git_basepath, "scripts/0_functions/01_add_bins.R"))
source(file.path(git_basepath, "scripts/0_functions/02_merge_geo.R"))
source(file.path(git_basepath, "scripts/0_functions/03_sf_plot_save.R"))


## 0.3. Input data ---------------------------------------------------------------
load(file.path(git_basepath, "output/2_poststrat/ensemble/heat_poststrat_output_all.rda"))

#-------------------------------------------------------------------------------
# 1. Merge county geography into df with predictions 
#-------------------------------------------------------------------------------

heat_worry_county <- 
  merge_county_geo(df=average_poststrat_comp, year=2021, crs_code=5070, df_geoid_col="geoid")

# Select columns based on which output we want to plot -- in this case, averages
heat_worry_county_sub <- heat_worry_county %>% 
  dplyr::select(GEOID, weighted_average_all, weighted_average_top_3, geometry)

# Remove unnecessary objects
rm(average_poststrat_comp)
#-------------------------------------------------------------------------------
# 2. Prepare data (sf object) to be mapped
#-------------------------------------------------------------------------------
# 10 color bins 
palette_10 <- c("#4575B4", "#77A6CE", "#ABD0E4", "#E0F3F8", "#F4FBD2", "#FEF4AF", "#FEE090",  "#FCA86B", "#EF6E48", "#D73027")

# Create sf with binned cols 
worry_sf_binned <- heat_worry_county_sub %>% 
  add_bins("weighted_average_all", 10)  %>% 
  add_bins("weighted_average_top_3", 10) 

#-------------------------------------------------------------------------------
# 3. Output maps
#-------------------------------------------------------------------------------
# Create output path and define arguments 
map_type <- "worry"
outpath <- file.path(git_basepath, "output/4_maps", map_type)
create_directory(outpath)
title <- "County-level heat worry"
legendlabel <- "Worry"

# Output 
sf_plot_save_county(worry_sf_binned, "weighted_average_all_bins", palette_10, 
                    0.35, title, legendlabel, file.path(outpath, "weight_avg_all.png"))

sf_plot_save_county(worry_sf_binned, "weighted_average_top_3_bins", palette_10, 
                    0.35, title, legendlabel, file.path(outpath, "weight_avg_top3.png"))

