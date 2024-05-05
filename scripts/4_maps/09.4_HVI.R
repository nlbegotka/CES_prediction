# Map county averages of the Manware Heat Vulnerability Index values
# Working in ypccc_hazards.Rproj
# last modified: 03/27/2024, NLB

#-------------------------------------------------------------------------------
# 0. Load objects
#-------------------------------------------------------------------------------
## 0.1. Packages ---------------------------------------------------------------

rm(list=ls())
library(dplyr)
library(ggplot2)
library(tigris)
library(sf)
library(scales)

## 0.2. Functions --------------------------------------------------------------

git_basepath <- "downscale/heat" # data path 
source(file.path(git_basepath, "scripts/0_functions/00_create_directory.R"))
source(file.path(git_basepath, "scripts/0_functions/01_add_bins.R"))
source(file.path(git_basepath, "scripts/0_functions/02_merge_geo.R"))
source(file.path(git_basepath, "scripts/0_functions/03_sf_plot_save.R"))
source(file.path(git_basepath, "scripts/0_functions/04_calc_gap.R"))


## 0.3. Input data -------------------------------------------------------------
hvi_input <- read.csv("_data/validation/CHENlab-Yale-HVI_US/hvicopy.csv", colClasses = c(GEOID = "character"))
load(file.path(git_basepath, "output/2_poststrat/ensemble/heat_poststrat_output_all.rda"))

## 0.4. Crosswalks -------------------------------------------------------------
load("_data/xwalks/state_region_xwalk_2023.rda") #(state_region_xwalk)
load("_data/xwalks/tract_county_state_xwalk_2022.rda") #(tract_region_xwalk)

#-------------------------------------------------------------------------------
# 1. Clean xwalk and HVI data set
#-------------------------------------------------------------------------------

county_state_xwalk <- tract_county_state_xwalk %>% 
  dplyr::select(reg9, state_fips, county_fips, region, stusps, state, county_name) %>% 
  distinct()

# Process the GEOID column
hvi <- hvi_input %>%
  mutate(GEOID = as.character(GEOID),  # Ensure GEOID is a character, this is redundant if colClasses is set correctly
         county_fips = ifelse(nchar(GEOID) == 10, paste0("0", GEOID), GEOID),  # Add leading zero if needed
         county_fips = substr(county_fips, 1, 5))  # Extract the first five digits for county_fips

hvi$X <- NULL

# Calculate the average HVI for each county_fips and create the HVI_county variable
hvi_cavg <- hvi %>%
  group_by(county_fips) %>%
  summarise(HVI_Score = mean(HVI_Score, na.rm = TRUE))  # Compute average HVI, removing NA values if any


#-------------------------------------------------------------------------------
# 2. Impute missing values for HVI by imputing by closest value 
#-------------------------------------------------------------------------------
hvi_sf <- left_join(average_poststrat_comp, hvi_cavg, by=c("geoid" = "county_fips")) %>% 
  merge_county_geo(2021, 5070, "geoid") %>% 
  dplyr::select(geoid=GEOID, HVI_Score, geometry) 

# Replace each missing value with the geographically closest HVI that is not missing 
for (i in which(is.na(hvi_sf$HVI_Score))) {
  distances <- st_distance(hvi_sf[i,], hvi_sf[!is.na(hvi_sf$HVI_Score),])
  nearest_index <- which.min(distances)
  hvi_sf$HVI_Score[i] <- hvi_sf$HVI_Score[nearest_index]
}

# Convert to df 
hvi_county_avg_imputed <- hvi_sf %>% 
  data.frame() %>% 
  dplyr::select(geoid, HVI_Score)

# Save this output 
save(hvi_county_avg_imputed, file=file.path(git_basepath, "temp/hvi_county_average.rda"))

#-------------------------------------------------------------------------------
# 2. Calculate the gap between heat worry and HVI 
#-------------------------------------------------------------------------------
hvi_gap <- left_join(average_poststrat_comp, hvi_county_avg_imputed, by="geoid") %>% 
  dplyr::select(geoid, heat_worry=weighted_average_top_3, HVI_Score) %>% 
  mutate(heat_gap = calc_gap(HVI_Score, heat_worry, c(-1, 1)))

# Remove NAs
hvi_gap <- na.omit(hvi_gap)

#-------------------------------------------------------------------------------
# 3. Create sf object
#-------------------------------------------------------------------------------
hvi_sf_gap <- merge_county_geo(hvi_gap, 2021, 5070, "geoid") 

#-------------------------------------------------------------------------------
# 4. Prepare data to be mapped
#-------------------------------------------------------------------------------
# 10 color bins 
palette_10 <- c("#4575B4", "#77A6CE", "#ABD0E4", "#E0F3F8", "#F4FBD2", "#FEF4AF", "#FEE090",  "#FCA86B", "#EF6E48", "#D73027")

# Bin columns 
hvi_sf_gap_binned <- hvi_sf_gap %>% 
  add_bins("HVI_Score", 10) %>% 
  add_bins("heat_gap", 10, 2)

#-------------------------------------------------------------------------------
# 5. Output maps  
#-------------------------------------------------------------------------------
## 5.1. HVI score map  ---------------------------------------------------------

map_type <- "HVI"
outpath <- file.path(git_basepath, "output/4_maps", map_type)
create_directory(outpath)
title <- "County-level Heat Vulnerability Index (HVI)"
legendlabel <- "HVI"

sf_plot_save_county(hvi_sf_gap_binned, "HVI_Score_bins", palette_10, 
                    0.35, title, legendlabel, file.path(outpath, "HVI_score.png"))

## 5.2. Gap map  ---------------------------------------------------------------

map_type <- "HVI"
outpath <- file.path(git_basepath, "output/4_maps", map_type)
create_directory(outpath)
title <- "County-level Heat Vulnerability Index (HVI) Gap"
legendlabel <- "Gap"

sf_plot_save_county(hvi_sf_gap_binned, "heat_gap_bins", palette_10, 
                    0.35, title, legendlabel, file.path(outpath, "HVI_score_gap.png"))



