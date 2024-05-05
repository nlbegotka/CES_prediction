# Map Howe 2015 heat worry and gap
# Working in ypccc_hazards.Rproj
# last modified: 03/27/2024, NLB

#-------------------------------------------------------------------------------
# 0. Load libraries and data
#-------------------------------------------------------------------------------

# 0.1. Packages ---------------------------------------------------------------

rm(list=ls())
library(dplyr)
library(ggplot2)
library(tigris)
library(sf)
library(scales)
library(stringr)

## 0.2. Functions --------------------------------------------------------------

git_basepath <- "downscale/heat" # data path 
drop_basepath <- "~/Dropbox (YSE)"
source(file.path(git_basepath, "scripts/0_functions/00_create_directory.R"))
source(file.path(git_basepath, "scripts/0_functions/01_add_bins.R"))
source(file.path(git_basepath, "scripts/0_functions/02_merge_geo.R"))
source(file.path(git_basepath, "scripts/0_functions/03_sf_plot_save.R"))
source(file.path(git_basepath, "scripts/0_functions/04_calc_gap.R"))


## 0.3. Input data -------------------------------------------------------------
howe_worry_cty <- read.csv("_data/validation/heat2015/riskp_county_table.csv")
load(file.path(git_basepath, "output/2_poststrat/ensemble/heat_poststrat_output_all.rda"))


#-------------------------------------------------------------------------------
# 1. Pad GEOIDs
#-------------------------------------------------------------------------------

# {ad county FIPS to 5 digits
howe_worry <- howe_worry_cty %>% 
  mutate(CountyFP = str_pad(CountyFP, width=5, pad="0"))

# Recode old county 
howe_worry$CountyFP <- gsub("46113", "46102", howe_worry$CountyFP)

# Rename our output df 
ypccc_worry <- average_poststrat_comp
rm(average_poststrat_comp)

#-------------------------------------------------------------------------------
# 2. Merge data and select relevant columns
#-------------------------------------------------------------------------------

howe_worry_merge <- ypccc_worry %>%
  left_join(howe_worry, by=c("geoid" = "CountyFP")) %>%
  dplyr::select(geoid, County_name, State_name, howe_worry=riskp_est, ypccc_worry=weighted_average_top_3,  reg4, reg9)


#-------------------------------------------------------------------------------
# 3. Calculate Howe YPCCC output gap 
#-------------------------------------------------------------------------------
howe_worry_gap <- howe_worry_merge %>% 
  mutate(worry_gap = calc_gap(howe_worry, ypccc_worry, c(-1, 1))) 

#-------------------------------------------------------------------------------
# 4. Merge county geography
#-------------------------------------------------------------------------------

howe_worry_sf <- merge_county_geo(howe_worry_gap, 2021, 5070, "geoid") 

#-------------------------------------------------------------------------------
# 5. Prepare data to be mapped
#-------------------------------------------------------------------------------

# 10 color bins 
palette_10 <- c("#4575B4", "#77A6CE", "#ABD0E4", "#E0F3F8", "#F4FBD2", "#FEF4AF", "#FEE090",  "#FCA86B", "#EF6E48", "#D73027")

# Bin columns
howe_worry_sf_binned <- howe_worry_sf %>% 
  add_bins("howe_worry", 10) %>% 
  add_bins("worry_gap", 10, 2)

#-------------------------------------------------------------------------------
# 6. Output maps
#-------------------------------------------------------------------------------
## 6.0. Global operations  -----------------------------------------------------
map_type <- "worry_Howe"
outpath <- file.path(git_basepath, "output/4_maps", map_type)
create_directory(outpath)

## 6.1. Howe worry map  --------------------------------------------------------
title <- "County-level 2015 Heat Worry Estimates (Howe)"
legendlabel <- "Worry"
sf_plot_save_county(howe_worry_sf_binned, "howe_worry_bins", palette_10, 
                    0.35, title, legendlabel, file.path(outpath, "Howe_worry.png"))

## 6.2. Gap map  ---------------------------------------------------------------
title <- "County-level 2015 Heat Worry Estimates (Howe) Gap"
legendlabel <- "Gap"
sf_plot_save_county(howe_worry_sf_binned, "worry_gap_bins", palette_10, 
                    0.35, title, legendlabel, file.path(outpath, "Howe_worry_gap.png"))




