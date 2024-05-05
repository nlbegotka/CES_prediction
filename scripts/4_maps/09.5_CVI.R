# Map county averages of the Climate Vulnerability Index
# Working in ypccc_hazards.Rproj
# last modified: 03/27/2024, NLB

#-------------------------------------------------------------------------------
# 0. Load libraries and data
#-------------------------------------------------------------------------------

## 0.1. Packages ---------------------------------------------------------------

rm(list=ls())
library(dplyr)
library(ggplot2)
library(tigris)
library(sf)
library(scales)
library(readxl)


## 0.2. Functions --------------------------------------------------------------

git_basepath <- "downscale/heat" # data path 
drop_basepath <- "~/Dropbox (YSE)"
source(file.path(git_basepath, "scripts/0_functions/00_create_directory.R"))
source(file.path(git_basepath, "scripts/0_functions/01_add_bins.R"))
source(file.path(git_basepath, "scripts/0_functions/02_merge_geo.R"))
source(file.path(git_basepath, "scripts/0_functions/03_sf_plot_save.R"))
source(file.path(git_basepath, "scripts/0_functions/04_calc_gap.R"))


## 0.3. Input data -------------------------------------------------------------
cvi_index <- read_excel(file.path(drop_basepath, "ypcccdb/_data/external/usa/cvi/Master CVI Dataset - Oct 2023.xlsx"), sheet="Domain CVI Values")
cvi_mortality <- read_excel(file.path(drop_basepath, "ypcccdb/_data/external/usa/cvi/Master CVI Dataset - Oct 2023.xlsx"), sheet="Indicator CVI Values") 
load(file.path(git_basepath, "output/2_poststrat/ensemble/heat_poststrat_output_all.rda"))

## 0.4. Crosswalks -------------------------------------------------------------
load("_data/xwalks/state_region_xwalk_2023.rda") #(state_region_xwalk)
load("_data/xwalks/tract_county_state_xwalk_2022.rda") #(tract_region_xwalk)

#-------------------------------------------------------------------------------
# 1. Clean xwalk and CVI data
#-------------------------------------------------------------------------------

county_state_xwalk <- tract_county_state_xwalk %>% 
  dplyr::select(reg9, state_fips, county_fips, region, stusps, state, county_name) %>% 
  distinct()

# Combine cvi data sets and select vars of intrest
cvi_input <- cbind(cvi_index, cvi_mortality) %>% 
  data.frame() %>% 
  dplyr::select(GEOID=FIPS.Code, cvi_score=Overall.CVI.Score, temp_related_mortality=Temperature.related.mortality)

# Fix problem with Shannon county FIPS code 
cvi_input$GEOID <- gsub("46113", "46102", cvi_input$GEOID)

# Process the GEOID column
cvi_df <- cvi_input %>%
  data.frame() %>% 
  mutate(GEOID = as.character(GEOID),  # Ensure GEOID is a character, this is redundant if colClasses is set correctly
         county_fips = ifelse(nchar(GEOID) == 10, paste0("0", GEOID), GEOID),  # Add leading zero if needed
         county_fips = substr(county_fips, 1, 5)) %>% # Extract the first five digits for county_fips
dplyr::select(county_fips, cvi_score, temp_related_mortality)


# Calculate the average CVI score and temp mortality for each county 
cvi_cavg <- cvi_df %>%
  group_by(county_fips) %>%
  summarise_all(mean)  

# Filter out AK and HI
cvi_cavg <- cvi_cavg %>% 
  filter(!grepl("^02|^15", county_fips))

# Save data for future use 
cvi_county_avg <- cvi_cavg 
save(cvi_county_avg, file=file.path(git_basepath, "temp/general", "cvi_county_average.rda"))


#-------------------------------------------------------------------------------
# 2. Calculate gaps for columns of interest -- CVI score and mortality
#-------------------------------------------------------------------------------
cvi_gap <- left_join(average_poststrat_comp, cvi_cavg, by=c("geoid" = "county_fips")) %>% 
  dplyr::select(geoid, heat_worry=weighted_average_top_3, cvi_score, temp_related_mortality) %>% 
  mutate(score_worry_gap = calc_gap(cvi_score, heat_worry, c(-1, 1))) %>% 
  mutate(mortality_worry_gap = calc_gap(temp_related_mortality, heat_worry, c(-1, 1)))


#-------------------------------------------------------------------------------
# 3. Create sf object
#-------------------------------------------------------------------------------

cvi_gap <- merge_county_geo(cvi_gap, 2021, 5070, "geoid") 


#-------------------------------------------------------------------------------
# 4. Prepare data to be mapped
#-------------------------------------------------------------------------------

# 10 color bins 
palette_10 <- c("#4575B4", "#77A6CE", "#ABD0E4", "#E0F3F8", "#F4FBD2", "#FEF4AF", "#FEE090",  "#FCA86B", "#EF6E48", "#D73027")

# Bin columns
cvi_sf_gap_binned <- cvi_gap %>% 
  add_bins("cvi_score", 10, 2) %>% 
  add_bins("temp_related_mortality", 10, 2) %>% 
  add_bins("score_worry_gap", 10, 2) %>% 
  add_bins("mortality_worry_gap", 10, 2) 
  

#-------------------------------------------------------------------------------
# 5. Output maps 
#-------------------------------------------------------------------------------
## 5.0. Global operations  -----------------------------------------------------
map_type <- "CVI"
outpath <- file.path(git_basepath, "output/4_maps", map_type)
create_directory(outpath)

## 5.1. CVI score map  ---------------------------------------------------------
title <- "County-level Climate Vulnerability Index (CVI) Score"
legendlabel <- "CVI" 
sf_plot_save_county(cvi_sf_gap_binned, "cvi_score_bins", palette_10, 
                    0.35, title, legendlabel, file.path(outpath, "CVI_score.png"))

## 5.2. CVI index gap map  -----------------------------------------------------
title <- "County-level Climate Vulnerability Index (CVI) Score Gap"
legendlabel <- "Gap" 
sf_plot_save_county(cvi_sf_gap_binned, "score_worry_gap_bins", palette_10, 
                    0.35, title, legendlabel, file.path(outpath, "CVI_score_gap.png"))

## 5.3. CVI mortality map  -----------------------------------------------------
title <- "County-level Temperature-Related Mortality (CVI)"
legendlabel <- "Mortality" 
sf_plot_save_county(cvi_sf_gap_binned, "temp_related_mortality_bins", palette_10, 
                    0.35, title, legendlabel, file.path(outpath, "CVI_mortality.png"))

## 5.4. CVI mortality gap map  -------------------------------------------------
title <- "County-level Temperature-Related Mortality (CVI) Gap"
legendlabel <- "Gap" 
sf_plot_save_county(cvi_sf_gap_binned, "mortality_worry_gap_bins", palette_10, 
                    0.35, title, legendlabel, file.path(outpath, "CVI_mortality_gap.png"))

