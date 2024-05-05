# Description
# Load ypccc_hazards.Rproj!
# Last updated: [date] by NLB

#-------------------------------------------------------------------------------
# 0. Load objects 
#-------------------------------------------------------------------------------
rm(list=ls())

# Packages 
library(dplyr)
library(ggplot2)
library(tigris)
library(sf)
library(scales)


# Functions 
source("downscale/heat/scripts/0_functions/00_create_directory.R")
source("downscale/heat/scripts/0_functions/01_add_bins.R")
source("downscale/heat/scripts/0_functions/02_merge_geo.R")
source("downscale/heat/scripts/0_functions/03_sf_plot_save.R")
source("downscale/heat/scripts/0_functions/04_calc_gap.R")

# Data 
community_resilience <- read.csv("_data/external/usa/census/cre/CRE19_Heat.csv")
load("downscale/heat/output/2_poststrat/ensemble/heat_poststrat_output_all.rda")

#-------------------------------------------------------------------------------
# 1. Clean data 
#-------------------------------------------------------------------------------
community_resilience_c <- community_resilience %>% 
  filter(GEO_LEVEL == "County") %>% 
  mutate(geoid = sub(".*US", "", GEO_ID)) %>% 
  dplyr::select(geoid, contains("PE"))

heat_worry <- average_poststrat_comp %>% 
  dplyr::select(geoid, heat_worry=weighted_average_top_3)

#-------------------------------------------------------------------------------
# 2. Prep data to be mapped 
#-------------------------------------------------------------------------------
community_resilience_m <- community_resilience_c %>%
  left_join(heat_worry, by="geoid") %>% 
  mutate(worry_gap = calc_gap(PRED12_PE, heat_worry, c(-1, 1))) 

community_resilience_g <- merge_county_geo(community_resilience_m, 2021, 5070, "geoid") %>% 
  na.omit()

#-------------------------------------------------------------------------------
# 3. Map data 
#-------------------------------------------------------------------------------
# 10 color bins 
palette_10 <- c("#4575B4", "#77A6CE", "#ABD0E4", "#E0F3F8", "#F4FBD2", "#FEF4AF", "#FEE090",  "#FCA86B", "#EF6E48", "#D73027")

# Bin columns
community_resilience_b <- community_resilience_g %>% 
  add_bins("PRED12_PE", 10) %>% 
  add_bins("worry_gap", 10, 2) 

## Save maps 
map_type <- "comm_resilience"
outpath <- file.path("downscale/heat/output/4_maps", map_type)
create_directory(outpath)

# Resilience map 
title <- "County-level Community Resilience (higher values = more vulnerable)"
legendlabel <- "Vulnerability"
sf_plot_save_county(community_resilience_b, "PRED12_PE_bins", palette_10, 
                    0.35, title, legendlabel, file.path(outpath, "comm_resilience.png"))

# Gap maps
title <- "County-level Comm Resilience Gap"
legendlabel <- "Gap"
sf_plot_save_county(community_resilience_b, "worry_gap_bins", palette_10, 
                    0.35, title, legendlabel, file.path(outpath, "comm_resilience_gap.png"))



