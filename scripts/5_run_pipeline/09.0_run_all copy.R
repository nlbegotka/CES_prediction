# Run all mapping scripts at once 
# Working in ypccc_hazards.Rproj
# last modified: 03/27/2024, NLB

#-------------------------------------------------------------------------------
# 1. Run scripts 
#-------------------------------------------------------------------------------
rm(list=ls())

## 1.1. Map YPCCC heat worry  --------------------------------------------------
source(file.path("downscale/heat/county/scripts/9_maps/", "09.1_worry_output.R"))

## 1.2. Map Climate Check heat risk  -------------------------------------------
source(file.path("downscale/heat/county/scripts/9_maps/", "09.2_cc_rating.R"))

## 1.3. Map Climate Check gap  -------------------------------------------------
source(file.path("downscale/heat/county/scripts/9_maps/", "09.3_risk_worry_gap.R.R"))

## 1.4. Map HVI and gap  -------------------------------------------------------
source(file.path("downscale/heat/county/scripts/9_maps/", "09.4_HVI.R"))

## 1.5. Map CVI score, mortality, and gaps  ------------------------------------
source(file.path("downscale/heat/county/scripts/9_maps/", "09.5_CVI.R"))

## 1.6. Map Howe heat worry and gap  -------------------------------------------
source(file.path("downscale/heat/county/scripts/9_maps/", "09.6_howe_comparison.R"))
