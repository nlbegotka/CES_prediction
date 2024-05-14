# Test individual model performance and ensemble model performance
# Last updated: 5/11/2024 by NB 

#-------------------------------------------------------------------------------
# 0. Load objects
#-------------------------------------------------------------------------------
# Libraries
rm(list=ls())
library(ggplot2)
library(dplyr)

# Data
load("output/3_aucs/auc_assessment.rda")

# Functions
source("scripts/0_functions/00_create_directory.R")