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

#-------------------------------------------------------------------------------
# 1. Plot
#-------------------------------------------------------------------------------

# Create df 
model_names <- names(aucs)
auc_values <- c(0.8097, 0.806, 0.7998, 0.5107, 0.5627, 0.8081, 0.8102, 0.81)
auc_df <- data.frame(model = model_names, auc = auc_values) %>%  
  arrange(desc(auc))
auc_df$model <- fct_reorder(auc_df$model, auc_df$auc, .desc = TRUE)

# Create a barplot 
ggplot(auc_df, aes(x = model, y = auc)) +
  geom_bar(stat = "identity", fill = "#B03A2E") +
  geom_text(aes(label = round(auc, 4)), vjust = -0.5) +  # Add labels above bars
  labs(title = "AUC Values for Different Models",
       x = "Model", y = "AUC") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Save

