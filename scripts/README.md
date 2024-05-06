# Ensemble Baysian Model Averaging (EBMA) for Hazards Downscaling

## Project background 
This is a new YPCCC approach to a classic Multilevel Regression with Poststratification (MRP)
problem by testing 6 machine learning models for variable selection and/or to make 
MRP predictions. To improve our final predictions, we pass all 6 models into an EBMA 
model that makes final predictions that are a weighted average of the 6 models we 
test. 

## File organization 
All files are organized into 3 folders: input, scripts, and output 

### Input 
The input (for now) only includes two poll data frames. Poll_state and poll_county 
contain state and county—level variables respectively. These dfs were created 
in script 08.1_prep_model_data_2.R located in ypccc_hazards/downscale/individual/scripts/1_clean_data. 

### Scripts
Contains all scripts used for the project

### Output
Contains all output including exploratory data analysis (eda) plots and 
model output. 


## Models tested

**Random Forest for MRP** 
Will add more later. 

**XGBoost for MRP** 
Will add more later. 

**Lasso for MRP** 
Will add more later. 

**Support Vector Machines (SVM) for MRP**
Will add more later. 

**PCA for selection and glmer for MRP** 
Will add more later. 

**Best subset for selection and glmer for MRP**
Will add more later. 


