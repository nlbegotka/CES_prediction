# Predicting climate opinion: a new approach to Multilevel Regression with Postratification (MRP)

## Project background 
This project is adapted from a project that incorporates machine learning models and ensembling into a traditional MRP workflow. MRP is a common approach used to model survey data for geographies (such as states or counties for the U.S.). This is a useful technique because it is often a challenge to obtain representative survey data, especially for smaller geographies. 

In a traditional MRP workflow, a Multilevel Regression model is built on survey data to predict individual-level opinion and then the predicted individual-level opinions are poststratified (weighted and aggregated) to construct opinion estimates for geographies of interest. In this workflow, 5 models are constructed to predict individual-level opinion: 
- Random Forest model
- XGBoost model
- Elastic net model
- Mixed model with PCA selection
- Mixed model with Best Subsets selection
  
The model predictions are assessed by splitting the survey data into train and test splits and calculating performance metrics. The 5 model predictions are ensembled by taking weighted averages of each model's prediction. The ensembled predictions are assessed, as well. The entire workflow is visually explained in the image below. 

![Example Image](background/model_flowchart.png)

This project uses survey data from the [Cooperative Election Study](https://cces.gov.harvard.edu/) (CES) administered by YouGov in 2022 to predict whether an individual is worried about climate change (1) or not (0). External data from the CDC and NOAA are merged to the CES survey data. This project omits the poststratification step of MRP but proposes an alternative to using a single Multilevel Regression model. 

## File organization 
All files are organized into 5 folders: 
- **background**: contains background information on the survey data used 
- **input**: contains all raw input data 
- **scripts**: contains all scripts used in this project
- **temp**: contains temporary data created by scripts
- **output**: contains the final project output, which is the performance metrics for each model 

