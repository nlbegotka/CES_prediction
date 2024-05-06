# Clean CES survey data
# Load CES_prediction.Rproj
# Last updated: 5/5/24 by NB

#-------------------------------------------------------------------------------
# 0. Load objects 
#-------------------------------------------------------------------------------
rm(list=ls())

# Packages 
library(dplyr)

# Data 
CES_data <- read.csv("input/CCES22_Common_OUTPUT_vv_topost_sample.csv") %>% 
  dplyr::select(-c(X, X.1)) # remove row labels

#-------------------------------------------------------------------------------
# 1. Select features known to affect climate change opinion -- rename for clarity 
#-------------------------------------------------------------------------------
CES_data_c <- CES_data %>% 
  dplyr::select(birthyr, 
                gender4, 
                educ, 
                race, hispanic,
                votereg, 
                pid3, 
                state_fips=inputstate, region4=region, 
                news_engagement=newsint, 
                cit1, # U.S. citizen
                urbancity, 
                investor, 
                pew_bornagain, 
                religpew, 
                marstat, 
                union, 
                ownhome, 
                family_income=faminc_new, 
                contains("milstat"), 
                child_parent=child18, 
                sexuality, transgender, 
                climate_change=CC22_333)

# Remove any features accidentally selected
CES_data_c2 <- CES_data_c %>% 
  dplyr::select(-contains("timing")) 


#-------------------------------------------------------------------------------
# 2. Do light feature engineering that can be applied to entire data set 
#-------------------------------------------------------------------------------
# First explore missingness 
apply(CES_data_c2, 2, \(x) sum(is.na(x)))

# Find variables correlated with being hispanic 
correlations <- cor(na.omit(CES_data_c2)) %>% 
  data.frame() %>% 
  dplyr::select(hispanic)


cor_df <- data.frame(
  Feature1 = rep("hispanic", length(correlations)),
  Feature2 = names(correlations),
  Correlation_Coefficient = correlations
)






# Create age feature
# Clarify gender feature 
# create white non-latino feature 
# use hisp feature
# 

CES_data_fe <- CES_data_c2 %>% 
  mutate(
    age = (2022 - birthyr), 
    gender = case_when(
      gender4 == 1 ~ "Male",
      gender4 == 2 ~ "Female",
      gender4 == 3 ~ "Non-binary",
      TRUE ~ "Other"
    ), 
    college_degree = ifelse(educ %in% c(4, 5, 6), 1, 0),
    white_non_hisp = ifelse(race == 1 & hispanic == 2, 1, 0),
    hispanic = ifelse(hispanic == 1, 1, 0), 
    votereg = ifelse(votereg == 1, 1, 0), 
    pol_identity = case_when(
      pid3 == 1 ~ "Democrat",
      pid3 == 2 ~ "Republican",
      pid3 == 3 ~ "Independent",
      TRUE ~ "Other"
    ), 
    news_engaged = ifelse(news_engagement %in% c(1, 2), 1, 0), 
    citizen = ifelse(cit1 == 1, 1, 0), 
    urban_resident = ifelse(urbancity == 1, 1, 0), 
    rural_resident = ifelse(urbancity == 4, 1, 0), 
    investor = ifelse(investor == 1, 1, 0), 
    evangelical = ifelse(pew_bornagain == 1, 1, 0), 
    married = ifelse(marstat == 1, 1, 0),
    roman_catholic = ifelse(religpew == 2, 1, 0),
    union_member = ifelse(union == 1, 1, 0), 
    home_owner = ifelse(ownhome == 1, 1, 0), 
    income_bucket = case_when(
      family_income %in% 1:3 ~ "Less than $30,000",
      family_income %in% 4:6 ~ "$30,000 - $59,999",
      family_income %in% 7:9 ~ "$60,000 - $99,999",
      family_income %in% 10:16 ~ "$100,000 or more"
    ), 
    military_service = ifelse(milstat_1 == 1 | milstat_3 == 1, 1, 0), 
    child_parent = ifelse(child_parent == 1, 1, 0),
    heterosexual = ifelse(sexuality == 1, 1, 0), 
    transgender = ifelse(transgender == 1, 1, 0), 
    climate_change_belief = ifelse(climate_change %in% c(1, 2), 1, 0)
  )

# Select only the newly created features
CES_data_fe <- CES_data_fe %>% 
  dplyr::select(age, gender, college_degree, white_non_hisp, hispanic, 
                votereg, pol_identity, news_engaged, citizen, 
                urban_resident, rural_resident, investor, 
                evangelical, married, union_member, home_owner, 
                income_bucket, military_service, child_parent, heterosexual, 
                transgender, climate_change_belief)

  

# create var for whether watched fox news 
# create var for whether watched msnbc or cnn 
# CC22_300d, whether or not they posted something political
# 


## 2.1.  ---------------------------------------------------------------





## 1.2.  --------------------------------------------------------------


## 1.3.  ---------------------------------------------------------------


#-------------------------------------------------------------------------------
# 2. 
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# 3.  
#-------------------------------------------------------------------------------


