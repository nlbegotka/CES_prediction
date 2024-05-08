# Clean CES survey data
# Load CES_prediction.Rproj
# Last updated: 5/7/24 by NB

#-------------------------------------------------------------------------------
# 0. Load objects 
#-------------------------------------------------------------------------------
rm(list=ls())

# Libraries 
library(dplyr)
library(stringr)

# Data 
CES_data <- read.csv("input/CCES22_Common_OUTPUT_vv_topost_sample.csv") %>% 
  dplyr::select(-c(X, X.1)) # remove row labels - error
# Read in other numeric data to merge 
noaa_disaster_df <- read.csv("input/noaa_state_disaster_cost.csv")
social_vulnerability_df <- read.csv("input/SVI_2020_US_county.csv")
state_populations <- read.csv("input/state_populations.csv", colClasses = c(state_fips = "factor") )

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
                parent=child18, 
                sexuality, 
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
# None are correlated highly, impute missing with "unknown" group 
correlations <- cor(na.omit(CES_data_c2)) %>% 
  data.frame() %>% 
  dplyr::select(hispanic)

cor_df <- data.frame(
  Feature1 = rep("hispanic", length(correlations)),
  Feature2 = names(correlations),
  Correlation_Coefficient = correlations
)

# Clean features 
# Impute by classifying missing classes as "Other/Unknown" or as the 0 class for features missing a few observations

CES_data_fe <- CES_data_c2 %>% 
  mutate(
    state_fips = str_pad(state_fips, width=2, pad="0"), 
    age = (2022 - birthyr), 
    gender = case_when(
      gender4 == 1 ~ "Male",
      gender4 == 2 ~ "Female",
      gender4 == 3 ~ "Non-binary",
      TRUE ~ "Other"
    ), 
    college_degree = ifelse(educ %in% c(4, 5, 6), "Degree", "No Degree"),
    hispanic = case_when(
      hispanic == 1 ~ "Hispanic",
      hispanic == 2 ~ "Non-Hispanic",
      TRUE ~ "Unknown"
    ),
    white_non_hisp = ifelse(race == 1 & hispanic == "Non-Hispanic", "White", "Non-White"),
    votereg = ifelse(votereg == 1, "Registered"," Unregistered"), 
    pol_identity = case_when(
      pid3 == 1 ~ "Democrat",
      pid3 == 2 ~ "Republican",
      pid3 == 3 ~ "Independent",
      TRUE ~ "Other"
    ), 
    news_engaged = ifelse(news_engagement %in% c(1, 2), "Engaged", "Disengaged"), 
    citizen = case_when(
      cit1 == 1 ~ "Citizen", 
      cit1 == 2 ~ "Non-Citizen", 
      TRUE ~ "Other"
    ), 
    urban_resident = ifelse(urbancity == 1, "Urban", "Non-Urban"), 
    rural_resident = ifelse(urbancity == 4, "Rural", "Non-Rural"), 
    investor = case_when(
      investor == 1 ~ "Investor", 
      investor == 2 ~ "Non-Investor", 
      TRUE ~ "Other"
    ),  
    evangelical = case_when(
      pew_bornagain == 1 ~ "Evangelical", 
      pew_bornagain == 2 ~ "Non-Evangelical", 
      TRUE ~ "Other"
    ),
    married = ifelse(is.na(marstat) | marstat != 1, "Married", "Unmarried"),
    union_member = ifelse(is.na(union) | union != 1, "Union", "Non-Union"), 
    home_owner = ifelse(is.na(ownhome) | ownhome != 1, "Owner", "Non-Owner"), 
    income_bucket = case_when(
      family_income %in% 1:3 ~ "Less than $30,000",
      family_income %in% 4:6 ~ "$30,000 - $59,999",
      family_income %in% 7:9 ~ "$60,000 - $99,999",
      family_income %in% 10:16 ~ "$100,000 or more", 
      TRUE ~ "Other"
    ), 
    military_service = ifelse(milstat_1 == 1 | milstat_3 == 1, "Military", "Non-Military"), 
    parent = case_when(
      parent == 1 ~ "Parent", 
      parent == 2 ~ "Non-Parent", 
      TRUE ~ "Other"
    ),
    heterosexual = ifelse(is.na(sexuality) | sexuality != 1, "Non-Heterosexual", "Heterosexual"), 
    climate_change_worry = ifelse(climate_change %in% c(1, 2), "1", "0")
  )


# Select clean and newly cleaned features 
CES_data_fe <- CES_data_fe %>% 
  dplyr::select(state_fips, age, gender, college_degree, white_non_hisp, hispanic, 
                votereg, pol_identity, news_engaged, citizen, 
                urban_resident, rural_resident, investor, 
                evangelical, married, union_member, home_owner, 
                income_bucket, military_service, parent, heterosexual, 
                climate_change_worry)

  
# Check for missingness again -- looks good 
apply(CES_data_fe, 2, \(x) sum(is.na(x)))

# Check classes of data -- a lot of nominal data 
sapply(CES_data_fe, class)

# Convert characters to factors and state_fips to factor 
CES_data_fe <- CES_data_fe %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(state_fips = as.factor(state_fips))

# Confirm data types are correct
sapply(CES_data_fe, class)

#-------------------------------------------------------------------------------
# 3. Merge in state-level aggregated numeric features 
#-------------------------------------------------------------------------------
# Calc cost per-capita of natural disaster damage -- could impact climate worry
noaa_pc_df <- noaa_disaster_df %>% 
  mutate(state_fips = str_pad(state_fips, width=2, pad="0")) %>% 
  mutate(total_cost = rowSums(select(., -state_fips))) %>% 
  left_join(state_populations, by="state_fips") %>% 
  mutate(noaa_cost_pc = total_cost/as.numeric(population)) %>% 
  dplyr::select(state_fips, noaa_cost_pc)

# Calc state-level social vulnerability -- could impact climate worry
svi_data_state <- social_vulnerability_df %>% 
  mutate(ST = as.factor(ST)) %>% 
  mutate(state_fips = str_pad(ST, width=2, pad="0"), 
         cdc_svi_socioecon = RPL_THEME1, 
         cdc_svi_demo = RPL_THEME2, 
         cdc_svi_race = RPL_THEME3, 
         cdc_svi_housing = RPL_THEME4) %>% 
  dplyr::select(state_fips, cdc_svi_socioecon, 
                cdc_svi_demo, cdc_svi_race, 
                cdc_svi_housing) %>% 
  group_by(state_fips) %>% 
  summarise_all(mean) %>% 
  data.frame()

# Merge variables
poll_clean <- CES_data_fe %>% 
  left_join(noaa_pc_df, by="state_fips") %>% 
  left_join(svi_data_state, by="state_fips") %>% 
  relocate(climate_change_worry, .after=cdc_svi_housing)

# Confirm no missing values and correct data types one final time
apply(poll_clean, 2, \(x) sum(is.na(x)))
sapply(poll_clean, class)

# Reconvert state_fips to factor
poll_clean$state_fips <- as.factor(poll_clean$state_fips)
sapply(poll_clean, class)

#-------------------------------------------------------------------------------
# 4. Save 
#-------------------------------------------------------------------------------
write.csv(poll_clean, "temp/poll_clean.csv")
save(poll_clean, file = "temp/poll_clean.rda")
