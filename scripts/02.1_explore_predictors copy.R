# Explore predictors that are best for predicting heat worry 
# load ypccc_hazards.Rproj!
# Last updated: 4/17/2024 by NLB 

#-------------------------------------------------------------------------------
# 0. Load objects 
#-------------------------------------------------------------------------------
rm(list=ls())
library(dplyr)
library(ggplot2)
library(corrplot)
library(stringr)
library(sjPlot)
library(tigris) # for maps 
library(sf) # for maps 
library(tools) # for toTitleCase
library(moments) # for skewness function
library(caret) # for Yeo-Johnson transformations

drop_basepath <- "~/Dropbox (YSE)/" # dynamic
load("downscale/heat/temp/poll_state_heat.rda")
load("downscale/heat/temp/poll_county_heat.rda")
# function
source("downscale/heat/scripts/0_functions/00_create_directory.R")

# xwalk
load("_data/xwalks/state_region_xwalk_2023.rda")

#-------------------------------------------------------------------------------
# 1. Prep data 
#-------------------------------------------------------------------------------
# Add stusps to poll_state
state_xwalk <- state_region_xwalk %>% 
  dplyr::select(geoid, stusps) %>% 
  filter(!geoid %in% c("02", "15"))

poll_state <- poll_state %>% 
  left_join(state_xwalk, by="geoid")

# Create lists to iterate over 
levels <- c("state", "county")
polls <- list(poll_state, poll_county) %>% 
  setNames(levels)

#-------------------------------------------------------------------------------
# 2. Explore dv_heat 
#-------------------------------------------------------------------------------
## 2.1 Response proportions ----------------------------------------------------
# Plot proportion of 0/1 responses for dv -- same for state and county 
dv <- "dv_heat"
bp <- poll_state %>%
  plot_frq(!!sym(dv), title=paste0(dv, " worry"))

directory_path <- "downscale/heat/temp/EDA/dv/"
create_directory(directory_path)
save_plot(filename=paste0(directory_path, dv, ".jpg"), fig=bp, width=15, height=10)



## 2.1 Response across states --------------------------------------------------
## Bar plot of heat worry for each state 
# Create df
state_dv_pct <- poll_state %>%
  dplyr::select(stusps, !!sym(dv)) %>% 
  mutate(!!sym(dv) := as.numeric(as.character(!!sym(dv)))) %>% 
  group_by(stusps, !!sym(dv)) %>%
  summarise(count = n(), .groups = 'drop') %>% 
  group_by(stusps) %>% 
  mutate(proportion = count / sum(count)) %>% 
  filter(!!sym(dv)==1) %>% 
  arrange(desc(proportion)) %>% 
  dplyr::select(stusps, proportion) %>% 
  mutate(percent = round(proportion, 2) *100) %>% 
  data.frame()

# Plot bar plot of state heat worry in descending order
bp_state <- ggplot(state_dv_pct, aes(y = reorder(stusps, percent), x = percent)) +
  geom_bar(stat = "identity", fill="#FFA500") +
  labs(title = paste0("State ", str_remove(dv, "dv_"), " worry in descending order"), 
       y = "State", 
       x = "% worry") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("downscale/heat/temp/EDA/dv/state_worry_barplot.png", plot= bp_state, width=6, height=10)   

## Map of state heat worry 
state_shp <- states(year=2022, cb=TRUE) 
state_dv_pct_geo <- left_join(state_shp, state_dv_pct, by = c("STUSPS" = "stusps")) %>% 
  filter(STUSPS %in% state_dv_pct$stusps) %>% 
  st_transform(crs = 5070) %>% 
  mutate(centroid = st_centroid(geometry))


(state_worry_geo <- ggplot(state_dv_pct_geo) +
    geom_sf(aes(fill = percent), lwd=0.35) +
    geom_sf_text(aes(label = percent, geometry = centroid), check_overlap = TRUE, size = 3) + 
    scale_fill_gradient(low = "whitesmoke", high ="#FFA500", name = "% worry") +  # You can choose a different palette if you prefer
    labs(title = paste0(toTitleCase(str_remove(dv, "dv_")), " worry by state"),
         fill = "Worry", 
         x="", y="") +
    theme_minimal())

# Save 
ggsave("downscale/heat/temp/EDA/dv/state_worry_map.png", plot= state_worry_geo, width=10, height=6)



#-------------------------------------------------------------------------------
# 3. Explore nominal (categorical) predictors across the dv
#-------------------------------------------------------------------------------
## Plot dv proportions for factor variables -- reg4/9, race, age, sex, and geoid 

# function to find the proportions of worry (0/1) in each group for a given var
calc_response_proportion <- function(df, var, dv) {
  df_prop <- df %>%
    group_by(!!sym(var), !!sym(dv)) %>%
    summarise(count = n(), .groups = 'drop') %>% 
    group_by(!!sym(var)) %>% 
    mutate(proportion = count / sum(count))
  return(df_prop)
}

# function to produce facet plots visualizing the df calculated in calc_response_proportion
create_facet_barplots <- function(df, var, dv, proportion_col, nrow) {
  fp <- ggplot(df, aes(x = !!sym(dv), y = !!sym(proportion_col)*100, fill=!!sym(dv))) +
    geom_bar(stat="identity", position = position_dodge()) +
    geom_text(aes(label = scales::percent(!!sym(proportion_col), accuracy = 1), y = !!sym(proportion_col)*100), 
              position = position_dodge(width = 0.9), 
              vjust = -1) +
    ylim(0, 100) +
    facet_wrap(as.formula(paste0("~", var)), nrow = nrow, scales = "free_x") +
    scale_fill_manual(values = c("0" = "#3ed5c4", "1" = "#D53E4F")) +
    labs(title = paste0(dv, " worry by ", var),
         x = dv,
         y = "percent in response group") 
  return(fp)
}

# Plot for state and county levels 
vars_to_plot <- c("reg4", "reg9", "sex", "race", "age_group")
nrows <- c(2, 3, 1, 2, 2)

lapply(levels, function(level){
  
  directory_path <- paste0("downscale/heat/temp/EDA/predictors/", level, "/dv_analysis/")
  create_directory(directory_path) 
  
  lapply(seq_along(vars_to_plot), function(idx) {
    fp <- polls[[level]] %>% 
      calc_response_proportion(var=vars_to_plot[idx], dv=dv) %>% 
      create_facet_barplots(var=vars_to_plot[idx], dv=dv, 
                            proportion_col="proportion", nrow=nrows[idx])
    file_path <- paste0(directory_path, vars_to_plot[idx], ".png")
    ggsave(file_path, plot = fp, width = 10, height = 6)
  })
  
})


#-------------------------------------------------------------------------------
# 4. Explore numeric predictors across the dv
#-------------------------------------------------------------------------------
# Make facet plots like in section 3 

polls_scl <- lapply(polls, function(df) {
  df_hist <- df %>% 
    mutate(!!sym(dv) := ifelse(!!sym(dv) == 1, "worried", "not worried")) %>% 
    mutate_if(is.numeric, scale)
  return(df_hist)
})


create_facet_plots_hist <- function(df, var, dv, binwidth) {
  dv_label <- sub("dv_", "", dv)
  fp <- ggplot(df, aes(x = !!sym(var), fill=!!sym(dv), col=!!sym(dv))) +
    geom_density(alpha=0.5) +
    scale_fill_manual(values = c("#3ed5c4", "#D53E4F")) +
    scale_color_manual(values = c("#3ed5c4", "#D53E4F")) +
    labs(title = paste0(dv_label, " worry across ", var),
         x = var,
         y = "Frequency")
  return(fp)
}



# Make facet plots for all numeric predictors on both levels
lapply(levels, function(level){
  directory_path <- paste0("downscale/heat/temp/EDA/predictors/", level, "/dv_analysis/")
  create_directory(directory_path)
  
  numeric_vars <- polls_scl[[level]] %>% dplyr::select_if(is.numeric) %>% names()
  # flood_vars_nc
  lapply(numeric_vars, function(var) {
    fp <- polls_scl[[level]] %>% 
      create_facet_plots_hist(var=var, dv=dv, binwidth=0.05)
    file_path <- paste0(directory_path, var, ".png")
    ggsave(file_path, plot = fp, width = 10, height = 6)
  })
  
})


#-------------------------------------------------------------------------------
# 5. Explore numeric predictors - distributions & transformations
#-------------------------------------------------------------------------------
## 5.1 Find skewness of predictors ---------------------------------------------
# This will inform which transformations (if any) we should apply to each predictors 

## First, apply cleaning steps/scaling we will be applying when modeling
model_formula <- as.formula(paste0(dv, " ~ ."))

polls_processed <- lapply(levels, function(level){
  recipe <- polls[[level]] %>%
    recipe(model_formula) %>% # define model formula
    step_select(all_numeric_predictors()) %>% 
    step_nzv(all_predictors(), freq_cut = 95/5) %>%  # remove low-variance predictors
    step_lincomb(all_predictors()) %>% # removes linear combinations
    step_corr(threshold = 0.9)  # removes correlated predictors
  
  poll_processed <- recipe %>% prep() %>% juice()
  return(poll_processed)
}) %>% 
  setNames(levels)


# Find skewness of all predictors 
calc_skewness <- function(df) {
  skew_vec <- skewness(df)
  names <- names(skew_vec)
  values <- unname(skew_vec)
  df_skew <- data.frame(predictor = names,
                        value = values)
  return(df_skew)
}

predictor_skewness <- lapply(polls_processed, function(df) {
  calc_skewness(df)
  }) %>%
  setNames(levels)


# Separate into left and right skewed predictors
find_right_skewed <- function(skewness_df, skew_col, threshold) {
  skewed_predictors <- skewness_df %>%
    dplyr::filter(!!sym(skew_col) > threshold) %>%
    pull(predictor)
  
  return(skewed_predictors)
}


find_left_skewed <- function(skewness_df, skew_col, threshold) {
  skewed_predictors <- skewness_df %>%
    dplyr::filter(!!sym(skew_col) < threshold) %>%
    pull(predictor)
  
  return(skewed_predictors)
}

polls_right_skewed <- lapply(levels, function(level) {
  skewed_predictors <- find_right_skewed(predictor_skewness[[level]], "value", threshold=0.5)
  df <- polls_processed[[level]] %>%
    dplyr::select(all_of(skewed_predictors))
  return(df)
}) %>%
  setNames(levels)



# Apply over levels because we need to use two dfs
polls_left_skewed <- lapply(levels, function(level){
  skewed_predictors <- find_left_skewed(predictor_skewness[[level]], "value", threshold=-0.5)
  df <- polls_processed[[level]] %>%
    dplyr::select(all_of(skewed_predictors))
  return(df)
}) %>%
  setNames(levels)


## 5.2 Define transformations to apply  ----------------------------------------
# Check the range for all the predictors we will be using -- the minimum value = 0 
sapply(polls_processed$state, range)
sapply(polls_processed$county, range)


# Define transformations to apply to right and left skewed predictors
transformations_right <- list(
  log = function(x) log(x + 1), # to handle 0s
  sqrt = function(x) sqrt(x + 1) # to handle 0s 
)

transformations_left <- list(
  square = function(x) x**2
  #exp = function(x) exp(x)  # remove exponential transformation bc it caused problems
  
)


# Write function to apply transformations specified above
transform_predictors <- function(df, transformations) {
  for(predictor in names(df)){
    for(tf in names(transformations)) {
      tf_col_name <- paste(predictor, tf, sep = "_")
      df[[tf_col_name]] <- transformations[[tf]](df[[predictor]])
    }
    new_name <- paste0(predictor, "_none") # rename original cols to indicate no applied transformation 
    names(df)[names(df) == predictor] <- new_name
  }
  return(df)
  
}

# Write function to apply the Yeo-Johnson transformation -- can be applied to both right and left skewed
# Because the functions's input is a df, we write a separate function
YeoJohnson_transform <- function(df) {
  preprocessed_pred <- preProcess(df, method = "YeoJohnson")
  transformed_df <- predict(preprocessed_pred, df)
  names(transformed_df) <- paste0(names(transformed_df), "_yeojohnson")
  return(transformed_df)
}


## 5.3 Apply transformations ---------------------------------------------------
polls_right_transformed <- lapply(polls_right_skewed, function(df) {
  df1 <- transform_predictors(df, transformations_right)
  df2 <- YeoJohnson_transform(df)
  df3 <- cbind(df1, df2)
  return(df3)
}) %>%
  setNames(levels)

polls_left_transformed <- lapply(polls_left_skewed, function(df) {
  df1 <- transform_predictors(df, transformations_left)
  df2 <- YeoJohnson_transform(df)
  df3 <- cbind(df1, df2)
  return(df3)
}) %>%
  setNames(levels)


## 5.3 Scale data to plot distributions of all transformations  ----------------
scale_df <- function(df) {
  df_scl <- sapply(df, scale) %>% 
    data.frame()
  return(df_scl)
}

polls_right_transformed <- lapply(polls_right_transformed, scale_df) %>% 
  setNames(levels)

polls_left_transformed <- lapply(polls_left_transformed, scale_df) %>% 
  setNames(levels)

## 5.4 Make violin plots  ------------------------------------------------------
# Write a function to apply to both left and right skewed lists
plot_violin <- function(df, level, directory_path) {
  long_df <- pivot_longer(df,
                          cols = everything(),
                          names_to = c("predictor", "transformation"),
                          names_pattern = "(.*)_(.*)")
  for (pred in unique(long_df$predictor)) {
    df_predictor <- filter(long_df, predictor == pred)
    vp <- ggplot(df_predictor, aes(x = transformation, y = value, fill = transformation)) +
      geom_violin() +
      geom_boxplot(width = 0.1, fill = "white") +
      labs(title = pred,
           x = "Transformation",
           y = "Value")
    
    file_path <- paste0(directory_path, pred, ".png")
    ggsave(file_path, plot = vp, width = 10, height = 6)
    
  }
}


# Make plots of transformations for right skewed predictors
lapply(levels, function(level) {
  directory_path <- paste0("downscale/heat/temp/EDA/predictors/", level, "/transformations/")
  create_directory(directory_path) # only do once 
  df <- polls_right_transformed[[level]]
  plot_violin(df, level, directory_path)
}) %>% 
  setNames(levels)

# Make plots of transformations for left skewed predictors
lapply(levels, function(level) {
  directory_path <- paste0("downscale/heat/temp/EDA/predictors/", level, "/transformations/")
  df <- polls_left_transformed[[level]]
  plot_violin(df, level, directory_path)
}) %>% 
  setNames(levels)



#-------------------------------------------------------------------------------
# 6. Conclusions 
#-------------------------------------------------------------------------------
# 1. Look at the geom density plots to see which predictors might predict dv_heat 
# 2. Take note of which transformations normalize predictor distributions below 

# The code below can apply these transformations to variables of interest. 
# In the current pipeline, we Yeo-Johnson transform all variables so this step
# is not performed but it can be in the future 

# Note which predictors are improved by transformations
to_sqrt_tf <- c("fema_HWAV_EXPT")
to_square_tf <- c("y_aircon")
to_yeojohnson_tf <- c("cc_heat_threshold_f", "fema_HWAV_AFREQ", "fema_HWAV_EALB", 
                     "fema_HWAV_EALT", "fema_HWAV_EVNTS", "y_mean_impervious")


# Write function to apply transformations 
apply_transformations <- function(df) {
  for (pred in names(df)) {
    if (pred %in% to_square_tf) {
      df[[pred]] <- df[[pred]]**2
    } else if (pred %in% to_yeojohnson_tf) {
      df[[pred]] <- bestNormalize::yeojohnson(df[[pred]], warn = FALSE)$x.t
    } else if (pred %in% to_sqrt_tf) {
      df[[pred]] <- sqrt(df[[pred]] + 1)
    }
  }
  return(df)
}


# Apply transformations
polls_transformed <- lapply(polls, apply_transformations) %>% 
  setNames(levels)


#-------------------------------------------------------------------------------
# 7. Explore interactions between numeric and categorical predictors
#-------------------------------------------------------------------------------
# Could be done in the future! 






















