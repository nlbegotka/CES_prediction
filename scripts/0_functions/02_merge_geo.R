
merge_state_geo <- function(df, year, crs_code, df_geoid_col) {
  state_geoids <- unique(df[[df_geoid_col]])
  state_shp <- states(year=year, cb=TRUE) %>% 
    select(GEOID, geometry) 
  merged_df <- left_join(state_shp, df, by = c("GEOID" = df_geoid_col)) %>% 
    filter(GEOID %in% state_geoids) %>% 
    st_transform(crs = crs_code)
  merged_df$centroid <- st_centroid(merged_df$geometry)
  return(merged_df)
}

merge_county_geo <- function(df, year, crs_code, df_geoid_col) {
  county_geoids <- unique(df[[df_geoid_col]])
  county_shp <- counties(year=year, cb=TRUE)  %>% 
    dplyr::select(GEOID, geometry) 
  merged_df <- left_join(county_shp, df, by = c("GEOID" = df_geoid_col)) %>% 
    filter(GEOID %in% county_geoids) %>% 
    st_transform(crs = crs_code)
  return(merged_df)
}
