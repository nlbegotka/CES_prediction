# Write function to aggregate poll data to validate output 

prep_validation_poll <- function(poll, geoid_col, geoname_col, response_cols) {
  
  geoid_col <- sym(geoid_col)   # convert strings to symbols to use dplyr operations
  geoname_col <- sym(geoname_col)
  
  validation_poll <- poll %>% 
    mutate(across(all_of(response_cols), ~as.numeric(as.character(.)))) %>% 
    group_by(!!geoid_col, !!geoname_col) %>% 
    summarise(across(c(all_of(response_cols)), mean), count = n()) %>% 
    data.frame() %>% 
    arrange(desc(count)) 
  return(validation_poll)
  
}