# Write function to calculate difference between two columns and scale

calc_gap <- function(metric_col, worry_col, gap_range_vec) {
  
  metric_col_scl <- scales::rescale(metric_col, to=c(0, 100)) 
  worry_col_scl <- scales::rescale(worry_col, to=c(0, 100))
  gap_col <- metric_col_scl - worry_col_scl
  gap_col_scl <- scales::rescale(gap_col, to=gap_range_vec)
  return(gap_col_scl)
}