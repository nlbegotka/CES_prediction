# Bin data and create column with labels

add_bins <- function(df, col, bin_n, round_n=0) {
  # create break points
  min_val <- min(df[[col]])
  max_val <- max(df[[col]])
  break_points <- round(seq(min_val, max_val, length.out = (bin_n + 1)), round_n)
  # create column with binned range 
  bin_labels <- paste(head(break_points, -1), tail(break_points, -1), sep = " - ") # starts with lowest
  bin_colname <- paste0(col, "_bins")
  df[[bin_colname]] <- cut(df[[col]], breaks=bin_n, labels=bin_labels) %>% as.factor()
  return(df)
  
}