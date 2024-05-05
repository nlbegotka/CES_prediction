# This function produces scatter plots to compare modeled opinion with surveyed opinion

scatter_validation_save <- function(df, x_col, y_col, geoname_col, survey_col, output_file) {

  
  scatter_plot <- ggplot(df, aes(x = get(x_col), y = get(y_col), color = as.factor(get(geoname_col)))) +
    geom_point(size=5) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    labs(title = paste("Surveyed vs. modeled", survey_col, "opinion"),
         x = "Surveyed opinion", y = "Modeled opinion") +
    theme_minimal() +
    scale_color_discrete(name = "Region name") 
  
  ggsave(output_file, scatter_plot, width = 11, height = 8, dpi = 900)
  
}


