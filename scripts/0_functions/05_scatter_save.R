# Write functions to output ungrouped and grouped scatterplots 
# These functions add a regression line and R-squared value to the plot 

scatter_save <- function(df, x_col, y_col, title, xlab, ylab, output_file) {
  
  model <- lm(df[[y_col]] ~ df[[x_col]])
  rsquared <- summary(model)$r.squared
  
  scatter_plt <- ggplot(df, aes(x = get(x_col), y = get(y_col))) +
    geom_point(size=2) +
    geom_smooth(method = "lm", se = FALSE, color="black") +
    #geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    labs(title = title,
         x = xlab, y = ylab) +
    theme_minimal() + 
    annotate(
      "text", x = Inf, y = Inf, 
      label = paste("R-squared =", round(rsquared, 3)),
      color = "blue", hjust = 1, vjust = 1, fontface = "bold"
    )
  
  ggsave(output_file, scatter_plt, width = 11, height = 8, dpi = 900)
  
}


scatter_save_reg4 <- function(df, x_col, y_col, group_col, title, xlab, ylab, legendname, output_file) {
  
  # Calculate the R-squared value for each facet
  rsquared_values <- df %>%
    group_by(!!sym(group_col)) %>%
    summarise(rsquared = summary(lm(get(y_col) ~ get(x_col)))$r.squared)
  
  # Create the scatter plot
  scatter_plt <- ggplot(df, aes(x = get(x_col), y = get(y_col), color = as.factor(get(group_col)))) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", se = FALSE, aes(group = get(group_col))) +
    labs(title = title,
         x = xlab, y = ylab) +
    theme_void() + 
    scale_color_discrete(name = legendname) +
    facet_wrap(~get(group_col), ncol = 2, scales = "free") +
    geom_text(data = rsquared_values, 
              aes(label = paste("R² =", round(rsquared, 3))), 
              x = Inf, y = Inf, color = "blue", hjust = 1, vjust = 1, fontface = "bold", 
              size = 5) +
    theme(strip.text = element_text(size = 15)) 
  
  # Save the plot
  ggsave(output_file, scatter_plt, width = 11, height = 8, dpi = 900)
}


scatter_save_reg9 <- function(df, x_col, y_col, group_col, title, xlab, ylab, legendname, output_file) {
  
  # Calculate the R-squared value for each facet
  rsquared_values <- df %>%
    group_by(!!sym(group_col)) %>%
    summarise(rsquared = summary(lm(get(y_col) ~ get(x_col)))$r.squared)
  
  # Create the scatter plot
  scatter_plt <- ggplot(df, aes(x = get(x_col), y = get(y_col), color = as.factor(get(group_col)))) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", se = FALSE, aes(group = {{ group_col }})) +
    labs(title = title,
         x = xlab, y = ylab) +
    theme_void() + 
    scale_color_discrete(name = legendname) +
    facet_wrap(~get(group_col), ncol = 3, scales = "free") +
    geom_text(data = rsquared_values, 
              aes(label = paste("R² =", round(rsquared, 3))), 
              x = Inf, y = Inf, color = "blue", hjust = 1, vjust = 1, fontface = "bold", 
              size = 4) +
    theme(strip.text = element_text(size = 12)) 
  
  # Save the plot
  ggsave(output_file, scatter_plt, width = 11, height = 8, dpi = 900)
}




