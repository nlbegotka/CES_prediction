# Write functions to save geographic maps for data on state and county level

sf_plot_save_state <- function(data, value_column, palette, title, legendlabel, output_file, width = 11, height = 8, dpi = 900) {
  plt <- ggplot(data) +
    geom_sf(aes(fill = get(value_column))) +
    scale_fill_manual(values = palette, name = legendlabel, guide = guide_legend(reverse = TRUE), drop = FALSE) +
    labs(title = title, x = "", y = "") +
    theme_void() +
    theme(legend.title = element_text(colour = "black", size = 12), 
          legend.text = element_text(colour = "black", size = 10), 
          legend.key.size = unit(1.5, "lines"),
          plot.margin = margin(.6, .6, 1.2, 1.2, "cm"))
  
  ggsave(output_file, plt, width = width, height = height, dpi = dpi)
}


sf_plot_save_county <- function(data, value_column, palette, lwd, title, legendlabel, output_file, width = 11, height = 8, dpi = 900) {
  
  state_shp <- states(year=2022, cb=TRUE) %>% 
    filter(!STATEFP %in% c("02", "15", "60", "66", "69","72", "78"))
  
  plt <- ggplot(data) +
    geom_sf(aes(fill = get(value_column))) +
    geom_sf(data = state_shp, color = "#333333",  fill = NA, lwd = lwd) +
    scale_fill_manual(values = palette_10, name = legendlabel, guide = guide_legend(reverse = TRUE), drop = FALSE) +
    labs(title = title, x = "", y = "") +
    theme_void() +
    theme(legend.title = element_text(colour = "black", size = 12), 
          legend.text = element_text(colour = "black", size = 10), 
          legend.key.size = unit(1.5, "lines"),
          plot.margin = margin(.6, .6, 1.2, 1.2, "cm"))
  
  ggsave(output_file, plt, width = width, height = height, dpi = dpi)
}

