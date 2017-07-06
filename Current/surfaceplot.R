# 3d Surface plot
# AKA RAM/CPU Eater Delux
# Use at your own risk

require("ggplot2")

surface_plot <- function(results) {
  results$I <- results$I + 1
  graph <- ggplot(results, aes(time, iter, z = I)) +
            geom_point(aes(colour = I)) +
            stat_density2d() +
            theme(panel.background = element_rect(fill = "#132B43"),
                  panel.grid = element_blank())
  plot(graph)
}
