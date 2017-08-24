require(plotly)
source("makeplotdat.R")
load("../../Data/gridsearch1_part2.dat")
measles_land$plot_dat <- makeplotdat(measles_land$run_3)
measles_land$p1 <- plot_ly(x = measles_land$plot_dat$x,
              y = measles_land$plot_dat$y,
              z = measles_land$plot_dat$z) %>% add_surface() %>%
              layout(title = "Grid Search Run 1 (96% Seroprevalence)",
                     scene = list(xaxis = list(title = "Population Size"),
                                  yaxis = list(title = "MMR Coverage (%)"),
                                  zaxis = list(title = "Maximum Outbreak Length (days)")))
save(measles_land, file = "../../Data/gridsearch1_part2.dat")
measles_land$p1
