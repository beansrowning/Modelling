# Example density plot
require("plotly")
require("data.table")
# Sys.setenv("plotly_username"="beansrowning")
# Sys.setenv("plotly_api_key"="<secret>")
#---Load data---------------------------------------------------------------
load("tworuns.dat")
#---Plotly requires a different layout--------------------------------------
solutions$plot_dat_1 <- list(x = unique(solutions$run_1[, ins]),
                             y = unique(solutions$run_1[, vacc]),
                             z = rbind(solutions$run_1[vacc == 0.94][, max],
                                       solutions$run_1[vacc == 0.95][, max]))
solutions$plot_dat_2 <- list(x = unique(solutions$run_2[, ins]),
                             y = unique(solutions$run_2[, vacc]),
                             z = rbind(solutions$run_2[vacc == 0.93][, max],
                                       solutions$run_2[vacc == 0.94][, max],
                                       solutions$run_2[vacc == 0.95][, max],
                                       solutions$run_2[vacc == 0.96][, max]))
font <- list(family = "Courier New, Monospace",
             size = 14,
             color = "#7f7f7f")
solutions$p1 <- plot_ly(x = solutions$plot_dat_1$x,
              y = solutions$plot_dat_1$y,
              z = solutions$plot_dat_1$z) %>% add_surface() %>%
              layout(title = "Hyperparameter Space Run 1",
                     scene = list(xaxis = list(title = "Introduction Rate",
                                               titlefont = font),
                                  yaxis = list(title = "Vaccine Coverage (%)",
                                               titlefont = font),
                                  zaxis = list(title = "Maximum Outbreak Length (days)",
                                               titlefont = font)))
solutions$p2 <- plot_ly(x = solutions$plot_dat_2$x,
              y = solutions$plot_dat_2$y,
              z = solutions$plot_dat_2$z) %>% add_surface() %>%
              layout(title = "Hyperparameter Space Run 2",
                     scene = list(xaxis = list(title = "Introduction Rate",
                                               titlefont = font,
                                               nticks = 8,
                                               range = c(0.01, 0.08)),
                                  yaxis = list(title = "Vaccine Coverage (%)"),
                                  zaxis = list(title = "Maximum Outbreak Length (days)")))
#---Push to Plotly and save locally---------
api_create(solutions$p1, filename = "Run_1")
api_create(solutions$p2, filename = "Run_2")
save(solutions, file = "../../Data/tworuns.dat")
