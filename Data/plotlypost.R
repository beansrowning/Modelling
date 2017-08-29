Sys.setenv("plotly_username"="beansrowning")
Sys.setenv("plotly_api_key"="JjiwmDmYu4pZIx40sedg")
require(plotly)
source("../Current/Parameterized/plot.R")
load("latvia1.dat")
latvia <- new.env()
threedPlot(solutions$run_1,
           variables = c("max"),
           cutoff = c(365),
           xlab ="Case Introduction Rate",
           ylab ="MMR Vaccination Rate",
           zlab = "Outbreak Length (days)",
           title = "Latvia - Run 1")
latvia$p1 <- plot
latvia$pd1 <- plot_dat
latvia$run_1 <- solutions$run_1
latvia$t1 <- solutions$t1
save(latvia, file = "latvia.dat")
load("latvia3.dat")

threedPlot(solutions$run_3,
           variables = c("max"),
           cutoff = c(365),
           xlab ="Case Introduction Rate",
           ylab ="MMR Vaccination Rate",
           zlab = "Outbreak Length (days)",
           title = "Latvia - Run 3")
 latvia$p3 <- plot
 latvia$pd3 <- plot_dat
 latvia$run_3 <- solutions$run_3
 latvia$t3 <- solutions$t3
save(latvia, file = "latvia.dat")
# base$z3 <- plot_dat$z
# mask1 <- matrix(365, nrow = nrow(base$z), ncol = ncol(base$z))
# mask2 <- matrix(730, nrow = nrow(base$z), ncol = ncol(base$z))
# mask3 <- matrix(1095, nrow = nrow(base$z), ncol = ncol(base$z))
# plot <- plot_ly(x = base$x,
#                 y = base$y) %>%
#         add_surface(z = base$z) %>%
#         add_surface(z = base$z2, opacity = 0.7) %>%
# 	add_surface(z = base$z3, opacity = 0.7) %>%
#         add_surface(z = mask1, opacity = 0.5, showscale = FALSE) %>%
#         add_surface(z = mask2, opacity = 0.5, showscale = FALSE) %>%
#         add_surface(z = mask3, opacity = 0.5, showscale = FALSE)
# threedPlot(measles_land$run_2_mod1,
#             variables =c("max"),
#             cutoff = c(365, 730, 1095),
#             xlab = "Population Size",
#             ylab = "MMR Vaccination Rate",
#             zlab = "Outbreak Length (days)",
#             title = "Measles Land: Younger Cases")
# measles_land$p2_mod1 <- plot
# measles_land$pd2_mod1 <- plot_dat
# swe <- new.env()
# threedPlot(solutions$run_1,
#             variables =c("max"),
#             xlab = "Case Insertion Rate",
#             ylab = "MMR Vaccination Rate",
#             zlab = "Outbreak Length (days)",
#             title = "Sweden - Grid Search Run 1")
# swe$p1 <- plot
# swe$pd1 <- plot_dat

# threedPlot(solutions$run_2,
#             variables =c("max"),
#             xlab = "Case Insertion Rate",
#             ylab = "MMR Vaccination Rate",
#             zlab = "Outbreak Length (days)",
#             title = "Sweden - Grid Search Run 2")
# swe$p2 <- plot
# swe$pd2 <- plot_dat

# threedPlot(solutions$run_3,
#             variables =c("max"),
#             xlab = "Case Insertion Rate",
#             ylab = "MMR Vaccination Rate",
#             zlab = "Outbreak Length (days)",
#             title = "Sweden - Grid Search Run 3")
# swe$p3 <- plot
# swe$pd3 <- plot_dat
# save(measles_land, file="sense_and_sensitivity.dat")
# api_create(plot, filename = "Meales Land: 92% Seroprevalence (combined)", fileopt = "overwrite")
api_create(latvia$p1, filename = "Latvia Run 1 - Equal Case Introduction", fileopt = "overwrite")
api_create(latvia$p3, filename = "Latvia Run 3 - Younger Case Introduction", fileopt = "overwrite")
