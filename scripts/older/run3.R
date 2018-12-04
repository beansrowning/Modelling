require(adaptivetau)
require(data.table)
require(Rcpp)
require(parallel)
require(doParallel)
require(foreach)
load("../../Data/gridsearch1_part2.dat")
source("gridsearch1.R")
source("get_popvalues.R")
source("plot.R")
set.seed(1000)
# Run 3
#-------
# Testing initial population size and effective vaccination rate on outbreak length
# Population sizes : 300,000 - 500,000 by 10,000
# Vaccination rates : 0.90 - 1 by 0.01
# Baseline Seroprevalence : 92%
# Case introduction rate : 0.01 (approximately 1 per 100 days)
# Equal introduction likelihood in either group
# Total grid area: 21 x 11 = 231
# Trying reduced depth : 2000 (simulations way too long to start at 10K)
print(paste0("Begining Run 1 - ", date()))
measles_land$parameters["introduction.rate"] <- 0.01
measles_land$parameters["start.time"] <- 0
measles_land$parameters["end.time"] <- 365
measles_land$t2 <- system.time(measles_land$run_2 <- solutionSpace(measles_land,
                                    count = 2000,
                                    insbound = seq(300000, 500000, 10000),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    offset = 2000,
                                    sero.p = c(0.92, 0.92)))
print(paste0("Run 2 done - ", measles_land$t2))
threedPlot(measles_land,
           measles_land$run_2,
           c("max", "median"),
           xlab = "Population Size",
           ylab = "MMR Vaccination Rate (%)",
           zlab = "Outbreak Length (days)",
           title = "Grid Search Run 1 : 92% Seroprevalence")
measles_land$p2 <- measles_land$plot
measles_land$p_d2 <- measles_land$plot_dat
rm(plot_dat, envir = measles_land)
rm(plot, envir = measles_land)
# let's save our progress and be done
print("All Done! - ", date())
save(measles_land, file="../../Data/gridsearch1_part2.dat")
