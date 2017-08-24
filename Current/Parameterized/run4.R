require(adaptivetau)
require(data.table)
require(Rcpp)
require(parallel)
require(doParallel)
require(foreach)
# measles_land <- new.env()
# measles_land$init.values <- c(
#             S = c(4800, 43200),
#             E = c(0, 0),
#             I = c(0, 0),
#             R = c(43200, 226800))
#
# measles_land$transitions <- ssa.maketrans(c("S1", "E1", "I1", "R1", "S2", "E2", "I2", "R2"),
# rbind(c("S1", "E1", "I1"), -1, c("E1", "I1", "R1"), +1),
# rbind(c("S1", "R1", "I1", "I2"), +1),
# rbind(c("S1", "E1", "I1", "R1"), -1, c("S2", "E2", "I2", "R2"), +1),
# rbind(c("S2", "E2", "I2"), -1, c("E2", "I2", "R2"), +1),
# rbind(c("S2", "E2", "I2", "R2"), -1)
# )
#
# measles_land$parameters <- c(
#   R0 = 16,
#   infectious.period = 7, # days
#   latent.period = 8,     # days
#   vacc.pro = 0.90,       # proportion vacc at birth
#   young.size = 14,       # years in the young compartment
#   birth.rate = 10.9,     # per 1000, anum
#   death.rate = 10.9,       # per 1000, anum
#   # Let's have some values that can be set by a wrapper function to
#   # change how our model funtions, as well.
#   introduction.rate = 0, # Some rate of new case introduction
#   start.time = 0, # Time between start of sim and new case introduction
#   end.time = 0, # Time between end of case introduction and end of sim
#   grp.yng = 0, # Bool, will new cases be introduced in this compartment?
#   grp.old = 0 # Bool, same as above
# )
# measles_land$RateF <- function(x, p, t) {
#              # Local parameters
#              beta <- p["R0"] / (p["infectious.period"])
#              f <- 1 / p["latent.period"]
#              gamma <- 1 / p["infectious.period"]
#              alpha <- p["birth.rate"] / 365
#              omega <- p["death.rate"] / 365
#              v <- p["vacc.pro"]
#              age.out <- 1 / (p["young.size"] * 365)
#              startt <- ifelse(t < p["start.time"], 0, 1)
#              endd <- ifelse(t < p["end.time"], 1, 0)
#              yng <- p["grp.yng"]
#              old <- p["grp.old"]
#              int.rate <- p["introduction.rate"]
#              # Local population values
#              S1 <- x["S1"]
#              E1 <- x["E1"]
#              I1 <- x["I1"]
#              R1 <- x["R1"]
#              S2 <- x["S2"]
#              E2 <- x["E2"]
#              I2 <- x["I2"]
#              R2 <- x["R2"]
#              S <- S1 + S2
#              E <- E1 + E2
#              I <- I1 + I2
#              R <- R1 + R2
#              N1 <- S1 + E1 + I1 + R1
#              N2 <- S2 + E2 + I2 + R2
#              Nt <- N1 + N2
#
#              return(c(S1 * beta * (I / Nt), # Young values
#                       E1 * f,
#                       I1 * gamma,
#                       (Nt / 1000) * alpha * (1 - v),
#                       (Nt / 1000) * alpha * v,
#                       # Defining when there will be new case importation :
#                       startt * endd * yng * int.rate, # Young
#                       startt * endd * old * int.rate, # Old
#                       # Aging out
#                       S1 * age.out,
#                       E1 * age.out,
#                       I1 * age.out,
#                       R1 * age.out,
#                       # Old
#                       S2 * beta * (I / Nt),
#                       E2 * f,
#                       I2 * gamma,
#                       # Death
#                       (S2 / 1000) * omega,
#                       (E2 / 1000) * omega,
#                       (I2 / 1000) * omega,
#                       (R2 / 1000) * omega
#              ))
# }
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
# Baseline Seroprevalence : 94%
# Case introduction rate : 0.01 (approximately 1 per 100 days)
# Equal introduction likelihood in either group
# Total grid area: 21 x 11 = 231
# Trying reduced depth : 2000 (simulations way too long to start at 10K)
print(paste0("Begining Run 1 - ", date()))
measles_land$parameters["introduction.rate"] <- 0.01
measles_land$parameters["start.time"] <- 0
measles_land$parameters["end.time"] <- 365
measles_land$t94 <- system.time(measles_land$run_94 <- solutionSpace(measles_land,
                                    insbound = seq(300000, 500000, 10000),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    offset = 2000,
                                    sero.p = c(0.94, 0.94)))
print(paste0("Run 2 done - ", measles_land$t94))
threedPlot(measles_land,
           measles_land$run_94,
           c("max", "median"),
           xlab = "Population Size",
           ylab = "MMR Vaccination Rate (%)",
           zlab = "Outbreak Length (days)",
           title = "Grid Search Run 1 : 94% Seroprevalence")
measles_land$p94 <- measles_land$plot
measles_land$p_d94 <- measles_land$plot_dat
rm(plot_dat, enivr = measles_land)
rm(plot, enivr = measles_land)
# let's save our progress and be done
print("All Done! - ", date())
save(measles_land, file="../../Data/gridsearch1_part2.dat")
