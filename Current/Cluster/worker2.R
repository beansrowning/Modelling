# require(Rmpi)
require(doMPI)
# Make Cluster
cl <- startMPIcluster(16, comm = 1)
registerDoMPI(cl)

clusterSize(cl)
print(cl)
require(Rcpp)
require(adaptivetau)
require(parallel)
require(foreach)
require(iterators)
require(data.table)

initEnvir <- function() {
  library(adaptivetau)
  len <- get("len", parent.frame())
}
opts <- list(chunkSize = ceiling(10000/(getDoParWorkers())),
             initEnvir = initEnvir)
sourceCpp("../src/Croots.cpp")
sourceCpp("../src/lenfind.cpp")
# Source data and functions
source("../../Data/model_global.R")
source("gridsearch1_mpi.R")
print("All dependencies loaded.")

# Run 2 mod 1 pt 2
#-------
# Testing sensitivity to more of the older population being measles cases
# Population sizes : 400,000 - 500,000 by 10,000
# Vaccination rates : 0.90 - 1 by 0.01
# Baseline Seroprevalence : 92%
# Case introduction rate : 0.01 (approximately 1 per 100 days)
# Equal introduction likelihood in either group
# Total grid area: 11 x 11 = 121
print(paste0("Begining Run 1 - ", date()))
measles_land$parameters["introduction.rate"] <- 0.01
measles_land$t2_2 <- system.time(measles_land$run_2_2 <- solutionSpace(measles_land,
                                    count = 2000,
                                    insbound = seq(400000, 500000, 10000),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    offset = 2000,
                                    grp = c(0.5, 1),
                                    sero.p = c(0.92, 0.92)))
print(paste0("Run 1 done - ", measles_land$t2_2[3]))
save(measles_land, file="../../Data/worker2.dat")

# Run 2 mod 2 pt 2
#-------
# Testing sensitivity to more of the younger population being measles cases
# Population sizes : 400,000 - 500,000 by 10,000
# Vaccination rates : 0.90 - 1 by 0.01
# Baseline Seroprevalence : 92%
# Case introduction rate : 0.01 (approximately 1 per 100 days)
# Equal introduction likelihood in either group
# Total grid area: 11 x 11 = 121
print(paste0("Begining Run 2 - ", date()))
measles_land$parameters["introduction.rate"] <- 0.01
measles_land$t2_22 <- system.time(measles_land$run_2_22 <- solutionSpace(measles_land,
                                    # Reduced search depth:
                                    count = 2000,
                                    insbound = seq(400000, 500000, 10000),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    offset = 2000,
                                    grp = c(1, 0.5)
                                    sero.p = c(0.92, 0.92)))
print(paste0("Run 2 done - ", measles_land$t2_22[3]))
save(measles_land, file="../../Data/worker2.dat")

# Run 2 mod 3 pt 2
#-------
# Testing sensitivity to higher proportion of younger population seronegative
# Population sizes : 400,000 - 500,000 by 10,000
# Vaccination rates : 0.90 - 1 by 0.01
# Baseline Seroprevalence : 92%
# Case introduction rate : 0.01 (approximately 1 per 100 days)
# Equal introduction likelihood in either group
# Total grid area: 11 x 11 = 121
print(paste0("Begining Run 3 - ", date()))
measles_land$parameters["introduction.rate"] <- 0.01
measles_land$t2_32 <- system.time(measles_land$run_2_32 <- solutionSpace(measles_land,
                                    # Reducded search depth:
                                    count = 2000,
                                    insbound = seq(400000, 500000, 10000),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    offset = 2000,
                                    grp = c(1, 1)
                                    sero.p = c(0.92, 0.94)))
print(paste0("Run 3 done - ", measles_land$t2_32[3]))
save(measles_land, file="../../Data/worker2.dat")

# Run 2 mod 4 pt 2
#-------
# Testing sensitivity to higher proportion of older population seronegative
# Population sizes : 400,000 - 500,000 by 10,000
# Vaccination rates : 0.90 - 1 by 0.01
# Baseline Seroprevalence : 92%
# Case introduction rate : 0.01 (approximately 1 per 100 days)
# Equal introduction likelihood in either group
# Total grid area: 10 x 11 = 110
print(paste0("Begining Run 4 - ", date()))
measles_land$parameters["introduction.rate"] <- 0.01
measles_land$t2_42 <- system.time(measles_land$run_2_42 <- solutionSpace(measles_land,
                                    # Reducded search depth:
                                    count = 2000,
                                    insbound = seq(400000, 500000, 10000),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    offset = 2000,
                                    grp = c(1, 1)
                                    sero.p = c(0.94, 0.92)))
print(paste0("Run 4 done - ", measles_land$t2_42[3]))
save(measles_land, file="../../Data/worker2.dat")
print(paste0("Done. - ", date()))
closeCluster(cl)
mpi.quit()

