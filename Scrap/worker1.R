# require(Rmpi)
require(doMPI)
# Make Cluster
cl <- startMPIcluster(20, comm = 1)
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
source("../Parameterized/get_popvalues.R")
print("All dependencies loaded.")


# Run 2 mod 4 pt 1
#-------
# Testing sensitivity to higher proportion of older population seronegative
# Population sizes : 300,000 - 390,000 by 10,000
# Vaccination rates : 0.90 - 1 by 0.01
# Baseline Seroprevalence : 92%
# Case introduction rate : 0.01 (approximately 1 per 100 days)
# Equal introduction likelihood in either group
# Total grid area: 10 x 11 = 110
print(paste0("Begining Run 4 - ", date()))
measles_land$parameters["introduction.rate"] <- 0.01
measles_land$t2_41 <- system.time(measles_land$run_2_41 <- solutionSpace(measles_land,
                                    # Reducded search depth:
                                    count = 2000,
                                    insbound = seq(300000, 390000, 10000),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    offset = 2000,
                                    grp = c(1, 1),
                                    sero.p = c(0.94, 0.92)))
print(paste0("Run 4 done - ", measles_land$t2_41[3]))
save(measles_land, file="../../Data/worker1c.dat")

print(paste0("Done. - ", date()))
closeCluster(cl)
mpi.quit()
