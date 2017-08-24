print("Creating New Environment")
# mpi.spawn.Rslaves(nslaves=15)
require(Rmpi)
require(doMPI)
# Make Cluster
cl <- getMPIcluster()
registerDoMPI(cl)
clusterSize(cl)
print(cl)
require(Rcpp)
require(adaptivetau)
require(parallel)
require(foreach)
require(iterators)
require(data.table)

cs <- list(chunkSize = ceiling(10000/(clusterSize(cl) - 1)))
sourceCpp("../src/Croots.cpp")
sourceCpp("../src/lenfind.cpp")
# Source data and functions
source("datap.r")
source("gridsearch1_mpi.R")
source("../Parameterized/get_popvalues.R")
print("All dependencies loaded.")


set.seed(1000)
test <- foreach(i = 1:10) %dopar% {
  paste(Sys.info()[["nodename"]], Sys.getpid(), mpi.comm.rank(),
        "of", mpi.comm.size())
}

measles_land$parameters["start.time"] <- 0
set.seed(1000)
# Run 2
#-------
# Testing initial population size and effective vaccination rate on outbreak length
# Population sizes : 300,000 - 500,000 by 10,000
# Vaccination rates : 0.90 - 1 by 0.01
# Baseline Seroprevalence : 92%
# Case introduction rate : 0.01 (approximately 1 per 100 days)
# Equal introduction likelihood in either group
# Total grid area: 21 x 11 = 231
print(paste0("Begining Run 2 - ", date()))
measles_land$parameters["introduction.rate"] <- 0.01
measles_land$t2 <- system.time(measles_land$run_2 <- solutionSpace(measles_land,
                                    insbound = seq(300000, 500000, 10000),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    # let's try 800 again
                                    offset = 800,
                                    sero.p = c(0.92, 0.92))
print(paste0("Run 2 done - ", measles_land$t2))

save(solutions, file = "hpcrun228.dat")
print(paste0("Done. - ", date()))
closeCluster(cl)
mpi.quit()
