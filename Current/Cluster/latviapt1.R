# Approach 2 gridsearch on the Latvia model
# 3 Runs, looking at how age of inserted case affects outcome
require(doMPI)
# Make Cluster
cl <- startMPIcluster()
registerDoMPI(cl)
require(Rcpp)
require(data.table)
require(adaptivetau)
source("../../Data/model_global.R")
source("gridsearch2_mpi.R")
sourceCpp("../src/Croots.cpp")
sourceCpp("../src/lenfind.cpp")
opts <- list(chunkSize = ceiling(10000 / getDoParWorkers()))

solutions <- new.env()
# Run 1
# Insertion rates :   0.01-0.1
# vaccinations rates: 0.9-1
# Total grid size : 10 * 11 = 110
# Search depth : 10,000 runs
# All other population values fixed
# Case introduction over the course of 1 year
# Offest by 2000 days beyond
print(paste0("Begining Run 1 - ", date()))
solutions$t1 <- system.time(solutions$run_1 <- solutionSpace(latvia,
                                    insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
                                                0.06, 0.07, 0.08, 0.09, 0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    offset = 2000))
print(paste0("Run 1 done - ", solutions$t1[3]))
save(solutions, file = "../../Data/latvia_1.dat")
# Run 2
# Insertion rates :   0.01-0.1
# vaccinations rates: 0.9-1
# Total grid size : 10 * 11 = 110
# Search depth : 10,000 runs
# All other population values fixed
# Old persons twice as likely to be introduced
# Case introduction over the course of 1 year
# Offest by 2000 days beyond
print(paste0("Begining Run 2 - ", date()))
solutions$t2 <- system.time(solutions$run_2 <- solutionSpace(latvia,
                                    insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
                                                 0.06, 0.07, 0.08, 0.09, 0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    grp = c(0.5, 1),
                                    offset = 2000))
print(paste0("Run 2 done - ", solutions$t2[3]))
save(solutions, file = "../../Data/latvia_1.dat")

# Run 3
# Insertion rates :   0.01-0.1
# vaccinations rates: 0.9-1
# Total grid size : 10 * 11 = 110
# Search depth : 10,000 runs
# All other population values fixed
# young persons twice as likely to be introduced
# Case introduction over the course of 1 year
# Offest by 2000 days beyond
print(paste0("Begining Run 3 - ", date()))
solutions$t3 <- system.time(solutions$run_3 <- solutionSpace(latvia,
                                    insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
                                                0.06, 0.07, 0.08, 0.09, 0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    grp = c(1, 0.5),
                                    offset = 2000))
print(paste0("Run 2 done - ", solutions$t2[3]))
save(solutions, file = "../../Data/latvia_1.dat")
print(paste0("All done - ", date()))

closeCluster(cl)
mpi.quit()
