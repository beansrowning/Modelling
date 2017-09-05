require(doMPI)
# Make Cluster
cl <- startMPIcluster()
registerDoMPI(cl)
tryCatch(require(Rcpp),
         error = function(e) {
           Sys.sleep(2)
           require(Rcpp)
         })
tryCatch(require(data.table),
         error = function(e) {
          Sys.sleep(2)
          require(data.table)
})
tryCatch(require(adaptivetau),
         error = function(e) {
          Sys.sleep(2)
          require(adaptivetau)
})
tryCatch(require(parallel),
         error = function(e) {
          Sys.sleep(2)
          require(parallel)
})
tryCatch(require(modelutil),
         error = function(e) {
          Sys.sleep(2)
          require(modelutil)
})
source("../../Data/worst_case.R")
source("gridsearch2_mpi.R")
source("../Parameterized/plot.R")
load("../../Data/fivenum_2.dat")
opts <- list(chunkSize = ceiling(10000 / getDoParWorkers()))

solutions <- new.env()

#solutions$t1 <- system.time(solutions$swe_01 <- solutionSpace(swe,
#                                    insbound = c(0.1),
#                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
#                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
#                                    len = 365,
#                                    offset = 2000))
#save(solutions, file = "../../Data/fivenum_2.dat")

solutions$t2 <- system.time(solutions$malta_01 <- solutionSpace(malta,
                                    count = 1000,
				    insbound = c(0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    offset = 15000))
save(solutions, file = "../../Data/fivenum_2.dat")

solutions$t3 <- system.time(solutions$latvia_01 <- solutionSpace(latvia,
                                    insbound = c(0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    offset = 2000))
save(solutions, file = "../../Data/fivenum_2.dat")

solutions$graph1 <- boxyPlot(solutions$swe_01)
solutions$graph2 <- boxyPlot(solutions$malta_01)
solutions$graph3 <- boxyPlot(solutions$latvia_01)
save(solutions, file = "../../Data/fivenum_2.dat")
closeCluster(cl)
mpi.quit()
