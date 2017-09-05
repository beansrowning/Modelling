# Five number summaries for part 2
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
load("../../Data/fivenum_3.dat")
opts <- list(chunkSize = ceiling(10000 / getDoParWorkers()))

solutions <- new.env()
swe$parameters["start.time"] <- 365
solutions$swe_1 <- solutionSpace(swe,
                                  insbound = c(0.1),
                                  vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                  len = 730,
                                  offset = 2000)
save(solutions, file = "../../Data/fivenum_3.dat")

swe$parameters["start.time"] <- 730
solutions$swe_2 <- solutionSpace(swe,
                                  insbound = c(0.1),
                                  vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                  len = 1095,
                                  offset = 2000)
save(solutions, file = "../../Data/fivenum_3.dat")

swe$parameters["start.time"] <- 1095
solutions$swe_3 <- solutionSpace(swe,
                                  insbound = c(0.1),
                                  vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                  len = 1460,
                                  offset = 2000)
save(solutions, file = "../../Data/fivenum_3.dat")



solutions$swe_1[, delay := 12]
solutions$swe_2[, delay := 24]
solutions$swe_3[, delay := 36]
solutions$swe_01 <- rbind(swe_1, swe_2, swe_3)

malta$parameters["start.time"] <- 365
solutions$malta_1 <- solutionSpace(malta,
                                  insbound = c(0.1),
                                  vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                  len = 730,
                                  offset = 2000)
save(solutions, file = "../../Data/fivenum_3.dat")

malta$parameters["start.time"] <- 730
solutions$malta_2 <- solutionSpace(malta,
                                  insbound = c(0.1),
                                  vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                  len = 1095,
                                  offset = 2000)
save(solutions, file = "../../Data/fivenum_3.dat")

malta$parameters["start.time"] <- 1095
solutions$malta_3 <- solutionSpace(malta,
                                  insbound = c(0.1),
                                  vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                  len = 1460,
                                  offset = 2000)
save(solutions, file = "../../Data/fivenum_3.dat")



solutions$malta_1[, delay := 12]
solutions$malta_2[, delay := 24]
solutions$malta_3[, delay := 36]
solutions$malta_01 <- rbind(malta_1, malta_2, malta_3)

latvia$parameters["start.time"] <- 365
solutions$latvia_1 <- solutionSpace(latvia,
                                  insbound = c(0.1),
                                  vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                  len = 730,
                                  offset = 2000)
save(solutions, file = "../../Data/fivenum_3.dat")

latvia$parameters["start.time"] <- 730
solutions$latvia_2 <- solutionSpace(latvia,
                                  insbound = c(0.1),
                                  vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                  len = 1095,
                                  offset = 2000)
save(solutions, file = "../../Data/fivenum_3.dat")

latvia$parameters["start.time"] <- 1095
solutions$latvia_3 <- solutionSpace(latvia,
                                  insbound = c(0.1),
                                  vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                  len = 1460,
                                  offset = 2000)
save(solutions, file = "../../Data/fivenum_3.dat")



solutions$latvia_1[, delay := 12]
solutions$latvia_2[, delay := 24]
solutions$latvia_3[, delay := 36]
solutions$latvia_01 <- rbind(latvia_1, latvia_2, latvia_3)
save(solutions, file = "../../Data/fivenum_3.dat")

closeCluster(cl)
mpi.quit()
