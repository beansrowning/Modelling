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
tryCatch(require(testpkg),
         error = function(e) {
          Sys.sleep(2)
          require(testpkg)
})
source("../../Data/worst_case.R")
source("gridsearch2_mpi.R")
opts <- list(chunkSize = ceiling(10000 / getDoParWorkers()))

solutions <- new.env()
# # Run 1 - 12 month delay
# # Insertion rates :   0.01-0.1
# # vaccinations rates: 0.9-1
# # Total grid size : 10 * 11 = 110
# # Search depth : 10,000 runs
# # All other population values fixed
# # Case introduction over the course of 1 year
# # Offest by 2000 days beyond
# print(paste0("Begining Run 1 - ", date()))
# # Start time at 12 mo
# swe$parameters["start.time"] <- 365
# solutions$t1 <- system.time(solutions$run_1 <- solutionSpace(swe,
#                                     insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
#                                                  0.06, 0.07, 0.08, 0.09, 0.1),
#                                     vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
#                                                   0.95, 0.96, 0.97, 0.98, 0.99, 1),
#                                     # Len is start.time + insertion time
#                                     len = 730,
#                                     # Offset is appended at the end of insertion
#                                     offset = 2000))
# print(paste0("Run 1 done - ", solutions$t1[3]))
# save(solutions, file = "../../Data/sweden_2_wc.dat")
# warnings()
# # Run 2 - 24 month delay
# # Insertion rates :   0.01-0.1
# # vaccinations rates: 0.9-1
# # Total grid size : 10 * 11 = 110
# # Search depth : 10,000 runs
# # All other population values fixed
# # Case introduction over the course of 1 year
# # Offest by 2000 days beyond
# print(paste0("Begining Run 2 - ", date()))
# # Start time at 24 mo
# swe$parameters["start.time"] <- 730
# solutions$t2 <- system.time(solutions$run_2 <- solutionSpace(swe,
#                                     insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
#                                                  0.06, 0.07, 0.08, 0.09, 0.1),
#                                     vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
#                                                   0.95, 0.96, 0.97, 0.98, 0.99, 1),
#                                     # Len is start.time + insertion time
#                                     len = 1095,
#                                     # Offset is appended at the end of insertion
#                                     offset = 2000))
# print(paste0("Run 2 done - ", solutions$t2[3]))
# save(solutions, file = "../../Data/sweden_2_wc.dat")
#
# # Run 3 - 36 month delay
# # Insertion rates :   0.01-0.1
# # vaccinations rates: 0.9-1
# # Total grid size : 10 * 11 = 110
# # Search depth : 10,000 runs
# # All other population values fixed
# # Case introduction over the course of 1 year
# # Offest by 2000 days beyond
# source("../../Data/model_global.R")
# print(paste0("Begining Run 3 - ", date()))
# # Start time at 36 mo
# swe$parameters["start.time"] <- 1095
# solutions$t3 <- system.time(solutions$run_3 <- solutionSpace(swe,
#                                     insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
#                                                  0.06, 0.07, 0.08, 0.09, 0.1),
#                                     vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
#                                                   0.95, 0.96, 0.97, 0.98, 0.99, 1),
#                                     # Len is start.time + insertion time
#                                     len = 1460,
#                                     # Offset is appended at the end of insertion
#                                     offset = 2000))
# print(paste0("Run 3 done - ", solutions$t3[3]))
# save(solutions, file = "../../Data/sweden_2_wc.dat")
#
# cat("All Done -", date())

# Run 3 - 36 month delay
# Insertion rates :   0.01-0.1
# vaccinations rates: 0.9-1
# Total grid size : 10 * 11 = 110
# Search depth : 10,000 runs
# All other population values fixed
# Case introduction over the course of 1 year
# Offest by 2000 days beyond
print(paste0("Begining Run 4 - ", date()))
# Start time at 36 mo
swe$parameters["start.time"] <- 1825
solutions$t4 <- system.time(solutions$run_4 <- solutionSpace(swe,
                                    insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
                                                 0.06, 0.07, 0.08, 0.09, 0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    # Len is start.time + insertion time
                                    len = 2190,
                                    # Offset is appended at the end of insertion
                                    offset = 2000))
print(paste0("Run 4 done - ", solutions$t4[3]))
save(solutions, file = "../../Data/swe_wc_3.dat")
cat("All Done -", date())

closeCluster(cl)
mpi.quit()
