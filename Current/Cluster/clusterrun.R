print("Creating New Environment")
# mpi.spawn.Rslaves(nslaves=15)
require(Rcpp)
require(adaptivetau)
require(parallel)
require(foreach)
require(iterators)
require(data.table)
require(doMPI)
tryCatch(sourceCpp("./Croots.cpp"),
         error = function(w) {
           print("Croots couldn't load, trying package instead... ")
           tryCatch(system("cd ../../Data/; R CMD INSTALL Croots"),
                    error = function(e){
                      print("Library failed to load. Is it installed?")
                      print(paste(e, w, sep = " "))
                    })
         })
tryCatch(sourceCpp("./lenfind.cpp"),
                    error = function(e) {
                      stop(e)
                      })

# Source data and functions
source("datap.r")
source("solutionp.R")
print("All dependencies loaded.")

cl <- startMPIcluster(verbose=TRUE, 
                      defaultopts = list(chunkSize = 10000/(mpi.comm.size(0) - 1)))
registerDoMPI(cl)
print(cl)

mpi.remote.exec(paste(Sys.info()[['nodename']], Sys.getpid(), mpi.comm.rank(), "of", mpi.comm.size())) 
test <- foreach(i=1:10,
                .combine = "c") %dopar% {
  paste(Sys.info()[['nodename']], Sys.getpid(), mpi.comm.rank(), "of", mpi.comm.size()) 
}
print(head(test))
stopifnot(length(test) == 10000)
set.seed(1000)
solutions <- new.env()
# Is this thing on?
# Let's just check that MPI and foreach are playing nicely at all
# before sending a large task
solutions$t1 <- system.time(solutions$run_1 <- solutionSpace(swe,
                                 insbound = c(0.01, 0.02),
                                 vaccbound = c(0.90, 0.91),
                                 len = 365))
print("Run 1 finished")
print(solutions$t1)
# print(paste0("Starting Run 1 - ", date()))
# # Run 1
# # Sweden model
# # ------------
# # Old and young equally likely to be introduced
# # length = 365 days
# # end lag = 600 days
# # Introduction rates : 0.01-0.1
# # Vaccination rates  : 0.90-0.98
# # Parameter space area : 9x10 = 90
# solutions$t1 <- system.time(solutions$run_1 <- solutionSpace(swe,
#                                 insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
#                                               0.06, 0.07, 0.08, 0.09, 0.1),
#                                 vaccbound = c(0.90, 0.91, 0.92, 0.93,
#                                               0.94, 0.95, 0.96, 0.97, 0.98),
#                                 len = 365))
# print("Run 1 finished")
# print(solutions$t1)
# print(paste0("Starting Run 2 - ", date()))
# # Run 2
# # Sweden model
# # ------------
# # Old 1/2 as likely to be introduced
# # length = 365 days
# # end lag = 600 days
# # Introduction rates : 0.01-0.1
# # Vaccination rates  : 0.90-0.98
# # Parameter space area : 9x10 = 90
# solutions$t2 <- system.time(solutions$run_2 <- solutionSpace(swe,
#                                   insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
#                                               0.06, 0.07, 0.08, 0.09, 0.1),
#                                   vaccbound = c(0.90, 0.91, 0.92, 0.93,
#                                                 0.94, 0.95, 0.96, 0.97, 0.98),
#                                   len = 365,
#                                   grp = c(1,0.5)))
# print("Run 2 finished")
# print(solutions$t2)
# print(paste0("Starting Run 3 - ", date()))
# # Run 3
# # Sweden model
# # ------------
# # young 1/2 as likely to be introduced
# # length = 365 days
# # end lag = 600 days
# # Introduction rates : 0.01-0.1
# # Vaccination rates  : 0.90-0.98
# # Parameter space area : 9x10 = 90
# solutions$t3 <- system.time(solutions$run_3 <- solutionSpace(swe,
#                                 insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
#                                               0.06, 0.07, 0.08, 0.09, 0.1),
#                                 vaccbound = c(0.90, 0.91, 0.92, 0.93,
#                                               0.94, 0.95, 0.96, 0.97, 0.98),
#                                 len = 365,
#                                 grp = c(0.5,1)))
# print("Run 3 finished")
# print(solutions$t3)
# print(paste0("Starting Run 4 - ", date()))
# # Run 4
# # Sweden model
# # ------------
# # only young introduced
# # length = 365 days
# # end lag = 600 days
# # Introduction rates : 0.01-0.1
# # Vaccination rates  : 0.90-0.98
# # Parameter space area : 9x10 = 90
# solutions$t4 <- system.time(solutions$run_4 <- solutionSpace(swe,
#                                   insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
#                                               0.06, 0.07, 0.08, 0.09, 0.1),
#                                   vaccbound = c(0.90, 0.91, 0.92, 0.93,
#                                                 0.94, 0.95, 0.96, 0.97, 0.98),
#                                   len = 365,
#                                   grp = c(1,0)))
# print("Run 4 finished")
# print(solutions$t4)
# print(paste0("Starting Run 5 - ", date()))

# # Run 5
# # Sweden model
# # ------------
# # only old introduced
# # length = 365 days
# # end lag = 600 days
# # Introduction rates : 0.01-0.1
# # Vaccination rates  : 0.90-0.98
# # Parameter space area : 9x10 = 90
# solutions$t5 <- system.time(solutions$run_5 <- solutionSpace(swe,
#                                 insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
#                                               0.06, 0.07, 0.08, 0.09, 0.1),
#                                 vaccbound = c(0.90, 0.91, 0.92, 0.93,
#                                               0.94, 0.95, 0.96, 0.97, 0.98),
#                                 len = 365,
#                                 grp = c(0,1)))
# print("Run 5 finished")
# print(solutions$t5)
# print(paste0("Saving - ", date()))
save(solutions, file = "hpcrun228.dat")
print(paste0("Done. - ", date()))
closeCluster(cl)
mpi.quit()
