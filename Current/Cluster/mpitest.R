# require(Rmpi)
require(doMPI)
# Make Cluster
cl1 <- startMPIcluster(4, comm = 1)
registerDoMPI(cl1)
require(parallel)
require(foreach)
require(iterators)
print(cl1)
set.seed(1000)
foreach(i = 1:(getDoParWorkers())) %dopar% {
  #---Report primary node----------------------
  cat(Sys.info()[["nodename"]],Sys.getpid(), 
      mpi.comm.rank(), mpi.comm.size() "\n")
  #---Assign slaves to primary node------------
  cluster <- startMPIcluster(2, comm = (i+1))
  assign(paste0("cl", i), cluster, envir = environment())
  registerDoMPI(paste0("cl", i))
  cat("DoParWorkers : "getDoParWorkers(), "\n")
  #---Have those slaves report themselves-------
  foreach(j = 1:(goDoParWorkers())) %dopar% {
    paste(Sys.getpid(), mpi.comm.rank(), "of", mpi.comm.size())
  }
  closeCluster(paste0("cl", i))
}
#---Done-----------
closeCluster(cl1)
mpi.quit()