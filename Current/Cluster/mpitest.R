# require(Rmpi)
require(doMPI)
# Make Cluster
cl <- startMPIcluster(4, comm = 0)
registerDoMPI(cl)
require(parallel)
require(foreach)
require(iterators)

set.seed(1000)
foreach(i = 1:(getDoParWorkers())) %dopar% {
  #---Report primary node----------------------
  cat(Sys.info()[["nodename"]], "\n")
  #---Assign slaves to primary node------------
  cluster <- startMPIcluster(2, comm = i)
  assign(paste0("cl",i),cluster, envir = environment())
  registerDoMPI(paste0("cl",i))
  #---Have those slaves report themselves-------
  foreach(j = 1:(goDoParWorkers())) %dopar% {
    paste(Sys.getpid(), mpi.comm.rank(), "of", mpi.comm.size())
  }
}
#---Done-----------
closeCluster(cl)
mpi.quit()