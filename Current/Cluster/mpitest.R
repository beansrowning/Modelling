# require(Rmpi)
require(doMPI)
# Spawn R Slaves
# universe <- 13
# n <- (universe - 1) / 3
mpi.spawn.Rslaves(4)
mpi.remote.exec(print(mpi.comm.rank()))

#---Define the number of workers we will assign to each slave----
m <- 2
#---Define Slave function-----------------
slavedoWorkers <- function(x) {
  require(doMPI)
  #---Report primary node----------------------
  cat(Sys.info()[["nodename"]],Sys.getpid(), 
      mpi.comm.rank(), mpi.comm.size(), "\n")
  #---Assign slaves to primary node------------
  slavecluster <- startMPIcluster(2)
  registerDoMPI(slavecluster)
  cat("DoParWorkers : ", getDoParWorkers(), "\n")
  #---Have those slaves report themselves-------
  foreach(j = 1:(goDoParWorkers())) %dopar% {
    paste(Sys.getpid(), mpi.comm.rank(), "of", mpi.comm.size())
  }
  closeCluster(slavecluster)
}
mpi.remote.exec(slavedoWorkers, m)
#---Done-----------
closeCluster(cl1)
mpi.quit()