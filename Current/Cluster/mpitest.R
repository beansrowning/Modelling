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
  paste(Sys.info()[["nodename"]], Sys.getpid(), mpi.comm.rank(),
        "of", mpi.comm.size())
}
closeCluster(cl)
mpi.quit()