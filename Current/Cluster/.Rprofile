# This R profile can be used when a cluster does not allow spawning or a job
# scheduler is required to launch any parallel jobs. Saving this file as
# .Rprofile in the working directory or root directory. For unix platform, run
# mpirun -n [cpu numbers] R --no-save -q

# Another way is to modify R_home_dir/bin/R by adding the following line after
# R_HOME_DIR
# R_PROFILE=${R_HOME_DIR}/library/Rmpi/Rprofile; export R_PROFILE

# For windows platform with mpich2, use mpiexec wrapper and specify a working
# directory where .Rprofile is inside.

# Cannot be used as Rprofile.site because it will not work

# If no CPU consumptions of slaves while waiting are desirable, change
# nonblocak=FALSE to nonblock=TRUE and change sleep time accordingly

# Following system libraries are not loaded automatically. So manual loads are
# needed.
library(utils)
library(stats)
library(datasets)
library(grDevices)
library(graphics)
library(methods)
library(Rcpp)
library(adaptivetau)
library(parallel)
library(doParallel)
library(foreach)
library(iterators)
library(data.table)
library(doMPI)
#Change to TRUE if you don't want any slave host info
quiet=FALSE

tryCatch(sourceCpp("./Croots.cpp"),
         warning = function(w) {
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

if (!invisible(library(Rmpi,logical.return = TRUE))){
    warning("Rmpi cannot be loaded")
    q(save = "no")
}

options(error=quote(assign(".mpi.err", FALSE, envir = .GlobalEnv)))

if (mpi.comm.size(0) > 1)
    invisible(mpi.comm.dup(0,1))

if (mpi.comm.rank(0) >0){
    #sys.load.image(".RData",TRUE)
    options(echo=FALSE)
    .comm <- 1
    mpi.barrier(0)
    repeat
		try(eval(mpi.bcast.cmd(rank=0,comm=.comm, nonblock=FALSE, sleep=0.1)),TRUE)
	if (is.loaded("mpi_comm_disconnect"))
        mpi.comm.disconnect(.comm)
    else mpi.comm.free(.comm)
    mpi.quit()
}

if (mpi.comm.rank(0)==0) {
    #options(echo=TRUE)
    mpi.barrier(0)
    if(mpi.comm.size(0) > 1 && !quiet)
        slave.hostinfo(1)
}

.Last <- function(){
    if (is.loaded("mpi_initialize")){
        if (mpi.comm.size(1) > 1){
            print("Please use mpi.close.Rslaves() to close slaves")
            mpi.close.Rslaves(comm=1)
    	}
    }
    #print("Please use mpi.quit() to quit R")
	if (is.loaded("mpi_initialize"))
       .Call("mpi_finalize",PACKAGE = "Rmpi")
}
