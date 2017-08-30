# Hyperparameter space mapping
# Functions: solutionSpace()
# Subroutines: mod_sub(), mod_thread(), ed_sub()
# Consider using Bytecode or JIT compilier
require("adaptivetau")
require("parallel")
require("foreach")
require("data.table")
require("Rcpp")

if (!"Croots" %in% ls()) {
tryCatch(sourceCpp("../src/Croots.cpp"),
         error = function(e) {
           owd <- getwd()
           setwd("../../Data/")
           system("R CMD INSTALL modelutil")
           setwd(owd)
           tryCatch(library("modelutil"),
           error = function(ee) {
             stop(e, ee)
             })
         })
}
if (!"lenFind" %in% ls()) {
tryCatch(sourceCpp("../src/lenfind.cpp"),
         error = function(e) {
           owd <- getwd()
           setwd("../../Data/")
           system("R CMD INSTALL modelutil")
           setwd(owd)
           tryCatch(library("modelutil"),
           error = function(ee) {
             stop(e, ee)
             })
         })
}

solutionSpace <- function(envir, count = 10000, insbound,
                          vaccbound = c(0.94),
                          len, grp = c(1, 1)) {
  # A function to perform a whole grid search on the hyperparmeters
  # ins and vacc, with the domain of the cartesian space defined by insbound
  # and vaccbound respectively.
  # The modelling subroutine is parallelized through
  # foreach and doParallel and will scalewith thread count of the host machine.
  # Due to limitations with the doParallel package, virtualized systems may
  # fail to create a cluster.
  # Args :
  #   envir     : (env)ironment where the model parameters can be found
  #   count     : (int) the number of simulations to run for each point
  #   insbound  : (numeric vector) domain of "ins" to search
  #   vaccbound : (numeric vector) domain of "vacc" to search
  #   len       : (int) length of each model run in days
  #   grp       : (numeric vector) signifying what group will get introductions
  #              ex. c(1,0) = Only "young" cases will be introduced
  #                  c(0,1) = Only "old" cases will be introduced
  #                  c(1,1) = both "young" and "old" cases will be introduced
  #                  c(n,m) = both will be introduced with some weights n and m
  #   offset    : (int) length to append to the end of `len` to overrun the
  #               simulation can also be used to determine endemic spread.
  #   ...       : Additional arguments to pass to get_popvalues()
  # Returns :
  #     output : (data.table) three column data.table containing the maximum
  #              outbreak observed given each value of "ins" and "vacc" as inputs
  #---Check parameters-----------------------------------------------
  # Check provided Args
  .vars <- c("init.values", "transitions", "parameters", "RateF")
  stopifnot(any(.vars %in% ls(envir)), is.environment(envir),
            (length(count) == 1 && count > 0), is.vector(insbound),
            is.vector(vaccbound), !is.null(len),
            (is.vector(grp) && length(grp) == 2 && !any(grp < 0)),
            !any(insbound < 0), !any(vaccbound < 0))

  #---Initialize parameters-------------------------------------------
  # TODO : Is this the best way to deal with parameters?
  fun_list <- list(init = get("init.values", envir = envir),
                   t = get("transitions", envir = envir),
                   rf = get("RateF", envir = envir),
                   p = get("parameters", envir = envir))
  #---Set the end time for the case introductions to be the given length-
  fun_list$p["end.time"] <- len
  #---Fix to avoid FP issues-----------------------------------------------
  output <- data.table(ins = integer(), vacc = integer(), min = integer(),
                       mean = integer(), median = integer(), iqr = integer(),
                       max = integer())
  mod_run <- data.table(time = numeric(), I = numeric(), iter = numeric())
  fun_list$p["grp.yng"] <- grp[1]
  fun_list$p["grp.old"] <- grp[2]
  minl <- integer()
  modl <- integer()
  meanl <- integer()
  iqrl <- integer()
  maxl <- integer()
  #---Get population values for part 1--------------------------------
  # popvalues <- get_popvalues(insbound, ...)
  #---Initialize parallel backend-------------------------------------
  opts <- get("opts", parent.frame())
  #---Stop cluster on exit--------------------------------------------
  # on.exit(stopCluster())
  # on.exit(closeAllConnections())
  #---Define function subroutines-------------------------------------
  mod_sub <- function() {
    # Batch model run subroutine
    # Takes in the parameters provided in the parent environment and runs several
    # simulations with ssa.adpativetau in parallel using the foreach package
    # Args :
    #   None
    # Input :
    #   cl : (cluster) created with the makeCluster() function in the parent
    #        environment
    # Returns :
    #   mod_run : (data.table) containing model data from all iterations
    #---Model in parallel--------------------------------------
    mod_run <- foreach(i = 1:count,
                       .packages = "adaptivetau",
                       .combine = "rbind",
                       .export = c("len". "fun_list"),
                       .options.mpi = opts) %dopar% {
                # Run several iteration of the model and append into data.frame
                out <- ssa.adaptivetau(fun_list$init,
                                           fun_list$t,
                                           fun_list$rf,
                                           fun_list$p,
                                           len)
                out <- cbind(out, I = rowSums(out[, c("I1", "I2")]), iter = i)
                out <- out[, c("time", "I", "iter"), drop = FALSE]
                out
              }
    #---Return as data.table to parent environment-------------
    mod_run <<- as.data.table(mod_run)
  }

  ed_sub <- function() {
    # Outbreak length subroutine
    # analyzes the model output from the calling enviroment to determine the maximum
    # outbreak length out of the simulations
    # It does this by running a C++ routine to return the "roots" of an outbreak,
    # and calculating the distance between those points.
    # For systems with a large thread count, a parallel implementation with
    # the foreach package may have some benefit.
    # Args :
    #   None
    # Input :
    #   mod_run : (data.table) the output from mod_sub in the parent environment
    # Returns :
    #   maxl : (int) maximum outbreak time out of the model data provided, stored
    #         in the parent enviroment
    #   mod_run : (data.table) appended with a "roots" column

    #---init----------------------------------------------------------
    # mod_run <- get("mod_run", parent.frame())
    tf <- mod_run[nrow(mod_run), "time"]
    iter_num <- vector()
    proc <- vector()
    outbreaks <- vector()
    cat("...Analyzing")
    #---Error check for outbreaks that ran over simulation time------
    # This would cause errors in the analysis
    setkey(mod_run, time)
    proc <- mod_run[.(tf)][, I]
    if (any(proc != 0) || nrow(mod_run[I > 0]) == 0) {
      cat(date(), ": ", vaccbound[i], "-", insbound[j], "FAILED", "\n")
      minl <<- NA
      modl <<- NA
      meanl <<- NA
      iqrl <<- NA
      maxl <<- NA
      return()
    }
    # re-sort to ensure Croots will work correctly
    setkey(mod_run, iter, time)
    #---Call root finder function on the Infection counts-------------
    mod_run[, roots := Croots(I)]
    #---The meat of 'er------------------------------------------------
    setkey(mod_run, roots)
    mat <- mod_run[J(TRUE)] # Take only roots
    #---Pass time and I columns as vectors for the C++ function to process
    outbreaks <- lenFind(mat[, time], mat[, I])
    #---Summary Stats------------------------
    minl <<- min(outbreaks)
    modl <<- median(outbreaks)
    meanl <<- mean(outbreaks)
    iqrl <<- IQR(outbreaks)
    maxl <<- max(outbreaks)
    #---Clear this from memory-----------------
    outbreaks <- NULL
    cat("\n")
  }
  #---Cartesian whole grid search-------------------------------------
  for (i in 1:length(vaccbound)) {
    for (j in 1:length(insbound)){
      #---Store value of ins and vacc to be evaluated------------------
      coord <- c(insbound[j], vaccbound[i])
      cat(date(), ": Running :",vaccbound[i], "-", insbound[j])
      cat(" ...Modelling")
      fun_list$p["introduction.rate"] <- coord[1]
      fun_list$p["vacc.pro"] <- coord[2]
      mod_sub()
      ed_sub()
      #---Append coord to output space--------------------------------
      # Using the data.table function rbindlist to populate the data.table
      output <- rbindlist(list(output, data.frame(ins = insbound[j],
                                                  vacc = vaccbound[i],
                                                  min = minl,
                                                  mean = meanl,
                                                  median = modl,
                                                  iqr = iqrl,
                                                  max = maxl)),
                          fill = TRUE)
      #---Clear vars for next iteration-------------------------------
      mod_run <- NULL
      minl <- NULL
      meanl <- NULL
      modl <- NULL
      iqrl <- NULL
      maxl <- NULL
    }
  }
  #---Return results----------------------------------------------------
  return(output)
}
