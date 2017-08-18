# Hyperparameter space mapping
# Functions: soluntionSpace()
# Subroutines: mod_sub(), mod_thread(), ed_sub()
# Consider using Bytecode or JIT compiling
require("adaptivetau")
require("parallel")
require("foreach")
require("data.table")
require("Rcpp")

if (!"Croots" %in% ls()) {
  tryCatch(sourceCpp(".src/Croots.cpp"),
           error = function(e) {
             stop(e)
           })
}

solutionSpace <- function(envir, count, insbound, occbound, length) {
  # A function to perform a whole grid search on the hyperparmeters
  # ins and occ, with the domain of the cartesian space defined by insbound
  # and occbound respectively.
  # The modelling subroutine is parallelized through
  # foreach and doParallel and will scalewith thread count of the host machine.
  # Due to limitations with the doParallel package, virtualized systems may
  # fail to create a cluster.
  # Args :
  #      envir : (env)ironment where the model parameters can be found
  #      count : (int) the number of simulations to run for each point
  #   insbound : (numeric vector) domain of "ins" to search
  #   occbound : (numeric vector) domain of "occ" to search
  #   length   : (int) length of each model run in days
  # Returns :
  #     output : (data.table) three column data.table containing the maximum
  #              outbreak observed given each value of "ins" and "occ" as inputs
  #---Check parameters-----------------------------------------------
  # Check provided environment contains necessary model parameters
  .vars <- c("init.values", "transitions", "parameters", "RateF")
  stopifnot(any(.vars %in% ls(envir)), is.environment(envir))
  # Check user input
  stopifnot(length(count) == 1, count > 0, is.vector(insbound),
            is.vector(occbound), !is.NULL(length))
  #---Initialize parameters-------------------------------------------
  # TODO : Easier to forego intermediary variables?
  # init <- get("init.values", envir = envir)
  # t <- get("transitions", envir = envir)
  # rf <- get("RateF", envir = envir)
  # p <- get("parameters", envir = envir)
  fun_list <- list(init = get("init.values", envir = envir),
                   t = get("transitions", envir = envir),
                   rf = get("RateF", envir = envir),
                   p = get("parameters", envir = envir),
                   tf)
  output <- data.table(ins = NULL, occ = NULL, max = NULL)
  mod_run <- data.table(time = NULL, I = NULL, iter = NULL)
  max <- NULL
  #---Initialize parallel backend-------------------------------------
  gettype <- ifelse(.Platform$OS.type == "windows", "PSOCK", "FORK")
  cl <- makeCluster(detectCores(logical = FALSE), type = gettype)
  # Stop cluster on exit
  on.exit(stopCluster())
  on.exit(closeAllConnections())
  # Export parameters to cluster environment
  ########################################################################
  # TODO: This may not be the correct place to export, consider export at#
  #       foreach function call.                                         #
  ########################################################################
  # clusterExport(cl, c("fun_list"), envir = environment())

  #---Cartesian whole grid search-------------------------------------
  for (i in 1:length(insbound)) {
    for (j in 1:length(occbound)){
      # Store value of ins and occ to be evaluated
      coord <- c(insbound[i], occbound[j])
      mod_sub() # Produces mod_run to be evaluated
      ed_sub() # Evaluates mod_run and produces max
      # Append coord to output space
      output <<- rbind(output, c(coord[1], coord[2], max))
      # Clear vars for next iteration
      mod_run <- NULL
      max <- NULL
    }
  }
  
  # # Alternative syntax using nested foreach loops :
  # # Needs profiling
  # foreach(i = insbound, .combine ='data.frame') %:%
  #   foreach(j = occbound, .combine='c') %do% {
  #     mod_sub()
  #     ed_sub()
  #     c(i, j, max) # May not be the proper return
  #     mod_run <- NULL
  #     max <- NULL
  #   }
  
  # Return results
  return(output)
}


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
  #---Parameter init-----------------------------------------

  ##############################################
  # TODO: deal with parameters, lexical scoping#
  ##############################################

  #---Set up parallel backend--------------------------------
  # TODO : Can this be called once from the parent function?
  registerDoParallel(cl)

  #---Modelling routine--------------------------------------
  mod_thread <- function() {
    # Per-thread modelling routine / wrapper
    # called from within the cluster environment, so all variables not availble
    # in the scope need to be exported to the cluster to called.
    # Args :
    #   None
    # Input :
    #   fun_list : (list) of all parameters from the environment passed to
    #              solutionSpace()
    #      coord : (numeric vector) containing the values of "ins" and "occ"
    #              currently being evaluated.
    # Returns :
    #   out : (data.frame) model output for a single iteration
    #         "time", "I", and "iter" columns
    
    #---Initialize variables--------------------------------
    times <- vector()
    out <- matrix()
    
    times <- c(coords[2])
    

    return(as.data.frame(out))
  }

  #---Model in parallel--------------------------------------
  mod_run <- foreach(i = 1:count,
                     .packages = "adaptivetau",
                     .combine = data.frame,
                     .export = "fun_list" ) %dopar% {
              # Run several iteration of the model and append into data.frame
              mod_thread()
            }
  #---Stop parallel execution--------------------------------
  registerDoSEQ()
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
  #   max : (int) maximum outbreak time out of the model data provided, stored
  #         in the parent enviroment
  #  mod_run : (data.table) appended with a "roots" column

  #---init----------------------------------------------------------
  tf <- mod_run[nrow(mod_run), "time"]
  iter_num <- vector()
  proc <- vector()
  outbreaks <- vector()
  #---Error check for outbreaks that ran over simulation time------
  # This would cause errors in the analysis
  setkey(mod_run, time)
  proc <- mod_run[.(tf)][, I]
  if (any(proc != 0)) {
    stop("Outbreak ran over simulation at least once!")
  }
  # re-sort to ensure Croots will work correctly
  setkey(mod_run, iter, time)
  #---Call root finder function on the Infection counts-------------
  mod_run[, roots := Croots(I)]
  #---The meat of 'er------------------------------------------------
  setkey(mod_run, roots)
  mat <- mod_run[J(TRUE)] # Take only roots
  for (i in 2:nrow(mat)) {
      # Determine if current observation is an endpoint
      # by checking that the previous observation was a start point (I > 0)
      if (mat$I[i - 1] > 0) {
        # Take the length between the preious observation (start) and this one
        outbreaks <- c(outbreaks, (mat$time[i] - mat$time[i - 1]))
      }
  }
  max <<- max(outbreaks)
}
