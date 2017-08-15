# Hyperparameter space mapping
# Functions: soluntionSpace(), mod_sub(), mod_thread(), ed_sub()

require("adaptivetau")
require("parallel")
require("foreach")
require("data.table")
require("Rcpp")

if (!"Croots" %in% ls()) {
  tryCatch(sourceCpp(".src/Croots.cpp"),
           error = function(e) {
             stop("Could not load src/Croots.cpp, does it exist?", e)
           })
  
}

solutionSpace <- function(envir, count, insbound, occbound) {
  # A function to perform a whole grid search on the hyperparmeters
  # ins and occ, with the domain of the cartesian space defined by insbound
  # and occbound respectively.
  # Both the modelling and analysis subroutines are parallelized through
  # foreach and doParallel and will scale with thread count of the host machine.
  # Due to limitations with the doParallel package, virtualized systems may
  # fail to create a cluster.
  # Args :
  #      envir : (env)ironment where the model parameters can be found
  #      count : (int) the number of simulations to run for each point
  #   insbound : (numeric vector) domain of "ins" to search
  #   occbound : (numeric vector) domain of "occ" to search
  # Returns : 
  #     output : (data.table) three column data.table containing the maximum
  #              outbreak observed given each value of "ins" and "occ" as inputs
  #---Check parameters-----------------------------------------------
  # Check provided environment contains necessary model parameters
  .vars <- c("init.values", "transitions", "parameters", "RateF")
  stopifnot(any(.vars %in% ls(envir)), is.environment(envir))
  # Check user input
  stopifnot(length(count) == 1, count > 0, is.vector(insbound), 
            is.vector(occbound))
  #---Initialize parameters-------------------------------------------
  init <- get("init.values", envir = envir)
  t <- get("transitions", envir = envir)
  rf <- get("RateF", envir = envir)
  p <- get("parameters", envir = envir)
  fun_list <- list(init, t, rf, p, tf)
  output <- data.table(ins = NULL, occ = NULL, max = NULL)
  mod_run <- data.table(time = NULL, I = NULL, iter = NULL)
  max <- NULL
  #---Initialize parallel backend-------------------------------------
  gettype <- ifelse(.Platform$OS.type == "windows", "PSOCK", "FORK")
  cl <- makeCluster(detectCores(logical = FALSE), type = gettype)
  # Export parameters to cluster environment
  ########################################################################
  # TODO: This may not be the correct place to export, consider export at#
  #       foreach function call.                                         #
  ########################################################################
  clusterExport(cl, c("fun_list"), envir = environment())

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
  # Close cluster connections
  closeAllConnections()
  # Return results
  return(output)
}


mod_sub <- function() {
  # Batch model run wrapper function
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
  registerDoParallel(cl)
  
  #---Modelling routine--------------------------------------
  mod_thread <- function() {
    # Per-thread modelling routine
    # called from within the cluster environment, so all variables not availble
    # in the global envrionment need to be exported to the cluster to called.
    # Args : 
    #   None
    # Input :
    #   fun_list : (list) of all parameters from the environment passed to
    #              solutionSpace()
    #      coord : (numeric vector) containing the values of "ins" and "occ" 
    #              currently being evaluated.
    # Returns :
    #   out : (data.frame) model output for a single iteration
    #         , "time", "I", and "iter" columns
    
    ###########################################
    # TODO: Write per-thread modelling routine#
    ###########################################
    
    return(out)
  }
  
  #---Model in parallel--------------------------------------
  mod_run <- foreach(i = 1:count, 
                     .packages = "adaptivetau",
                     .combine = data.frame) %dopar% {
              # Run several iteration of the model and append into data.frame
              mod_thread()     
            }
  #---Stop parallel execution--------------------------------
  registerDoSEQ()
  #---Return as data.table to parent environment-------------
  mod_run <<- as.data.table(mod_run)
}


ed_sub <- function() {
  # outbreak length subroutine
  # analyzes the model output from the calling enviroment to determine the maximum
  # outbreak length out of the simulations
  # Args :
  #   None
  # Input : 
  #   mod_run : (data.table) the output from mod_sub in the parent environment
  # Returns :
  #   max : (int) maximum outbreak time out of the model data provided, stored
  #         in the parent enviroment

  #---init----------------------------------------------------------
  outbreak_max <- 0
  tf <- mod_run[nrow(mod_run), "time"]
  iter_num <- vector()
  outbreak_time <- vector()
  proc <- vector()
  outbreaks <- vector()
  #---Error check for outbreaks that ran over simulation time------
  setkey(mod_run, time)
  proc <- mod_run[.(tf)][, I]
  if (any(proc != 0)) {
    stop("Outbreak ran over simulation at least once!")
  }
  # re-sort to ensure proper analysis
  setkey(mod_run, iter, time) 
  #---Call root finder function on the Infection counts-------------
  mod_run[, roots := Croots(I)]
  #---The meat of 'er------------------------------------------------
  ####################################
  # TODO: decide which method is best#
  ####################################
  #---Possibly even better implementation----------------------------
  # if only the max is needed, can the outer for loop be removed for
  # optimal speed?
  # could also use %dopar% for parallelized execution
  setkey(mod_run, iter, roots)
  mat <- mod_run[, roots = TRUE] # Take all roots
  outbreaks <- foreach(i = 2:nrow(mat), 
                       .combine = c) %do% {
                      outbreak_time <- NULL
                      if (mat$I[i - 1] > 0) {
                        outbreak_time <- c(outbreak_time, 
                                           (mat$time[i] - mat$time[i - 1]))
                      }
                      outbreak_time
                }
  #---Possible new implementation-----------------------------------
  # Using foreach instead of base for loop method
  setkey(mod_run, iter, roots)
  outbreaks <- foreach(i = 1:mod_run$iter[nrow(mod_run)],
                       .combine = c) %do% {
                       outbreak_time <- NULL
                       mat <- NULL
                       mat <- mod_run[.(i, TRUE)]
                       for (j i 2:nrow(mat)) {
                        if (mat$I[j - 1] > 0) {
                            outbreak_time <- c(outbreak_time, 
                                               (mat$time[j] - mat$time[j - 1]))
                        }
                       }
                       outbreak_time
                }
  
  #---Old implementation--------------------------------------------
  for (i in 1:mod_run$iter[nrow(mod_run)]) {
    setkey(mod_run, iter, roots)
    mat <- mod_run[.(i, TRUE)]
    for (j in 2:nrow(mat)) {
      if (mat$I[j - 1] > 0) {
        outbreak_time <- c(outbreak_time, (mat$time[j] - mat$time[j - 1]))
      }
    }
    outbreaks <- c(outbreaks, outbreak_time)
    outbreak_time <- NULL
    mat <- NULL
  }
 #--- Results---------------------------------------------------------
  max <<- max(outbreaks)
}