# Hyperparameter space mapping
# Functions: soluntionSpace()
# Subroutines: mod_sub(), mod_thread(), ed_sub()
# Consider using Bytecode or JIT compilier
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

solutionSpace <- function(envir, count = 10000, insbound,
                          occbound, len, grp) {
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
  #   len   : (int) length of each model run in days
  #   grp      : (String) either "y" or "a" signifying what age group to test
  # Returns :
  #     output : (data.table) three column data.table containing the maximum
  #              outbreak observed given each value of "ins" and "occ" as inputs
  #---Check parameters-----------------------------------------------
  # Check provided Args
  .vars <- c("init.values", "transitions", "parameters", "RateF")
  stopifnot(any(.vars %in% ls(envir)), is.environment(envir),
            length(count) == 1, count > 0, is.vector(insbound),
            is.vector(occbound), !is.null(len),
            (grp == "y" || grp == "a"), !any(insbound < 0),
            !any(occbound < 0))
  if (len < (occbound[length(occbound)] + 365)) {
    len <- (occbound[length(occbound)] + 400)
    warning(paste0("Provided length was too short, reset to", len))
  }
  #---Initialize parameters-------------------------------------------
  # TODO : Is this the best way to deal with parameters?
  fun_list <- list(init = get("init.values", envir = envir),
                   t = get("transitions", envir = envir),
                   rf = get("RateF", envir = envir),
                   p = get("parameters", envir = envir))
  output <- data.table(ins = numeric(), occ = numeric(), max = numeric())
  mod_run <- data.table(time = numeric(), I = numeric(), iter = numeric())
  grp <- ifelse(grp == "y", "I1", "I2")
  maxl <- integer()
  #---Initialize parallel backend-------------------------------------
  gettype <- ifelse(.Platform$OS.type == "windows", "PSOCK", "FORK")
  cl <- makeCluster(detectCores(logical = FALSE), type = gettype)
  registerDoParallel(cl)
  #---Stop cluster on exit--------------------------------------------
  on.exit(stopCluster())
  on.exit(closeAllConnections())
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
    #---Parameter init-----------------------------------------
    # fun_list <- get("fun_list", parent.frame())
    # coord <- get("coord", parent.frame())
    # grp <- get("grp", parent.frame())
    # count <- get("count", parent.frame())
    # len <- get("len", parent.frame())
    #---Modelling routine--------------------------------------
    mod_thread <- function() {
      # Per-thread modelling routine / wrapper
      # called from within the cluster environment, so all variables not
      # availble in the scope need to be exported to the cluster to called.
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

      #---Initialize variables----------------------------------
      times <- vector()
      out <- matrix()
      run1 <- matrix()
      init_new <- fun_list$init
      print(fun_list$init)
      #---Check that we shouldn't be starting with infected------
      if (coord[2] != 0) {
        #---Run once----------------------------------------------
        run1 <- ssa.adaptivetau(fun_list$init,
                                fun_list$t,
                                fun_list$rf,
                                fun_list$p,
                                coord[2])
        #---Add number of infected at the insertion time----------
        run1[nrow(run1), grp] <- run1[nrow(run1), grp] + coord[1]
        init_new <- run1[nrow(run1), ]
        init_new <- init_new[-1]
      } else {
        #---Skip firt run and just insert infected at time 0------
        init_new[grp] <- init_new[grp] + coord[1]
      }

      #---Run second time after inserting infected---------------
      print(init_new) # DEBUG
      out <- ssa.adaptivetau(init_new,
                             fun_list$t,
                             fun_list$rf,
                             fun_list$p,
                             len)
      #---Check again to decide how we handle the data-------------
      if (coord[2] != 0) {
        #---Offset time in the second run by the end of the first---
        out <- cbind(apply(out[, "time", drop = FALSE], 2,
                           function(x) x + run1[nrow(run1), "time"]),
                           out[, -1])
        #---Combine, dropping the first row of the second row-------
        out <- rbind(run1, out[-1, ])
      }
      return(as.data.frame(out))
    }

    #---Model in parallel--------------------------------------
    mod_run <- foreach(i = 1:count,
                       .packages = "adaptivetau",
                       .combine = "rbind",
                       .export = c("fun_list", "coord", "grp",
                                   "len", "count")) %dopar% {
                # Run several iteration of the model and append into data.frame
                out <- mod_thread()
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
    #---Error check for outbreaks that ran over simulation time------
    # This would cause errors in the analysis
    setkey(mod_run, time)
    proc <- mod_run[.(tf)][, I]
    if (any(proc != 0)) {
      stop("Outbreak ran over simulation at least once, check sim length!")
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
    maxl <<- max(outbreaks)
  }
  #---Cartesian whole grid search-------------------------------------
  for (i in 1:length(insbound)) {
    for (j in 1:length(occbound)){
      #---Store value of ins and occ to be evaluated------------------
      coord <- c(insbound[i], occbound[j])
      mod_sub()
      ed_sub()
      # eval(call("mod_sub"), environment()) # Produces mod_run
      # eval(call("ed_sub"), environment()) # Produces max
      #---Append coord to output space--------------------------------
      # Using the data.table function rbindlist to populate the data.table
      output <- rbindlist(list(output, data.frame(ins = insbound[i],
                                                  occ = occbound[j],
                                                  max = maxl)),
                          fill = TRUE)
      #---Clear vars for next iteration-------------------------------
      mod_run <- NULL
      maxl <- NULL
    }
  }

  # # Alternative syntax using nested foreach loops :
  # # Needs profiling
  # foreach(i = insbound, .combine ='data.frame') %:%
  #   foreach(j = occbound, .combine='c') %do% {
  #     mod_sub()
  #     ed_sub()
  #     c(i, j, maxl) # May not be the proper return
  #     mod_run <- NULL
  #     maxl <- NULL
  #   }

  #---Return results----------------------------------------------------
  return(output)
}
