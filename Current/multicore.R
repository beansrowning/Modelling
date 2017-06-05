# Multicore Batch Run
# 4 Jun 2017

require("adaptivetau")
require("parallel")
require("doParallel")
require("foreach")

batch_run_mc <- function(batch = 10000,
                          fun_list = list(init.values, transitions,
                          RateF, parameters, 365),
                          grp = NULL, insertion = 0,
                          i_number = NULL, occ = 2) {
  # Runs a specified number of adaptivetau simulations utilizing multithreaded
  # processing by assigning CPU cores to a parallel socket cluster.
  # It will not, therefore, run in a virtual machine. UNIX untested.
  # Performance increase over base function is close to sqrt(t) on 4 cores
  #
  # Args:
  #   batch : Number of desired ssa runs
  #   fun_list : List of parameters passed to ssa.adaptivetau
  #   grp : 'y' or 'a' to indicate which age strata to insert into
  #   insertion : Time point of first insertion (in days)
  #   i_number : Number of infected persons to insert each time
  #   occ : How many total insertion events should occur
  # Returns:
  #   plot_dat : data frame of time and infected counts for each run

  # Throw some errors 
  if (is.null(grp) == TRUE) {
    stop("No infection group specified!")
  }
  if (is.null(occ) == TRUE) {
    stop("Number of insertions not specified!")
  }
  if (is.null(insertion) == TRUE || insertion < 0) {
    stop("Something is wrong with your start time, partner.")
  }
  
  # Set up PSOCK Cluster
  core <- detectCores(logical = FALSE)
  cl <- makePSOCKcluster(core)
  registerDoParallel(cl)
  
  # Assign args to globalEnv for mul_ins to acess
  batch <<- batch 
  fun_list <<- fun_list
  grp <<- grp
  insertion <<- insertion
  i_number <<- i_number
  occ <<- occ
  
  clusterExport(cl, c("mul_ins", "batch", "fun_list",
                      "grp", "insertion", "i_number", "occ"))


  par_run <- function() {
    results <- mul_ins()
    results <- cbind(results, I = rowSums(results[, c("I1", "I2")]))
    results <- cbind(results, iter = i)
    results <- results[, c("time", "I", "iter"), drop = FALSE]
    return(results)
  }
  
  # batch runs
  plot_dat <- data.frame(time = NULL, I = NULL, iter = NULL)
  plot_dat <- foreach(i = 1:batch, .packages = "adaptivetau", 
                      .combine = rbind) %dopar% {
      par_run()

    }

  # Return Plot Data for analysis and plotting 
  plot_dat <- as.data.frame(plot_dat)
  return(plot_dat)

  # Stop PSOCK cluster
  stopCluster(cl) 
  registerDoSEQ()
  
  # Garbage Collection
  rm(cl)
  rm(batch, envir = .GlobalEnv)
  rm(fun_list, envir = .GlobalEnv)
  rm(grp, envir = .GlobalEnv)
  rm(insertion, envir = .GlobalEnv)
  rm(i_number, envir = .GlobalEnv)
  rm(occ, envir = .GlobalEnv)
}

mul_ins <- function() {
  # - batch_run_mc subroutine interfacing with ssa.apaptivetau
  # - Allows for single or multiple insertions of infected persons
  #   at any time point. 
  # - If multiple insertions are called, it will space them out equally
  #   over the specified run length. 
  # - Appends ssa run matrices together, and adjusts for time offset.
  # Args:
  #   None, inherits from batch_run_mc
  # Returns:
  #   results : matrix of conglomerated run data

  # __inherits__
  init <- fun_list[[1]]
  t <- fun_list[[2]]
  RF <- fun_list[[3]]
  P <- fun_list[[4]]
  ins <- occ
  i_num <- i_number
  i_start <- insertion
  age <- grp
  tf <- fun_list[[5]]
  inf_grp <- ifelse(age == "a", "I2", "I1")

  # Check for first insertion
  if (i_start == 0) {
    t_start <- tf * (1 / ins)
    init[inf_grp] <- init[inf_grp] + i_num
    ins <- ins - 1
  } else {
    t_start <- i_start
  }
  
  # First run 
  results <- ssa.adaptivetau(init, t, RF, P, t_start, tl.params = list(maxtau = 360))
  
  # Subsequent runs
  for (i in 1:ins) {
    # Add infected
    results[nrow(results), inf_grp] <- results[nrow(results), inf_grp] +
      i_num

    # Set results from first run as init for next run

    init_new <- results[nrow(results), ]
    init_new <- init_new[c("S1", "S2", "E1", "E2", "I1",
                           "I2", "R1", "R2", "D"), drop = FALSE]

    # Calculate new run length
    t_new <- (tf - i_start) * (i / ins) - (tf - i_start) * ((i - 1) / ins)

    # Run with new inits
    run <- ssa.adaptivetau(init_new, t, RF, P, t_new, tl.params = list(maxtau = 360))

    # Offset time by the final time of the past run
    run <- cbind(apply(run[, "time", drop = FALSE], 2, function(x) x +
      results[nrow(results), "time"]), run[, -1])

    # Drop duplicated first row
    results <- rbind(results, run[-1, ])
  }
  return(results)
}
